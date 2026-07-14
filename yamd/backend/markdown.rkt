#lang typed/racket/base
;; Markdown (CommonMark / GFM) backend: serializes fully-resolved IR to a
;; CommonMark document string WITHOUT a trailing newline (the golden runner
;; adds one). Mirrors backend/html.rkt: refuses delayed nodes, raw nodes
;; addressed to other backends, and internal `%`-tags — no silent corruption.
;;
;; ---------------------------------------------------------------------------
;; MAPPING DECISIONS (read before extending)
;;
;; Two families of tags:
;;
;;  * NATIVELY MAPPED — rendered with a CommonMark construct. Presentation-only
;;    attributes that CommonMark cannot express (`id`, `class`, `type`, …) are
;;    DROPPED (they are not document *content*; the visible text and structural
;;    role are preserved). Covered: p, h1–h6, em, strong, code, pre+code
;;    (fenced; tok spans stripped to plain code text), a (href/title kept),
;;    img (src/alt/title kept), ul, ol (rendered as a decimal list; a non-"1"
;;    `type` degrades to decimal numbering), li, blockquote, hr, table
;;    (GFM pipe table), and the transparent containers section / nav (unwrapped
;;    into their child blocks).
;;
;;    ANCHOR TARGETS. CommonMark has no attribute syntax, so a native heading /
;;    link / list item cannot carry an `id`. To keep intra- AND cross-document
;;    fragment links (`#sec-N`, `page.md#def-x`, footnote/glossary anchors — the
;;    multidoc builder rewrites the cross-page ones) resolvable, an id-bearing
;;    native element emits a leading inline-HTML anchor `<a id="x"></a>`:
;;    headings inline (`# <a id="sec-1"></a>Title`), links prefixed, list items
;;    after the marker, and any other native block on its own leading line.
;;    Inline HTML is the right vehicle because (a) the builder's rewrite pass
;;    emits raw fragment ids (`#sec-2`), not text-derived slugs, so GitHub-style
;;    auto heading anchors would not match; (b) `{#id}` heading attributes are a
;;    pandoc/kramdown-only extension that ONLY works on headings, whereas ids
;;    also land on `a`/`li`/`sup`; (c) raw inline HTML is already this backend's
;;    fallback idiom and resolves in any CommonMark processor that permits it.
;;    Elements rendered via the HTML fallback (dfn, sup, …) already carry their
;;    id in the emitted tag, so they get NO extra anchor (it would duplicate).
;;
;;  * HTML FALLBACK — tags with no CommonMark construct at all are serialized
;;    as embedded HTML (CommonMark permits inline and block raw HTML), which
;;    preserves the tag, ALL its attributes, and its content (the "never drop
;;    content" guarantee). Covered: dfn, sup, span, dl/dt/dd, MathML
;;    (math/mrow/mi/…), and any unknown tag.
;;      - INLINE fallback (phrasing tags: dfn, sup, span, math, …): emit
;;        `<tag attrs>` + children rendered in MARKDOWN INLINE context +
;;        `</tag>`. CommonMark does not suppress markdown between inline HTML
;;        tags, and backslash escapes survive, so nested links/emphasis still
;;        work and metacharacters in leaf text stay literal.
;;      - BLOCK fallback (flow tags: dl, div, figure, …): emit the element as a
;;        literal HTML block (text HTML-escaped, children serialized as HTML —
;;        NOT markdown, because CommonMark does not parse markdown inside an
;;        HTML block), surrounded by blank lines.
;;
;; TABLES: GFM pipe tables. A table with a `<thead>` uses its (single) header
;; row; a header-less table gets a synthesized EMPTY header row (GFM requires
;; one). Column count is the max cell count across rows; short rows are padded.
;; A table is rendered as HTML fallback instead when it cannot be a pipe table:
;; any cell containing block content, or more than one header row.
;;
;; RAW nodes:
;;  * backend 'markdown — pass through verbatim (the output IS markdown); a
;;    block raw is emitted as a standalone block, an inline raw joins the flow.
;;  * backend 'html    — ALSO passed through verbatim. Reasoned yes: CommonMark
;;    explicitly permits embedded inline and block HTML, so 'html raws are legal
;;    markdown content; refusing them would needlessly reject portable output.
;;  * any other backend — an error, exactly like backend/html.rkt.
;;
;; ESCAPING: text leaves are escaped context-appropriately (see escape-inline),
;; deliberately avoiding over-escaping prose (intraword `_`, bare `&`, `#`/`>`
;; away from line starts are left alone). Block-level triggers (`#`, `>`,
;; bullet/ordered markers, setext/thematic `=`/`-` lines) are neutralized per
;; physical line by guard-line-start, applied only to paragraph text.

(require racket/string
         racket/list
         "../ir.rkt"
         (only-in "../decode.rkt" block-tag?))

(provide content->markdown
         escape-inline
         guard-line-start
         current-markdown-styled-ref)

;; A rendered block plus its kind (kind drives spacing inside list items).
(struct md-block ([kind : Symbol] [text : String]) #:transparent)

;; How an inline `br` (hard line break) renders. Default is a CommonMark hard
;; break — a backslash followed by a newline. In contexts where a literal
;; newline is DESTRUCTIVE (a pipe-table cell, whose newlines break the row; a
;; heading, whose newlines are collapsed to spaces), this is rebound to inline
;; `<br />` so the break survives the later newline->space collapse. CommonMark
;; permits raw inline HTML, so `<br />` is a portable fallback.
(: current-linebreak (Parameterof String))
(define current-linebreak (make-parameter "\\\n"))

;; ---------------------------------------------------------------------------
;; Entry point

(: content->markdown (-> Content String))
(define (content->markdown c)
  (define blocks (render-blocks (if (list? c) c (list c))))
  (string-join (map md-block-text blocks) "\n\n"))

;; ---------------------------------------------------------------------------
;; Splice flattening

(: splice-flatten (-> (Listof Content) (Listof Content)))
(define (splice-flatten items)
  (for/fold ([acc : (Listof Content) '()] #:result (reverse acc))
            ([it (in-list items)])
    (if (list? it)
        (append (reverse (splice-flatten it)) acc)
        (cons it acc))))

;; ---------------------------------------------------------------------------
;; Names / internal-tag guards (mirrors backend/html.rkt)

(define name-rx #rx"^[a-zA-Z][a-zA-Z0-9-]*$")

(: internal-tag? (-> Symbol Boolean))
(define (internal-tag? t)
  (regexp-match? #rx"^%" (symbol->string t)))

(: reject-internal (-> Symbol (U srcloc False) Void))
(define (reject-internal t loc)
  (when (internal-tag? t)
    (error 'content->markdown "internal element ~a leaked into backend (at ~a)" t loc)))

(: check-name (-> Symbol String (U srcloc False) Void))
(define (check-name sym what loc)
  (unless (regexp-match? name-rx (symbol->string sym))
    (error 'content->markdown "invalid ~a name ~e (at ~a)" what sym loc)))

;; ---------------------------------------------------------------------------
;; Block flow

;; Renders a list of Content as a sequence of blocks. Inline runs are grouped
;; into paragraphs (split at %para-break markers and block elements), exactly
;; like decode's flow grouping. Empty paragraphs are dropped.
(: render-blocks (-> (Listof Content) (Listof md-block)))
(define (render-blocks items)
  (define out : (Listof md-block) '())   ; reversed
  (define acc : (Listof Content) '())    ; reversed inline accumulator
  (: flush! (-> Void))
  (define (flush!)
    (unless (null? acc)
      (define s (string-trim (render-inline (reverse acc))))
      (set! acc '())
      (unless (string=? s "")
        (set! out (cons (md-block 'para (guard-lines s)) out)))))
  ;; Input is always decoded (decode-flow guarantees no %para-break survives),
  ;; so a %para-break element here is an internal leak — it falls through to
  ;; the inline path and errors via reject-internal, exactly like backend/html.
  (for ([it (in-list (splice-flatten items))])
    (cond
      [(and (element? it) (memq (element-tag it) '(section nav)))
       ;; Transparent containers: unwrap into their child blocks. A container
       ;; carrying its own id still contributes an anchor target (the id would
       ;; otherwise vanish with the wrapper).
       (reject-internal (element-tag it) (element-loc it))
       (flush!)
       (let ([id (attr-ref it 'id)])
         (when id (set! out (cons (md-block 'raw (id-anchor id)) out))))
       (for ([b (in-list (render-blocks (element-children it)))])
         (set! out (cons b out)))]
      [(and (element? it) (block-tag? (element-tag it)))
       (flush!)
       (define b (render-block it))
       (unless (string=? (md-block-text b) "")
         (set! out (cons b out)))]
      [(and (raw? it) (raw-block? it))
       (flush!)
       (case (raw-backend it)
         [(markdown html) (set! out (cons (md-block 'raw (raw-text it)) out))]
         [else (error 'content->markdown
                      "raw node for backend ~a cannot be rendered to markdown"
                      (raw-backend it))])]
      [(delayed? it)
       (error 'content->markdown "unresolved delayed node; run resolve-content first")]
      [else (set! acc (cons it acc))]))
  (flush!)
  (reverse out))

;; Native block tags whose CommonMark construct cannot carry an `id`; when one
;; does, a leading inline-HTML anchor line precedes it. (headings carry theirs
;; inline via render-heading; the html-fallback `else` keeps its id in the tag.)
(define native-block-tags : (Listof Symbol)
  '(p ul ol blockquote pre hr table))

(: render-block (-> element md-block))
(define (render-block e)
  (define tag (element-tag e))
  (reject-internal tag (element-loc e))
  (define b
    (case tag
      [(h1 h2 h3 h4 h5 h6) (md-block 'heading (render-heading e tag))]
      [(p) (md-block 'para (render-paragraph (element-children e)))]
      [(ul) (md-block 'list (render-list e #f))]
      [(ol) (md-block 'list (render-list e #t))]
      [(blockquote) (md-block 'quote (render-quote e))]
      [(pre) (render-pre e)]
      [(hr) (md-block 'hr "---")]
      [(table) (render-table e)]
      [else (md-block 'block-html (html-fallback e))]))
  (define id (attr-ref e 'id))
  ;; Prepend a standalone anchor line ONLY when the block rendered as a native
  ;; construct: a dynamic HTML fallback (a pre without code, a non-pipe table)
  ;; already serialized the id in its own tag, so anchoring again would DUPLICATE
  ;; it. A blank line after the anchor keeps `hr`/`table` intact (an anchor line
  ;; directly above `---` would make it a setext underline).
  (if (and id (memq tag native-block-tags) (not (eq? (md-block-kind b) 'block-html)))
      (md-block (md-block-kind b)
                (string-append (id-anchor id) "\n\n" (md-block-text b)))
      b))

;; ---------------------------------------------------------------------------
;; Paragraphs, headings

(: render-paragraph (-> (Listof Content) String))
(define (render-paragraph items)
  (guard-lines (string-trim (render-inline items))))

(: render-heading (-> element Symbol String))
(define (render-heading e tag)
  (define level
    (case tag [(h1) 1] [(h2) 2] [(h3) 3] [(h4) 4] [(h5) 5] [(h6) 6] [else 1]))
  (define title
    (string-trim
     (string-replace
      (parameterize ([current-linebreak "<br />"])
        (render-inline (element-children e)))
      "\n" " ")))
  (string-append (make-string level #\#) " " (id-anchor (attr-ref e 'id)) title))

;; Inline-HTML anchor target for an id a native CommonMark construct cannot
;; carry; "" when the element has no id. See the ANCHOR TARGETS note above.
(: id-anchor (-> (U String False) String))
(define (id-anchor id)
  (if id (string-append "<a id=\"" (attr-escape id) "\"></a>") ""))

;; ---------------------------------------------------------------------------
;; Lists

(: render-list (-> element Boolean String))
(define (render-list e ordered?)
  (define lis (children-with-tags e '(li)))
  (string-join
   (for/list : (Listof String) ([li (in-list lis)] [idx (in-naturals 1)])
     (define marker (if ordered? (string-append (number->string idx) ". ") "- "))
     (render-li li marker))
   "\n"))

(: render-li (-> element String String))
(define (render-li li marker)
  (define anchor (id-anchor (attr-ref li 'id)))
  (define blocks (render-blocks (element-children li)))
  (define indent (make-string (string-length marker) #\space))
  (cond
    [(null? blocks) (string-trim (string-append marker anchor) #:left? #f)]
    [else
     (define first-lines (lines-of (md-block-text (car blocks))))
     (define head
       (string-join
        (cons (string-append marker anchor (car first-lines))
              (for/list : (Listof String) ([l (in-list (cdr first-lines))])
                (indent-line l indent)))
        "\n"))
     (define rest
       (apply string-append
              (for/list : (Listof String) ([b (in-list (cdr blocks))])
                (define sep (if (eq? (md-block-kind b) 'list) "\n" "\n\n"))
                (string-append sep (indent-block (md-block-text b) indent)))))
     (string-append head rest)]))

;; ---------------------------------------------------------------------------
;; Blockquote

(: render-quote (-> element String))
(define (render-quote e)
  (define blocks (render-blocks (element-children e)))
  (define inner (string-join (map md-block-text blocks) "\n\n"))
  (string-join
   (for/list : (Listof String) ([l (in-list (lines-of inner))])
     (if (string=? l "") ">" (string-append "> " l)))
   "\n"))

;; ---------------------------------------------------------------------------
;; Code blocks

(: render-pre (-> element md-block))
(define (render-pre e)
  (define codeel : (U element False)
    (for/or : (U element False)
        ([ch (in-list (splice-flatten (element-children e)))])
      (and (element? ch) (eq? (element-tag ch) 'code) ch)))
  (cond
    [codeel
     (define lang (code-lang codeel))
     (define text (strip-one-trailing-newline (collect-text codeel)))
     (define fence (make-string (max 3 (add1 (longest-backtick-run text))) #\`))
     (md-block 'code (string-append fence lang "\n" text "\n" fence))]
    [else (md-block 'block-html (html-fallback e))]))

(: code-lang (-> element String))
(define (code-lang codeel)
  (define cls (attr-ref codeel 'class))
  (cond
    [(and cls (regexp-match #px"language-([^\\s]+)" cls))
     => (lambda (m)
          (define l (cadr m))
          (if (string? l) l ""))]
    [else ""]))

;; ---------------------------------------------------------------------------
;; Tables (GFM pipe tables, or HTML fallback)

(: render-table (-> element md-block))
(define (render-table e)
  (define head-rows
    (append-map (lambda ([t : element]) (children-with-tags t '(tr)))
                (children-with-tags e '(thead))))
  (define body-rows
    (append (children-with-tags e '(tr))
            (append-map (lambda ([b : element]) (children-with-tags b '(tr)))
                        (children-with-tags e '(tbody)))))
  (define all-rows (append head-rows body-rows))
  (cond
    ;; Not representable as a pipe table -> faithful HTML fallback.
    [(null? all-rows) (md-block 'block-html (html-fallback e))]
    [(> (length head-rows) 1) (md-block 'block-html (html-fallback e))]
    [(ormap row-has-block? all-rows) (md-block 'block-html (html-fallback e))]
    [else
     (define ncol
       (apply max 1 (map (lambda ([r : element]) (length (cells-of r))) all-rows)))
     (define hdr-cells (if (null? head-rows) '() (cells-of (car head-rows))))
     (define hdr-line (pipe-row (pad-to (map render-cell hdr-cells) ncol)))
     (define delim
       (string-append "|" (apply string-append
                                 (for/list : (Listof String) ([_ (in-range ncol)])
                                   " --- |"))))
     ;; With no <thead>, every row is a data row under the synthesized header.
     (define data-rows (if (null? head-rows) all-rows body-rows))
     (define body-lines
       (for/list : (Listof String) ([r (in-list data-rows)])
         (pipe-row (pad-to (map render-cell (cells-of r)) ncol))))
     (md-block 'table (string-join (list* hdr-line delim body-lines) "\n"))]))

(: cells-of (-> element (Listof element)))
(define (cells-of tr) (children-with-tags tr '(th td)))

(: render-cell (-> element String))
(define (render-cell cell)
  (string-replace
   (string-replace
    (string-trim
     (parameterize ([current-linebreak "<br />"])
       (render-inline (element-children cell))))
    "\n" " ")
   "|" "\\|"))

(: pipe-row (-> (Listof String) String))
(define (pipe-row cells)
  (string-append "| " (string-join cells " | ") " |"))

(: pad-to (-> (Listof String) Integer (Listof String)))
(define (pad-to cells n)
  (append cells (make-list (max 0 (- n (length cells))) "")))

(: row-has-block? (-> element Boolean))
(define (row-has-block? r)
  (ormap (lambda ([c : element]) (ormap content-has-block? (element-children c)))
         (cells-of r)))

(: content-has-block? (-> Content Boolean))
(define (content-has-block? c)
  (cond
    [(element? c) (or (block-tag? (element-tag c))
                      (ormap content-has-block? (element-children c)))]
    [(raw? c) (raw-block? c)]
    [(list? c) (ormap content-has-block? c)]
    [else #f]))

;; ---------------------------------------------------------------------------
;; Inline rendering

(: render-inline (-> (Listof Content) String))
(define (render-inline items)
  ;; A CommonMark hard break is a backslash immediately before the line-ending
  ;; newline. When `#linebreak` sits at the END of a source line the reader also
  ;; emits that line's own "\n" text node right after the `br`, so a naive concat
  ;; yields `\` + newline + newline — a BLANK line, which CommonMark reads as a
  ;; paragraph break (dropping the stray `\`). The two breaks are one intent
  ;; (explicit hard + implicit line-join), so we drop the redundant soft newline
  ;; that immediately follows a newline-terminated hard break. Only the default
  ;; `\`+newline form triggers this; the `<br />` fallback (cells/headings, no
  ;; trailing newline) never does, and non-`br` rendering is untouched.
  (let loop ([xs : (Listof Content) (splice-flatten items)]
             [strip-lead-nl? : Boolean #f]
             [acc : (Listof String) '()])
    (cond
      [(null? xs) (apply string-append (reverse acc))]
      [else
       (define c (car xs))
       (define rendered (render-inline-1 c))
       (define piece
         (if (and strip-lead-nl? (string? c)
                  (> (string-length rendered) 0)
                  (char=? (string-ref rendered 0) #\newline))
             (substring rendered 1)
             rendered))
       (define next-strip?
         (and (element? c) (eq? (element-tag c) 'br)
              (let ([lb (current-linebreak)])
                (and (> (string-length lb) 0)
                     (char=? (string-ref lb (sub1 (string-length lb))) #\newline)))))
       (loop (cdr xs) next-strip? (cons piece acc))])))

(: render-inline-1 (-> Content String))
(define (render-inline-1 c)
  (cond
    [(string? c) (escape-inline c)]
    [(element? c) (render-inline-element c)]
    [(raw? c)
     (case (raw-backend c)
       [(markdown html) (raw-text c)]
       [else (error 'content->markdown
                    "raw node for backend ~a cannot be rendered to markdown"
                    (raw-backend c))])]
    [(delayed? c)
     (error 'content->markdown "unresolved delayed node; run resolve-content first")]
    [else (render-inline c)]))

(: render-inline-element (-> element String))
(define (render-inline-element e)
  (define tag (element-tag e))
  (reject-internal tag (element-loc e))
  (case tag
    [(em) (wrap-emph "*" e)]
    [(strong) (wrap-emph "**" e)]
    [(code) (inline-code (string-replace (collect-text e) "\n" " "))]
    [(br) (current-linebreak)]
    [(a) (render-link e)]
    [(img) (render-img e)]
    [else
     (if (block-tag? tag)
         ;; A block element sitting in inline position: best-effort — render
         ;; its children inline (block structure can't survive here).
         (render-inline (element-children e))
         (render-inline-html e))]))

;; Emphasis / strong. CommonMark forbids delimiters flanking whitespace, so
;; pull any leading/trailing whitespace outside the delimiters.
(: wrap-emph (-> String element String))
(define (wrap-emph delim e)
  (define inner (render-inline (element-children e)))
  (define-values (lead core trail) (split-edges-ws inner))
  (if (string=? core "")
      inner
      (string-append lead delim core delim trail)))

(: render-link (-> element String))
(define (render-link e)
  (define href (attr-ref e 'href))
  (cond
    [(not href) (render-inline-html e)]
    ;; A styled cross-reference (class carries "xref-ref"): CommonMark cannot
    ;; carry the class, so route the whole link through the host-configurable
    ;; hook. Default = italic (see current-markdown-styled-ref).
    [(styled-ref? e) ((current-markdown-styled-ref) e (plain-link e href))]
    [else (plain-link e href)]))

;; The ordinary `[text](dest)` rendering (with any id anchor), shared by plain
;; links and the default styled-ref hook.
(: plain-link (-> element String String))
(define (plain-link e href)
  (string-append (id-anchor (attr-ref e 'id))
                 "[" (render-inline (element-children e)) "]"
                 "(" (format-dest href) (format-title (attr-ref e 'title)) ")"))

;; True when `e` carries the "xref-ref" class token (a #ref cross-reference).
(: styled-ref? (-> element Boolean))
(define (styled-ref? e)
  (define cls (attr-ref e 'class))
  (and cls (and (member "xref-ref" (string-split cls " ")) #t)))

;; How a #ref renders in markdown. Receives the ref element (its `class`/`href`
;; are the styling payload) and the ALREADY-RENDERED link. Default italicises
;; it (markdown has no class hook, and a shared stylesheet cannot reach plain
;; markdown); override with host code to emit raw `<a class="xref-ref …">` HTML,
;; branch on the scope classes, etc. See docs — this is the markdown arm of the
;; cross-backend styled-element hook.
(: current-markdown-styled-ref (Parameterof (-> element String String)))
(define current-markdown-styled-ref
  (make-parameter (lambda ([e : element] [rendered : String]) (wrap-italic rendered))))

;; Emphasis wrapper that respects CommonMark's no-whitespace-flanking rule.
(: wrap-italic (-> String String))
(define (wrap-italic s)
  (define-values (lead core trail) (split-edges-ws s))
  (if (string=? core "") s (string-append lead "*" core "*" trail)))

(: render-img (-> element String))
(define (render-img e)
  (define src (attr-ref e 'src))
  (cond
    [src
     (string-append "![" (escape-inline (or (attr-ref e 'alt) "")) "]"
                    "(" (format-dest src) (format-title (attr-ref e 'title)) ")")]
    [else (render-inline-html e)]))

;; Inline HTML fallback: tag wrapper + markdown-rendered children.
(: render-inline-html (-> element String))
(define (render-inline-html e)
  (define tag (element-tag e))
  (check-name tag "tag" (element-loc e))
  (string-append "<" (symbol->string tag) (attrs->string (element-attrs e) (element-loc e)) ">"
                 (render-inline (element-children e))
                 "</" (symbol->string tag) ">"))

;; ---------------------------------------------------------------------------
;; HTML block fallback (a literal HTML region — text HTML-escaped, children
;; serialized as HTML, never markdown). Mirrors backend/html.rkt but its raw
;; policy passes BOTH 'html and 'markdown raws through (both are legal inside
;; a markdown document), erroring only on foreign backends.

(define void-tags : (Listof Symbol)
  '(area base br col embed hr img input link meta source track wbr))

(: html-fallback (-> Content String))
(define (html-fallback c)
  (define out (open-output-string))
  (write-html c out)
  (get-output-string out))

(: write-html (-> Content Output-Port Void))
(define (write-html c out)
  (cond
    [(string? c) (write-string (html-escape c) out) (void)]
    [(element? c) (write-html-element c out)]
    [(raw? c)
     (case (raw-backend c)
       [(html markdown) (write-string (raw-text c) out) (void)]
       [else (error 'content->markdown
                    "raw node for backend ~a cannot be rendered to markdown"
                    (raw-backend c))])]
    [(delayed? c)
     (error 'content->markdown "unresolved delayed node; run resolve-content first")]
    [else (for ([ch (in-list c)]) (write-html ch out))]))

(: write-html-element (-> element Output-Port Void))
(define (write-html-element e out)
  (define tag (element-tag e))
  (reject-internal tag (element-loc e))
  (check-name tag "tag" (element-loc e))
  (write-string "<" out)
  (write-string (symbol->string tag) out)
  (write-string (attrs->string (element-attrs e) (element-loc e)) out)
  (cond
    [(memq tag void-tags)
     (unless (null? (element-children e))
       (error 'content->markdown "void element ~a has children" tag))
     (write-string " />" out)]
    [else
     (write-string ">" out)
     (for ([ch (in-list (element-children e))]) (write-html ch out))
     (write-string "</" out)
     (write-string (symbol->string tag) out)
     (write-string ">" out)])
  (void))

(: attrs->string (-> Attrs (U srcloc False) String))
(define (attrs->string attrs loc)
  (apply string-append
         (for/list : (Listof String) ([a (in-list attrs)])
           (check-name (car a) "attribute" loc)
           (string-append " " (symbol->string (car a))
                          "=\"" (attr-escape (cdr a)) "\""))))

(: html-escape (-> String String))
(define (html-escape s)
  (string-replace
   (string-replace (string-replace s "&" "&amp;") "<" "&lt;")
   ">" "&gt;"))

(: attr-escape (-> String String))
(define (attr-escape s)
  (string-replace (html-escape s) "\"" "&quot;"))

;; ---------------------------------------------------------------------------
;; Escaping helpers

;; Escapes markdown metacharacters in a text leaf, context-appropriately.
;; Always: \ ` * [ ] < . Contextual: `_` only at word boundaries (intraword
;; underscore is literal in CommonMark), `&` only when it could begin an entity.
(: escape-inline (-> String String))
(define (escape-inline s)
  (define n (string-length s))
  (define out (open-output-string))
  (for ([i (in-range n)])
    (define ch (string-ref s i))
    (define esc?
      (cond
        [(memv ch '(#\\ #\` #\* #\[ #\] #\<)) #t]
        [(char=? ch #\_)
         (not (and (alnum-at? s (sub1 i)) (alnum-at? s (add1 i))))]
        [(char=? ch #\&)
         (define j (add1 i))
         (and (< j n)
              (let ([nx (string-ref s j)])
                (or (char-alphabetic? nx) (char-numeric? nx) (char=? nx #\#))))]
        [else #f]))
    (when esc? (write-char #\\ out))
    (write-char ch out))
  (get-output-string out))

(: alnum-at? (-> String Integer Boolean))
(define (alnum-at? s i)
  (and (>= i 0) (< i (string-length s))
       (let ([ch (string-ref s i)])
         (or (char-alphabetic? ch) (char-numeric? ch)))))

;; Applies guard-line-start to every physical line of a paragraph string.
(: guard-lines (-> String String))
(define (guard-lines s)
  (string-join (map guard-line-start (lines-of s)) "\n"))

;; Neutralizes a leading block-level construct on a single line by prefixing a
;; backslash (or, for ordered-list markers, escaping the delimiter). Only
;; triggers that would change block structure are handled — inline emphasis /
;; link syntax generated elsewhere is left intact.
(: guard-line-start (-> String String))
(define (guard-line-start line)
  (cond
    [(regexp-match? #px"^#{1,6}([ \t]|$)" line) (string-append "\\" line)]
    [(regexp-match? #px"^>" line) (string-append "\\" line)]
    [(regexp-match? #px"^[-+]([ \t]|$)" line) (string-append "\\" line)]
    [(regexp-match? #px"^=+[ \t]*$" line) (string-append "\\" line)]
    [(regexp-match? #px"^-+[ \t]*$" line) (string-append "\\" line)]
    [else (guard-ordered-marker line)]))

(: guard-ordered-marker (-> String String))
(define (guard-ordered-marker line)
  (define m (regexp-match #px"^([0-9]+)([.)])(.*)$" line))
  (cond
    [(and m (= (length m) 4))
     (define digits (list-ref m 1))
     (define punct (list-ref m 2))
     (define rest (list-ref m 3))
     (if (and (string? digits) (string? punct) (string? rest)
              (regexp-match? #px"^([ \t]|$)" rest))
         (string-append digits "\\" punct rest)
         line)]
    [else line]))

;; ---------------------------------------------------------------------------
;; Inline code

(: inline-code (-> String String))
(define (inline-code text0)
  (define text (string-replace text0 "\n" " "))
  (define fence (make-string (add1 (longest-backtick-run text)) #\`))
  (define len (string-length text))
  (define pad
    (if (and (> len 0)
             (or (char=? (string-ref text 0) #\`)
                 (char=? (string-ref text (sub1 len)) #\`)))
        " " ""))
  (string-append fence pad text pad fence))

(: longest-backtick-run (-> String Integer))
(define (longest-backtick-run s)
  (define best 0)
  (define cur 0)
  (for ([ch (in-string s)])
    (cond
      [(char=? ch #\`) (set! cur (add1 cur)) (when (> cur best) (set! best cur))]
      [else (set! cur 0)]))
  best)

;; ---------------------------------------------------------------------------
;; Link/image helpers

(: format-dest (-> String String))
(define (format-dest url)
  (if (regexp-match? #px"^[^\\s()<>]*$" url)
      url
      (string-append
       "<"
       (string-replace (string-replace (string-replace url "\\" "\\\\") "<" "\\<") ">" "\\>")
       ">")))

(: format-title (-> (U String False) String))
(define (format-title t)
  (if t
      (string-append " \"" (string-replace (string-replace t "\\" "\\\\") "\"" "\\\"") "\"")
      ""))

;; ---------------------------------------------------------------------------
;; Small utilities

(: attr-ref (-> element Symbol (U String False)))
(define (attr-ref e k)
  (define p (assq k (element-attrs e)))
  (if p (cdr p) #f))

(: children-with-tags (-> element (Listof Symbol) (Listof element)))
(define (children-with-tags e tags)
  (for/fold ([acc : (Listof element) '()] #:result (reverse acc))
            ([ch (in-list (splice-flatten (element-children e)))])
    (if (and (element? ch) (memq (element-tag ch) tags))
        (cons ch acc)
        acc)))

;; Concatenated text of a subtree (strips tok spans etc. from code content).
(: collect-text (-> Content String))
(define (collect-text c)
  (cond
    [(string? c) c]
    [(element? c) (apply string-append (map collect-text (element-children c)))]
    [(raw? c) (raw-text c)]
    [(delayed? c)
     (error 'content->markdown "unresolved delayed node in code content")]
    [else (apply string-append (map collect-text c))]))

;; Splits on newlines, preserving empty lines; "" -> one empty line.
(: lines-of (-> String (Listof String)))
(define (lines-of s)
  (if (string=? s "") (list "") (string-split s "\n" #:trim? #f)))

(: indent-line (-> String String String))
(define (indent-line l indent)
  (if (string=? l "") "" (string-append indent l)))

(: indent-block (-> String String String))
(define (indent-block text indent)
  (string-join
   (for/list : (Listof String) ([l (in-list (lines-of text))]) (indent-line l indent))
   "\n"))

(: strip-one-trailing-newline (-> String String))
(define (strip-one-trailing-newline s)
  (define n (string-length s))
  (if (and (> n 0) (char=? (string-ref s (sub1 n)) #\newline))
      (substring s 0 (sub1 n))
      s))

;; Splits a string into (leading-whitespace core trailing-whitespace).
(: split-edges-ws (-> String (Values String String String)))
(define (split-edges-ws s)
  (define n (string-length s))
  (: scan-lead (-> Integer Integer))
  (define (scan-lead i)
    (if (and (< i n) (char-whitespace? (string-ref s i))) (scan-lead (add1 i)) i))
  (define lead (scan-lead 0))
  (: scan-trail (-> Integer Integer))
  (define (scan-trail i)
    (if (and (> i lead) (char-whitespace? (string-ref s (sub1 i)))) (scan-trail (sub1 i)) i))
  (define trail (scan-trail n))
  (values (substring s 0 lead) (substring s lead trail) (substring s trail n)))

;; ---------------------------------------------------------------------------
;; Tests

(module+ test
  (require typed/rackunit)

  ;; --- escape-inline: metacharacters ---
  (check-equal? (escape-inline "a*b") "a\\*b")
  (check-equal? (escape-inline "code `x`") "code \\`x\\`")
  (check-equal? (escape-inline "see [1] and <tag>") "see \\[1\\] and \\<tag>")
  (check-equal? (escape-inline "back\\slash") "back\\\\slash")
  ;; intraword underscore is NOT escaped; boundary underscore is
  (check-equal? (escape-inline "snake_case") "snake_case")
  (check-equal? (escape-inline "_emph_") "\\_emph\\_")
  ;; ampersand: only entity-like, not standalone
  (check-equal? (escape-inline "A & B") "A & B")
  (check-equal? (escape-inline "&amp;") "\\&amp;")
  ;; # and > away from line start are prose, left alone
  (check-equal? (escape-inline "issue #42 a>b") "issue #42 a>b")

  ;; --- guard-line-start: block triggers ---
  (check-equal? (guard-line-start "# not a heading") "\\# not a heading")
  (check-equal? (guard-line-start "> not a quote") "\\> not a quote")
  (check-equal? (guard-line-start "- not a bullet") "\\- not a bullet")
  (check-equal? (guard-line-start "1. not a list") "1\\. not a list")
  (check-equal? (guard-line-start "3) also not") "3\\) also not")
  (check-equal? (guard-line-start "---") "\\---")
  (check-equal? (guard-line-start "plain text") "plain text")
  (check-equal? (guard-line-start "-5 degrees") "-5 degrees")

  ;; --- plain text / paragraph ---
  (check-equal? (content->markdown "hello world") "hello world")
  (check-equal? (content->markdown (list (element 'p '() (list "a paragraph") #f)))
                "a paragraph")

  ;; --- emphasis / strong / inline code ---
  (check-equal? (content->markdown (element 'em '() (list "emph") #f)) "*emph*")
  (check-equal? (content->markdown (element 'strong '() (list "bold") #f)) "**bold**")
  (check-equal? (content->markdown (element 'code '() (list "x = 1") #f)) "`x = 1`")
  ;; inline code containing a backtick uses a longer fence (no padding needed
  ;; since the content itself does not begin or end with a backtick)
  (check-equal? (content->markdown (element 'code '() (list "a`b") #f)) "``a`b``")
  ;; content that begins/ends with a backtick gets space padding
  (check-equal? (content->markdown (element 'code '() (list "`x") #f)) "`` `x ``")
  ;; emphasis pulls flanking whitespace outside the delimiters (shown embedded,
  ;; since a top-level paragraph would trim the edges anyway)
  (check-equal? (content->markdown
                 (element 'p '() (list "a" (element 'em '() (list " y ") #f) "b") #f))
                "a *y* b")

  ;; --- links / images ---
  (check-equal? (content->markdown
                 (element 'a '((href . "https://x")) (list "text") #f))
                "[text](https://x)")
  ;; a link carrying an id emits a leading inline-HTML anchor target (its id
  ;; cannot live on the markdown link), then the plain markdown link
  (check-equal? (content->markdown
                 (element 'a '((id . "ref-1") (href . "#def-vm")) (list "vm") #f))
                "<a id=\"ref-1\"></a>[vm](#def-vm)")
  ;; url needing angle brackets
  (check-equal? (content->markdown
                 (element 'a '((href . "a b")) (list "t") #f))
                "[t](<a b>)")
  (check-equal? (content->markdown
                 (element 'img '((src . "c.png") (alt . "a cat")) '() #f))
                "![a cat](c.png)")

  ;; --- headings (unwrapped from a section; id becomes an inline anchor) ---
  (check-equal? (content->markdown
                 (element 'section '()
                          (list (element 'h1 '((id . "sec-1")) (list "Title") #f)) #f))
                "# <a id=\"sec-1\"></a>Title")
  ;; a heading with no id is unchanged
  (check-equal? (content->markdown (element 'h2 '() (list "Plain") #f)) "## Plain")

  ;; --- lists ---
  (check-equal? (content->markdown
                 (element 'ul '() (list (element 'li '() (list "a") #f)
                                        (element 'li '() (list "b") #f)) #f))
                "- a\n- b")
  (check-equal? (content->markdown
                 (element 'ol '((type . "1")) (list (element 'li '() (list "a") #f)) #f))
                "1. a")
  ;; nested list under an item
  (check-equal? (content->markdown
                 (element 'ul '()
                          (list (element 'li '()
                                         (list "a"
                                               (element 'ul '()
                                                        (list (element 'li '() (list "a1") #f)) #f)) #f)) #f))
                "- a\n  - a1")

  ;; a list item carrying an id (e.g. a footnote note) anchors after the marker
  (check-equal? (content->markdown
                 (element 'ol '() (list (element 'li '((id . "fn-note-1")) (list "note") #f)) #f))
                "1. <a id=\"fn-note-1\"></a>note")
  ;; a native block container carrying an id gets a leading anchor block (blank
  ;; line after it, so an `hr`/`table` below is not read as a setext underline)
  (check-equal? (content->markdown
                 (element 'ul '((id . "l-1")) (list (element 'li '() (list "a") #f)) #f))
                "<a id=\"l-1\"></a>\n\n- a")
  ;; an hr with an id keeps the thematic break (anchor on its own paragraph)
  (check-equal? (content->markdown (element 'hr '((id . "h-1")) '() #f))
                "<a id=\"h-1\"></a>\n\n---")
  ;; a pre that falls back to inline HTML keeps its id in the tag — no dup anchor
  (check-equal? (content->markdown (element 'pre '((id . "p-1")) (list "raw") #f))
                "<pre id=\"p-1\">raw</pre>")

  ;; --- blockquote ---
  (check-equal? (content->markdown (element 'blockquote '() (list "quoted") #f))
                "> quoted")

  ;; --- hr ---
  (check-equal? (content->markdown (element 'hr '() '() #f)) "---")

  ;; --- linebreak (br): CommonMark hard break in a paragraph ---
  (check-equal? (content->markdown
                 (element 'p '() (list "a" (element 'br '() '() #f) "b") #f))
                "a\\\nb")
  ;; a `br` at the end of a source line is followed by that line's "\n" text
  ;; node; the redundant soft newline is coalesced so it stays ONE hard break
  ;; (a blank line would drop the break and split the paragraph)
  (check-equal? (content->markdown
                 (element 'p '() (list "a" (element 'br '() '() #f) "\n" "b") #f))
                "a\\\nb")
  ;; in a heading, a literal newline would collapse to a space; falls back to
  ;; inline <br /> so the break survives
  (check-equal? (content->markdown
                 (element 'h1 '() (list "a" (element 'br '() '() #f) "b") #f))
                "# a<br />b")
  ;; in a pipe-table cell, a literal newline would break the row; falls back too
  (check-equal? (content->markdown
                 (element 'table '()
                          (list (element 'tbody '()
                                         (list (element 'tr '()
                                                        (list (element 'td '()
                                                                       (list "a" (element 'br '() '() #f) "b") #f)) #f)) #f)) #f))
                "|  |\n| --- |\n| a<br />b |")

  ;; --- pagebreak (styled div): raw HTML block ---
  (check-equal? (content->markdown
                 (element 'div '((style . "break-after: page")) '() #f))
                "<div style=\"break-after: page\"></div>")

  ;; --- code block, tok spans stripped, lang from class ---
  (check-equal? (content->markdown
                 (element 'pre '()
                          (list (element 'code '((class . "language-python"))
                                         (list (element 'span '((class . "tok-keyword")) (list "def") #f)
                                               " f()") #f)) #f))
                "```python\ndef f()\n```")

  ;; --- pipe table ---
  (check-equal? (content->markdown
                 (element 'table '()
                          (list (element 'thead '()
                                         (list (element 'tr '() (list (element 'th '() (list "Name") #f)
                                                                      (element 'th '() (list "Role") #f)) #f)) #f)
                                (element 'tbody '()
                                         (list (element 'tr '() (list (element 'td '() (list "Kiril") #f)
                                                                      (element 'td '() (list "infra") #f)) #f)) #f)) #f))
                "| Name | Role |\n| --- | --- |\n| Kiril | infra |")
  ;; header-less table gets a synthesized empty header row
  (check-equal? (content->markdown
                 (element 'table '()
                          (list (element 'tbody '()
                                         (list (element 'tr '() (list (element 'td '() (list "x") #f)) #f)) #f)) #f))
                "|  |\n| --- |\n| x |")
  ;; a cell with block content forces HTML fallback for the whole table
  (check-true (regexp-match?
               #rx"^<table>"
               (content->markdown
                (element 'table '()
                         (list (element 'tbody '()
                                        (list (element 'tr '()
                                                       (list (element 'td '() (list (element 'p '() (list "blk") #f)) #f)) #f)) #f)) #f))))

  ;; --- inline HTML fallback for phrasing tags (children stay markdown) ---
  (check-equal? (content->markdown (element 'dfn '((id . "def-vm")) (list "vm") #f))
                "<dfn id=\"def-vm\">vm</dfn>")
  (check-equal? (content->markdown
                 (element 'sup '((id . "fn-ref-1"))
                          (list (element 'a '((href . "#fn-note-1")) (list "1") #f)) #f))
                "<sup id=\"fn-ref-1\">[1](#fn-note-1)</sup>")

  ;; --- block HTML fallback for flow tags (children serialized as HTML) ---
  (check-equal? (content->markdown
                 (element 'dl '((class . "glossary"))
                          (list (element 'dt '() (list "term") #f)
                                (element 'dd '() (list (element 'em '() (list "def") #f)) #f)) #f))
                "<dl class=\"glossary\"><dt>term</dt><dd><em>def</em></dd></dl>")

  ;; --- raw passthrough ---
  (check-equal? (content->markdown (raw 'markdown "**verbatim**")) "**verbatim**")
  (check-equal? (content->markdown (raw 'html "<b>x</b>")) "<b>x</b>")
  (check-equal? (content->markdown (raw 'html "<div>x</div>" #t)) "<div>x</div>")

  ;; --- error paths (mirror backend/html.rkt) ---
  (check-exn #rx"raw node for backend latex"
             (lambda () (content->markdown (raw 'latex "\\x"))))
  (check-exn #rx"unresolved delayed node"
             (lambda () (content->markdown (delayed (lambda (i) "")))))
  (check-exn #rx"internal element %para-break leaked"
             (lambda () (content->markdown (element '%para-break '() '() #f))))
  (check-exn #rx"invalid tag name"
             (lambda () (content->markdown (element '|bad tag| '() '() #f))))

  ;; --- multiple blocks joined by a blank line ---
  (check-equal? (content->markdown
                 (list (element 'p '() (list "one") #f)
                       (element 'p '() (list "two") #f)))
                "one\n\ntwo")

  ;; --- a paragraph whose text begins with a block trigger is guarded ---
  (check-equal? (content->markdown (list (element 'p '() (list "# hi") #f)))
                "\\# hi"))
