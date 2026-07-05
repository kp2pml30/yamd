#lang typed/racket/base
;; Typst backend: serializes fully-resolved IR to Typst markup. Mirrors
;; backend/html.rkt's structure and error conventions. Refuses delayed nodes
;; and raw nodes addressed to other backends, and rejects internal `%`-tags —
;; no silent output corruption, no dropped content.
;;
;; Output: Typst markup WITHOUT a trailing newline (the golden runner adds
;; one). Block-level pieces are separated by a blank line.
;;
;; ===========================================================================
;; MAPPING (stdlib-emitted tags -> Typst)
;;
;;   text            escaped (see escape-typst); Typst-special chars neutralised
;;   p               a paragraph (blank-line separated)
;;   section         its children (heading + content), rendered as a block seq
;;   h1..h6          `=`..`======` headings
;;   em              `_..._`      strong `*...*`
;;   br              #linebreak() (inline hard line break)
;;   div             #pagebreak() — ONLY a `style="break-after: page"` div (the
;;                   stdlib #pagebreak); any other div is an error
;;   code (inline)   inline raw `` `..` `` (or #raw("..") if it holds a backtick)
;;   pre>code        fenced ```lang raw block; tok spans stripped to plain text,
;;                   language taken from `class="language-X"`
;;   a               #link("url")[text]  (href="#anchor" -> #link("#anchor"), a
;;                   plain URL fragment link; see NOTE on anchors below)
;;   img             #image("src", alt: "..")
;;   ul / ol / li    `-` / `+` items, nested via 2-space indentation
;;   blockquote      #quote(block: true)[..]
;;   hr              #line(length: 100%)
;;   table/tr/th/td  #table(columns: N, table.header(..), ..cells..)
;;   sup             #super[..]
;;   dfn             #emph[..]   (the definition term, italicised)
;;   dl / dt / dd    Typst term list: `/ term: description`
;;   nav / span      transparent wrappers — their children are rendered
;;   raw 'typst      passed through verbatim (block vs inline per raw-block?)
;;
;; NOTE on internal anchors. Elements carry HTML-style `id`s and links use
;; `href="#id"` (sections/xref/footnotes; the multidoc builder rewrites the
;; cross-page ones to `page.typ#id`). We surface `id`s as Typst LABELS so
;; fragment links resolve, without ever emitting typst that fails to compile:
;;
;;  * Every id-bearing element is preceded by an invisible labelled anchor
;;    `#metadata(none)<id>` (block elements on their own line, inline elements
;;    in place). metadata is labellable, has a location, produces no output,
;;    and `#link(<id>)` jumps to it — uniform across headings, defs, refs,
;;    footnotes, list items, without the fragility of attaching labels directly
;;    to arbitrary content.
;;  * A same-document fragment link `href="#id"` becomes `#link(<id>)[text]`
;;    IFF `id` is a label emitted ON THIS PAGE (see current-page-labels). Any
;;    other href — a genuine URL, or a cross-file `page.typ#id` — stays a
;;    string `#link("..")[text]`. Guarding on the page-local label set is what
;;    keeps compilation safe: Typst errors on `#link(<x>)` to a missing label,
;;    and hrefs left un-rewritten by the builder (e.g. targeting an id inside a
;;    raw node the IR walk cannot see) have no label, so they take the string
;;    form.
;;  * A label name must be a valid Typst label token; an id that is not (e.g.
;;    one containing whitespace) gets NO label and its links take the string
;;    form — degraded but always compiling.
;;  * Duplicate labels are a Typst compile error. Stdlib ids are unique per page
;;    by construction (section/ref counters, #def dup-check, unique footnote
;;    ids) and the multi-file builder rejects an id shared ACROSS pages; a
;;    hand-authored id duplicated WITHIN one page is not caught here (nor by the
;;    builder's cross-page-only registry) and would fail to compile — detecting
;;    it requires a full anchor registry.
;;
;; LIMITATION (documented): Typst labels are per-document, so a cross-FILE
;; `page.typ#id` link cannot resolve to the other file's label from a Typst
;; compile; it is emitted as a string link naming the right page (the builder's
;; page-relative rewrite), and the target page still carries the `<id>` label —
;; correct if/when Typst gains cross-file fragment linking or via HTML export.
;;
;; NOTE on ordered-list styling. HTML `<ol type=a>` numbering styles are not
;; carried to Typst (every ordered list uses `+`, i.e. arabic). Reproducing the
;; style needs scoped `set enum(numbering: ..)` rules; this is documented
;; styling loss, not content loss.
;;
;; ===========================================================================
;; MATH strategy (MathML `math`/`mrow`/`msub`/.. -> Typst math `$..$`)
;;
;; asciimath emits a small, closed set of MathML nodes; we translate each
;; structurally into Typst math:
;;   math            `$..$` (inline) or `$ .. $` (display, block) per @display
;;   mrow            space-joined children
;;   mi              single char verbatim (italic var / greek); multi-char =
;;                   a function name (sin, cos, ..) emitted verbatim
;;   mn              digits verbatim
;;   mo              operator verbatim (Typst accepts unicode math operators
;;                   like ∑ ∈ → ≤ directly and NEVER errors on a stray symbol);
;;                   math-special ASCII (`_ ^ & # $ \ ` ~ { } "`) is emitted as
;;                   a Typst string `".."` so it renders literally and compiles
;;   mtext           Typst string `".."` (upright)
;;   msub/msup       `base_(sub)` / `base^(sup)`  (parens group invisibly in
;;   msubsup         `base_(sub)^(sup)`            script position)
;;   mfrac           `frac(num, den)`
;;   msqrt/mroot     `sqrt(x)` / `root(index, radicand)`
;; Deliberately emitting literal unicode for operators (rather than guessing
;; Typst symbol NAMES like `sum`) is the safe choice: an unknown symbol name is
;; a compile error, whereas an unknown unicode char in math mode is not. Big
;; operators therefore get SIDE-set scripts, which matches asciimath's own
;; documented `msub/msup` (not `munderover`) simplification. Nothing is ever
;; dropped.

(require racket/string
         racket/list
         racket/set
         "../ir.rkt")

(provide content->typst
         escape-typst)

;; ===========================================================================
;; Anchors / labels (see the NOTE on internal anchors in the header)

;; Ids that are valid Typst labels and are present on the page currently being
;; rendered. Only these get a `<label>` and are eligible for `#link(<label>)`.
(: current-page-labels (Parameterof (Setof String)))
(define current-page-labels (make-parameter ((inst set String))))

;; A conservative subset of Typst-label-legal characters covering every id the
;; stdlib emits (sec-N, def-KEY, ref-N, fn-*). Anything outside it degrades to
;; a string link so compilation never breaks on an exotic id.
(define label-rx #rx"^[A-Za-z_][A-Za-z0-9_.:-]*$")

(: valid-typst-label? (-> String Boolean))
(define (valid-typst-label? s)
  (and (regexp-match? label-rx s) #t))

;; Ids that the renderer WILL anchor with a `<label>`, mirroring exactly which
;; elements reach anchor-prefix (render-block-element / render-inline-element /
;; render-list-item, plus the list-item-nested blocks that item-pieces anchors).
;; This must not over-approximate: a `#link(<id>)` to an id NOT emitted here is
;; a Typst "label does not exist" error. So two element classes contribute no
;; own-label — their content is rendered but the wrapper is destructured by its
;; parent:
;;   * structural wrappers (thead/tbody/tr/th/td, dt/dd) — recurse into content;
;;   * opaque bodies (math, pre) — the element itself IS anchored, but its
;;     descendants are consumed structurally (math nodes, code inside pre), so
;;     stop before them.
;; (delayed nodes are handled by the renderers' own error path — no ids here.)
(: anchor-collect (-> Content (Listof String)))
(define (anchor-collect c)
  (cond
    [(element? c)
     (define tag (element-tag c))
     (define own (assq 'id (element-attrs c)))
     (define own-id : (Listof String) (if own (list (cdr own)) '()))
     (cond
       [(memq tag '(math pre)) own-id]
       [(memq tag '(thead tbody tr th td dt dd))
        (append-map anchor-collect (element-children c))]
       [else (append own-id (append-map anchor-collect (element-children c)))])]
    [(list? c) (append-map anchor-collect c)]
    [else '()]))

;; The label set for a page: anchorable ids that are also valid label tokens.
(: page-labels (-> Content (Setof String)))
(define (page-labels c)
  (for/fold ([s : (Setof String) ((inst set String))])
            ([id (in-list (anchor-collect c))]
             #:when (valid-typst-label? id))
    (set-add s id)))

;; An invisible labelled anchor for an id-bearing element, "" otherwise. `sep`
;; separates it from the element's own rendering ("\n" for block, "" inline).
(: anchor-prefix (-> element String String))
(define (anchor-prefix e sep)
  (define id (assq 'id (element-attrs e)))
  (if (and id (set-member? (current-page-labels) (cdr id)))
      (string-append "#metadata(none)<" (cdr id) ">" sep)
      ""))

;; ===========================================================================
;; Escaping

;; Characters that are always markup-special in Typst text and get a `\`.
(define always-escape : (Listof Char)
  '(#\\ #\` #\$ #\# #\* #\_ #\[ #\] #\< #\> #\@ #\~))

;; Escape Typst text. `\` prefixes always-special chars. Line-leading `=`/`-`/`+`
;; (heading/list markers) and a leading `N.` (enum) are escaped only at the
;; start of a physical line, since they are block-structural there. `//` and
;; `/*` (comment openers, which would swallow content) are escaped anywhere.
(: escape-typst (-> String String))
(define (escape-typst s)
  (define n (string-length s))
  (define out (open-output-string))
  (: nth (-> Integer (U Char False)))
  (define (nth k) (and (< k n) (>= k 0) (string-ref s k)))
  (let loop ([i : Integer 0] [line-start? : Boolean #t])
    (cond
      [(>= i n) (get-output-string out)]
      [else
       (define c (string-ref s i))
       (cond
         [(char=? c #\newline)
          (write-char #\newline out) (loop (add1 i) #t)]
         [(memv c always-escape)
          (write-char #\\ out) (write-char c out) (loop (add1 i) #f)]
         [(and line-start?
               (memv c '(#\= #\- #\+))
               (let ([nc (nth (add1 i))]) (or (not nc) (char-whitespace? nc))))
          (write-char #\\ out) (write-char c out) (loop (add1 i) #f)]
         [(and line-start? (char<=? #\0 c #\9))
          ;; a line-leading run of digits followed by `.` is an enum marker
          (let dloop ([j : Integer i])
            (define d (nth j))
            (cond
              [(and d (char<=? #\0 d #\9)) (write-char d out) (dloop (add1 j))]
              [(eqv? d #\.)
               (write-char #\\ out) (write-char #\. out) (loop (add1 j) #f)]
              [else (loop j #f)]))]
         [(and (char=? c #\/)
               (let ([nc (nth (add1 i))]) (and nc (memv nc '(#\/ #\*)))))
          (write-char #\\ out) (write-char #\/ out) (loop (add1 i) #f)]
         [else (write-char c out) (loop (add1 i) #f)])])))

;; Escape a Typst string literal's contents (for #link/#image args, math text).
(: escape-string (-> String String))
(define (escape-string s)
  (string-replace (string-replace s "\\" "\\\\") "\"" "\\\""))

;; ===========================================================================
;; Public entry

(: content->typst (-> Content String))
(define (content->typst c)
  (parameterize ([current-page-labels (page-labels c)])
    (string-trim (render-blocks (list c)))))

;; ===========================================================================
;; Splice flattening

(: flatten* (-> Content (Listof Content)))
(define (flatten* c)
  (if (list? c) (append-map flatten* c) (list c)))

;; ===========================================================================
;; Block-level flow

(define block-tags : (Listof Symbol)
  '(p section h1 h2 h3 h4 h5 h6 ul ol dl blockquote pre hr table nav div))

(: block-item? (-> Content Boolean))
(define (block-item? c)
  (or (and (element? c) (and (memq (element-tag c) block-tags) #t))
      (and (raw? c) (raw-block? c))))

;; Render a sequence of content at block level, joining pieces with a blank
;; line. Consecutive inline items are grouped into one paragraph.
(: render-blocks (-> (Listof Content) String))
(define (render-blocks items)
  (string-join (render-flow-pieces items) "\n\n"))

(: render-flow-pieces (-> (Listof Content) (Listof String)))
(define (render-flow-pieces items)
  (define flat (append-map flatten* items))
  (let loop ([xs : (Listof Content) flat]
             [buf : (Listof Content) '()]
             [acc : (Listof String) '()])
    (: flush (-> (Listof String) (Listof String)))
    (define (flush a)
      (if (null? buf) a
          (let ([p (string-trim (render-inline (reverse buf)))])
            (if (string=? p "") a (cons p a)))))
    (cond
      [(null? xs) (reverse (flush acc))]
      [(block-item? (car xs))
       (loop (cdr xs) '() (cons (render-block (car xs)) (flush acc)))]
      [else (loop (cdr xs) (cons (car xs) buf) acc)])))

;; Render children that may contain block-level content. When any block is
;; present, render as a blank-line-separated block sequence (legal inside a
;; Typst `[..]` content block); otherwise render inline. Used for `[..]`
;; positions (table cells) that can legitimately carry blocks.
(: render-content (-> (Listof Content) String))
(define (render-content kids)
  (if (ormap block-item? (flatten* kids))
      (render-blocks kids)
      (render-inline kids)))

(: render-block (-> Content String))
(define (render-block c)
  (cond
    [(raw? c) (render-raw c)]
    [(element? c) (render-block-element c)]
    [else (error 'content->typst "render-block: not a block: ~e" c)]))

(: render-block-element (-> element String))
(define (render-block-element e)
  (string-append (anchor-prefix e "\n") (render-block-element* e)))

(: render-block-element* (-> element String))
(define (render-block-element* e)
  (define tag (element-tag e))
  (reject-internal tag (element-loc e))
  (define kids (element-children e))
  (case tag
    [(p) (string-trim (render-inline kids))]
    [(section nav) (render-blocks kids)]
    [(h1 h2 h3 h4 h5 h6)
     (define level (- (char->integer (string-ref (symbol->string tag) 1))
                      (char->integer #\0)))
     (string-append (make-string level #\=) " " (string-trim (render-inline kids)))]
    [(ul) (render-list e "" "-")]
    [(ol) (render-list e "" "+")]
    [(dl) (render-dl e)]
    [(blockquote)
     (string-append "#quote(block: true)[\n" (render-blocks kids) "\n]")]
    [(pre) (render-code-block e)]
    [(hr) "#line(length: 100%)"]
    [(table) (render-table e)]
    [(div)
     ;; The only <div> the stdlib emits is #pagebreak's page break; recognize it
     ;; by its exact style and map it to Typst's #pagebreak(). Any other div is
     ;; unsupported (no generic div in Typst markup) — fail loudly, do not drop.
     (define style (assq 'style (element-attrs e)))
     (cond
       [(and style (string=? (cdr style) "break-after: page")) "#pagebreak()"]
       [else (error 'content->typst "cannot render block element div (at ~a)"
                    (element-loc e))])]
    [else
     (error 'content->typst "cannot render block element ~a (at ~a)"
            tag (element-loc e))]))

;; ===========================================================================
;; Inline

(: render-inline (-> Content String))
(define (render-inline c)
  (cond
    [(string? c) (escape-typst c)]
    [(element? c) (render-inline-element c)]
    [(raw? c) (render-raw c)]
    [(delayed? c)
     (error 'content->typst "unresolved delayed node; run resolve-content first")]
    [else (apply string-append (map render-inline c))]))

(: render-inline-element (-> element String))
(define (render-inline-element e)
  (string-append (anchor-prefix e "") (render-inline-element* e)))

(: render-inline-element* (-> element String))
(define (render-inline-element* e)
  (define tag (element-tag e))
  (reject-internal tag (element-loc e))
  (define kids (element-children e))
  (case tag
    [(em) (string-append "_" (render-inline kids) "_")]
    [(strong) (string-append "*" (render-inline kids) "*")]
    [(br)
     ;; A hard line break. #linebreak() is preferred over the `\ ` markup form:
     ;; render-inline concatenates inline pieces with NO separators, and the
     ;; function call is self-delimiting, whereas `\ ` relies on a trailing space
     ;; that could merge with adjacent escaped text.
     "#linebreak()"]
    [(code) (render-inline-code e)]
    [(a) (render-link e)]
    [(img) (render-img e)]
    [(span) (render-inline kids)]
    [(sup) (string-append "#super[" (render-inline kids) "]")]
    [(dfn) (string-append "#emph[" (render-inline kids) "]")]
    [(math) (render-math e)]
    [else
     (error 'content->typst "cannot render element ~a in inline context (at ~a)"
            tag (element-loc e))]))

(: render-inline-code (-> element String))
(define (render-inline-code e)
  (define text (plain-text (element-children e)))
  (if (regexp-match? #rx"`" text)
      (string-append "#raw(\"" (escape-string text) "\")")
      (string-append "`" text "`")))

(: render-link (-> element String))
(define (render-link e)
  (define href (assq 'href (element-attrs e)))
  (define body (render-inline (element-children e)))
  (cond
    [(not href) body]
    [(same-page-anchor (cdr href))
     => (lambda (id) (string-append "#link(<" id ">)[" body "]"))]
    [else (string-append "#link(\"" (escape-string (cdr href)) "\")[" body "]")]))

;; If `href` is a same-document fragment `#id` whose label was emitted on this
;; page, returns that id (for a `#link(<id>)`); otherwise #f (string link).
(: same-page-anchor (-> String (U String False)))
(define (same-page-anchor href)
  (and (> (string-length href) 0)
       (char=? (string-ref href 0) #\#)
       (let ([id (substring href 1)])
         (and (set-member? (current-page-labels) id) id))))

(: render-img (-> element String))
(define (render-img e)
  (define src (assq 'src (element-attrs e)))
  (define alt (assq 'alt (element-attrs e)))
  (unless src (error 'content->typst "img element has no src attribute"))
  (string-append "#image(\"" (escape-string (cdr src)) "\""
                 (if alt
                     (string-append ", alt: \"" (escape-string (cdr alt)) "\"")
                     "")
                 ")"))

;; ===========================================================================
;; Lists

(: li? (-> Content Boolean))
(define (li? c) (and (element? c) (eq? (element-tag c) 'li)))

(: render-list (-> element String String String))
(define (render-list e indent marker)
  (define lis (filter li? (flatten* (element-children e))))
  (string-join
   (for/list : (Listof String) ([li (in-list lis)])
     (render-list-item (assert li element?) indent marker))
   "\n"))

(: render-list-item (-> element String String String))
(define (render-list-item li indent marker)
  (define sub-indent (string-append indent "  "))
  ;; A labelled anchor for the item's own id (e.g. a footnote note), placed
  ;; right after the marker so `#link(<id>)` lands on the item.
  (define anchor (anchor-prefix li ""))
  ;; Group the item body into kinded pieces (a leading paragraph or inline run
  ;; is a piece too, so it sits on the marker line — NOT pushed to a sub-line).
  (define pieces (item-pieces (flatten* (element-children li))))
  ;; Continuation pieces are placed on indented sub-lines: a nested list
  ;; follows without a blank line; other blocks (paragraphs, quotes, code) are
  ;; blank-line separated.
  (: continuation (-> (Listof (Pairof Symbol String)) String))
  (define (continuation ps)
    (apply string-append
           (for/list : (Listof String) ([p (in-list ps)])
             (define sep (if (eq? (car p) 'list) "\n" "\n\n"))
             (string-append sep (indent-lines (cdr p) sub-indent)))))
  ;; `marker` plus the item's anchor (a space only when the anchor is present,
  ;; to keep the bare-marker output byte-identical for anchor-less items).
  (define marked (if (string=? anchor "") marker (string-append marker " " anchor)))
  (cond
    [(null? pieces) (string-append indent marked)]
    ;; A leading nested list must NOT share the marker line: `- - b` reads in
    ;; Typst as one bullet with literal text `- b` (the sublist is lost). Keep
    ;; the marker line bare and indent every piece as a continuation.
    [(eq? (car (car pieces)) 'list)
     (string-append (string-append indent marked) (continuation pieces))]
    [else
     (define head-lines (string-split (cdr (car pieces)) "\n" #:trim? #f))
     (define head
       (string-join
        (cons (string-append indent marked " " (car head-lines))
              (for/list : (Listof String) ([l (in-list (cdr head-lines))])
                (indent-nonblank-line l sub-indent)))
        "\n"))
     (string-append head (continuation (cdr pieces)))]))

;; Groups a flattened list-item body into (kind . text) pieces, text at column
;; 0. Consecutive inline content becomes a 'para piece; block children keep
;; their structural kind so callers can choose the right separator/indent.
(: item-pieces (-> (Listof Content) (Listof (Pairof Symbol String))))
(define (item-pieces flat)
  (let loop ([xs : (Listof Content) flat]
             [buf : (Listof Content) '()]
             [acc : (Listof (Pairof Symbol String)) '()])
    (: flush (-> (Listof (Pairof Symbol String)) (Listof (Pairof Symbol String))))
    (define (flush a)
      (if (null? buf) a
          (let ([s (string-trim (render-inline (reverse buf)))])
            (if (string=? s "") a (cons (cons 'para s) a)))))
    (cond
      [(null? xs) (reverse (flush acc))]
      [(block-item? (car xs))
       (define b (car xs))
       ;; ul/ol/p nested directly in an item are rendered here, bypassing
       ;; render-{block,inline}-element, so anchor their own id explicitly —
       ;; otherwise a same-page `#link(<id>)` to them would have no label. (Other
       ;; blocks go through render-block -> render-block-element, already
       ;; anchored.)
       (define piece : (Pairof Symbol String)
         (cond
           [(and (element? b) (eq? (element-tag b) 'ul))
            (cons 'list (string-append (anchor-prefix b "\n") (render-list b "" "-")))]
           [(and (element? b) (eq? (element-tag b) 'ol))
            (cons 'list (string-append (anchor-prefix b "\n") (render-list b "" "+")))]
           [(and (element? b) (eq? (element-tag b) 'p))
            (cons 'para (string-append (anchor-prefix b "")
                                       (string-trim (render-inline (element-children b)))))]
           [else (cons 'block (render-block b))]))
       ;; Drop pieces that render to nothing (an empty <p>, an empty nested
       ;; list): they would otherwise yield a car-of-'() crash on the marker
       ;; line or spurious blank lines between real pieces.
       (loop (cdr xs) '()
             (if (string=? (cdr piece) "") (flush acc) (cons piece (flush acc))))]
      [else (loop (cdr xs) (cons (car xs) buf) acc)])))

;; Indent every non-empty line by `prefix`; blank lines stay empty (Typst
;; treats a blank line as a paragraph break inside the item, so it must not be
;; polluted with indentation).
(: indent-lines (-> String String String))
(define (indent-lines s prefix)
  (string-join
   (for/list : (Listof String) ([l (in-list (string-split s "\n" #:trim? #f))])
     (indent-nonblank-line l prefix))
   "\n"))

(: indent-nonblank-line (-> String String String))
(define (indent-nonblank-line l prefix)
  (if (string=? l "") "" (string-append prefix l)))

;; ===========================================================================
;; Description lists (dl/dt/dd)

(: render-dl (-> element String))
(define (render-dl e)
  (define flat
    (filter (lambda ([x : Content])
              (and (element? x) (and (memq (element-tag x) '(dt dd)) #t)))
            (flatten* (element-children e))))
  (let loop ([xs : (Listof Content) flat] [acc : (Listof String) '()])
    (cond
      [(null? xs) (string-join (reverse acc) "\n")]
      [else
       (define x (assert (car xs) element?))
       (cond
         [(eq? (element-tag x) 'dt)
          (define term (string-trim (render-inline (element-children x))))
          (define nxt (and (pair? (cdr xs)) (assert (cadr xs) element?)))
          (cond
            [(and nxt (eq? (element-tag nxt) 'dd))
             (define desc (string-trim (render-inline (element-children nxt))))
             (loop (cddr xs) (cons (string-append "/ " term ": " desc) acc))]
            [else
             (loop (cdr xs) (cons (string-append "/ " term ": ") acc))])]
         [else ;; a stray dd with no preceding dt
          (loop (cdr xs)
                (cons (string-append "/ : "
                                     (string-trim (render-inline (element-children x))))
                      acc))])])))

;; ===========================================================================
;; Code blocks

(: render-code-block (-> element String))
(define (render-code-block e)
  (define code (findf (lambda ([c : Content])
                        (and (element? c) (eq? (element-tag c) 'code)))
                      (flatten* (element-children e))))
  (cond
    [(element? code)
     (define lang (code-lang code))
     (define text (plain-text (element-children code)))
     (define fence (backtick-fence text 3))
     (string-append fence lang "\n" text "\n" fence)]
    [else
     ;; A <pre> without a <code> child: emit its text verbatim in a raw block.
     (define text (plain-text (element-children e)))
     (define fence (backtick-fence text 3))
     (string-append fence "\n" text "\n" fence)]))

(: code-lang (-> element String))
(define (code-lang code)
  (define cls (assq 'class (element-attrs code)))
  (cond
    [(and cls (regexp-match #rx"^language-(.+)$" (cdr cls)))
     => (lambda (m) (assert (cadr m) string?))]
    [else ""]))

(: backtick-fence (-> String Integer String))
(define (backtick-fence text min-len)
  (make-string (max min-len (add1 (longest-run #\` text))) #\`))

(: longest-run (-> Char String Integer))
(define (longest-run ch s)
  (define n (string-length s))
  (let loop ([i : Integer 0] [cur : Integer 0] [best : Integer 0])
    (cond
      [(>= i n) (max cur best)]
      [(char=? (string-ref s i) ch) (loop (add1 i) (add1 cur) best)]
      [else (loop (add1 i) 0 (max cur best))])))

;; Concatenate all string leaves of a content tree (raw text passes through).
(: plain-text (-> Content String))
(define (plain-text c)
  (cond
    [(string? c) c]
    [(element? c) (apply string-append (map plain-text (element-children c)))]
    [(raw? c) (raw-text c)]
    [(delayed? c) ""]
    [else (apply string-append (map plain-text c))]))

;; ===========================================================================
;; Tables

(: render-table (-> element String))
(define (render-table e)
  (define parts (flatten* (element-children e)))
  (define head-rows (rows-under parts 'thead))
  (define body-rows (rows-under parts 'tbody))
  (define all-rows (append head-rows body-rows))
  (define columns
    (apply max 1 (for/list : (Listof Integer) ([r (in-list all-rows)])
                   (length (row-cells r)))))
  (define lines : (Listof String)
    (append
     (list (string-append "  columns: " (number->string columns) ","))
     (if (null? head-rows)
         '()
         (list (string-append
                "  table.header("
                (string-join (append-map render-cells head-rows) ", ")
                "),")))
     (for/list : (Listof String) ([r (in-list body-rows)])
       (string-append "  " (string-join (render-cells r) ", ") ","))))
  (string-append "#table(\n" (string-join lines "\n") "\n)"))

;; Child elements (splice-flattened) whose tag is in `tags`.
(: elements-with-tag (-> (Listof Content) (Listof Symbol) (Listof element)))
(define (elements-with-tag lst tags)
  (for/list : (Listof element)
            ([x (in-list lst)]
             #:when (element? x)
             #:when (memq (element-tag x) tags))
    x))

;; tr elements found directly under a wrapper tag (thead/tbody).
(: rows-under (-> (Listof Content) Symbol (Listof element)))
(define (rows-under parts wrapper)
  (append-map
   (lambda ([w : element]) (elements-with-tag (flatten* (element-children w)) '(tr)))
   (elements-with-tag parts (list wrapper))))

(: row-cells (-> element (Listof element)))
(define (row-cells tr)
  (elements-with-tag (flatten* (element-children tr)) '(th td)))

(: render-cells (-> element (Listof String)))
(define (render-cells tr)
  (for/list : (Listof String) ([c (in-list (row-cells tr))])
    ;; A Typst `[..]` cell is a content block: it may hold block content
    ;; (paragraphs, lists, ..), so render block-aware rather than inline-only.
    (string-append "[" (string-trim (render-content (element-children c))) "]")))

;; ===========================================================================
;; Math

(define math-special : (Listof Char)
  '(#\_ #\^ #\& #\# #\$ #\\ #\` #\~ #\{ #\} #\"))

(: math-needs-string? (-> String Boolean))
(define (math-needs-string? s)
  (for/or : Boolean ([c (in-string s)]) (and (memv c math-special) #t)))

(: math-string (-> String String))
(define (math-string s)
  (string-append "\"" (escape-string s) "\""))

(: render-math (-> element String))
(define (render-math e)
  (define disp (assq 'display (element-attrs e)))
  (define body (string-join (map render-mnode (element-children e)) " "))
  (if (and disp (equal? (cdr disp) "block"))
      (string-append "$ " body " $")
      (string-append "$" body "$")))

(: render-mnode (-> Content String))
(define (render-mnode node)
  (cond
    [(string? node) (math-string node)]
    [(element? node) (render-mnode-el node)]
    [(list? node) (string-join (map render-mnode node) " ")]
    [else (error 'content->typst "cannot render math node ~e" node)]))

(: render-mnode-el (-> element String))
(define (render-mnode-el node)
  (define tag (element-tag node))
  (define kids (element-children node))
  (case tag
    [(mi mo) (define t (plain-text kids))
             (if (math-needs-string? t) (math-string t) t)]
    [(mn) (plain-text kids)]
    [(mtext) (math-string (plain-text kids))]
    [(mrow) (string-join (map render-mnode kids) " ")]
    [(msub) (string-append (render-mnode (car kids)) "_(" (render-mnode (cadr kids)) ")")]
    [(msup) (string-append (render-mnode (car kids)) "^(" (render-mnode (cadr kids)) ")")]
    [(msubsup)
     (string-append (render-mnode (car kids))
                    "_(" (render-mnode (cadr kids)) ")"
                    "^(" (render-mnode (caddr kids)) ")")]
    [(mfrac)
     (string-append "frac(" (render-mnode (car kids)) ", " (render-mnode (cadr kids)) ")")]
    [(msqrt) (string-append "sqrt(" (render-mnode (car kids)) ")")]
    [(mroot)
     ;; children are (base index); Typst root(index, radicand)
     (string-append "root(" (render-mnode (cadr kids)) ", " (render-mnode (car kids)) ")")]
    [else (error 'content->typst "unknown MathML node ~a" tag)]))

;; ===========================================================================
;; Shared

(: render-raw (-> Content String))
(define (render-raw c)
  (cond
    [(raw? c)
     (unless (eq? (raw-backend c) 'typst)
       (error 'content->typst
              "raw node for backend ~a cannot be rendered to typst" (raw-backend c)))
     (raw-text c)]
    [else (error 'content->typst "render-raw: not a raw node: ~e" c)]))

;; Internal `%`-prefixed tags (only %para-break so far) must never reach a
;; backend — decode strips them; a leak is a bug, not silent output.
(: reject-internal (-> Symbol (U srcloc False) Void))
(define (reject-internal tag loc)
  (when (regexp-match? #rx"^%" (symbol->string tag))
    (error 'content->typst "internal element ~a leaked into backend (at ~a)" tag loc)))

;; ===========================================================================
;; Tests

(module+ test
  (require typed/rackunit)

  ;; --- escape-typst ---
  (check-equal? (escape-typst "plain text") "plain text")
  (check-equal? (escape-typst "a #b *c* _d_") "a \\#b \\*c\\* \\_d\\_")
  (check-equal? (escape-typst "$x$ @ref <lbl>") "\\$x\\$ \\@ref \\<lbl\\>")
  (check-equal? (escape-typst "[a] `b` ~c\\d") "\\[a\\] \\`b\\` \\~c\\\\d")
  ;; line-leading markers escaped
  (check-equal? (escape-typst "= not a heading") "\\= not a heading")
  (check-equal? (escape-typst "- not a list") "\\- not a list")
  (check-equal? (escape-typst "1. not an enum") "1\\. not an enum")
  ;; but a `-` mid-line (en-dash cosmetics aside) is not line-structural
  (check-equal? (escape-typst "a - b") "a - b")
  ;; only newline resets line-start
  (check-equal? (escape-typst "x\n= y") "x\n\\= y")
  ;; comment openers escaped anywhere (escaping the first `/` breaks `//`)
  (check-equal? (escape-typst "http://x") "http:\\//x")
  (check-equal? (escape-typst "a /* b") "a \\/\\* b")
  (check-equal? (escape-typst "a / b") "a / b")

  ;; --- plain text / paragraphs ---
  (check-equal? (content->typst "hello") "hello")
  ;; two block paragraphs are separated by a blank line
  (check-equal? (content->typst (list (elem 'p "a") (elem 'p "b"))) "a\n\nb")

  ;; --- inline emphasis / strong / code ---
  (check-equal? (content->typst (elem 'em "x")) "_x_")
  (check-equal? (content->typst (elem 'strong "x")) "*x*")
  (check-equal? (content->typst (elem 'code "x = 1")) "`x = 1`")
  ;; inline code containing a backtick falls back to #raw
  (check-equal? (content->typst (elem 'code "a`b")) "#raw(\"a`b\")")

  ;; --- link / image ---
  (check-equal? (content->typst (elem 'a #:attrs '((href . "https://x")) "t"))
                "#link(\"https://x\")[t]")
  (check-equal? (content->typst (elem 'img #:attrs '((src . "c.png") (alt . "a cat"))))
                "#image(\"c.png\", alt: \"a cat\")")
  ;; a ref whose target label is NOT on this page: its own id is anchored, but
  ;; the href stays a string link (no `#link(<def-vm>)` to a missing label)
  (check-equal? (content->typst (elem 'a #:attrs '((id . "ref-1") (href . "#def-vm")) "vm"))
                "#metadata(none)<ref-1>#link(\"#def-vm\")[vm]")
  ;; …but when the target label IS on the page, the link resolves to it and the
  ;; target carries a `<label>` anchor
  (check-equal?
   (content->typst
    (list (elem 'a #:attrs '((id . "ref-1") (href . "#def-vm")) "vm")
          " " (elem 'dfn #:attrs '((id . "def-vm")) "virtual machine")))
   "#metadata(none)<ref-1>#link(<def-vm>)[vm] #metadata(none)<def-vm>#emph[virtual machine]")
  ;; a cross-FILE fragment link (builder-rewritten) stays a string link
  (check-equal? (content->typst (elem 'a #:attrs '((href . "intro.typ#def-vm")) "vm"))
                "#link(\"intro.typ#def-vm\")[vm]")
  ;; an id that is not a valid Typst label token gets neither a label nor a
  ;; label-link — it degrades to a string link so compilation is safe
  (check-equal?
   (content->typst
    (list (elem 'a #:attrs '((href . "#bad id")) "x")
          (elem 'dfn #:attrs '((id . "bad id")) "d")))
   "#link(\"#bad id\")[x]#emph[d]")

  ;; an id on a structural table wrapper (a `td`) is NOT anchored (the cell is
  ;; destructured by render-table, so no label would be emitted); a same-page
  ;; link to it must therefore degrade to a string link, never `#link(<cell>)`
  (check-equal?
   (content->typst
    (list (elem 'a #:attrs '((href . "#cell")) "go")
          (elem 'table (elem 'tbody (elem 'tr (elem 'td #:attrs '((id . "cell")) "x"))))))
   (string-append "#link(\"#cell\")[go]\n\n#table(\n  columns: 1,\n  [x],\n)"))
  ;; …but an id on an element INSIDE a cell is anchored (its content is rendered
  ;; through render-inline-element) and so resolves as a label link
  (check-equal?
   (content->typst
    (list (elem 'a #:attrs '((href . "#d1")) "go")
          (elem 'table (elem 'tbody (elem 'tr (elem 'td (elem 'dfn #:attrs '((id . "d1")) "t")))))))
   (string-append "#link(<d1>)[go]\n\n#table(\n  columns: 1,\n  [#metadata(none)<d1>#emph[t]],\n)"))
  ;; a nested list that is a direct child of a list item is anchored, so a
  ;; same-page link to its id resolves (regression: item-pieces bypassed
  ;; render-block-element, leaving the label unemitted)
  (check-equal?
   (content->typst
    (list (elem 'a #:attrs '((href . "#sub")) "go")
          (elem 'ul (elem 'li "head" (elem 'ul #:attrs '((id . "sub")) (elem 'li "x"))))))
   "#link(<sub>)[go]\n\n- head\n  #metadata(none)<sub>\n  - x")

  ;; a heading carrying an id is preceded by an invisible labelled anchor
  (check-equal? (content->typst (elem 'h1 #:attrs '((id . "sec-1")) "Title"))
                "#metadata(none)<sec-1>\n= Title")
  ;; a footnote-style list item id is anchored right after the marker
  (check-equal? (content->typst (elem 'ol (elem 'li #:attrs '((id . "fn-note-1")) "note")))
                "+ #metadata(none)<fn-note-1> note")

  ;; --- headings ---
  (check-equal? (content->typst (elem 'h1 "Title")) "= Title")
  (check-equal? (content->typst (elem 'h3 "Sub")) "=== Sub")
  (check-equal? (content->typst (elem 'section (elem 'h2 "S") (elem 'p "body")))
                "== S\n\nbody")

  ;; --- lists ---
  (check-equal? (content->typst (elem 'ul (elem 'li "a") (elem 'li "b")))
                "- a\n- b")
  (check-equal? (content->typst (elem 'ol (elem 'li "a") (elem 'li "b")))
                "+ a\n+ b")
  ;; nested list under an item, 2-space indent
  (check-equal?
   (content->typst (elem 'ul (elem 'li "a" (elem 'ul (elem 'li "b")))))
   "- a\n  - b")

  ;; --- list item whose body is paragraph(s): content sits on the marker line,
  ;; not pushed to an empty-marker sub-line (regression) ---
  (check-equal?
   (content->typst (elem 'ul (elem 'li (elem 'p "note body"))))
   "- note body")
  (check-equal?
   (content->typst (elem 'ul (elem 'li (elem 'p "one") (elem 'p "two"))))
   "- one\n\n  two")
  ;; a leading paragraph followed by a nested list still nests correctly
  (check-equal?
   (content->typst (elem 'ol (elem 'li (elem 'p "head") (elem 'ul (elem 'li "x")))))
   "+ head\n  - x")
  ;; a list item that is ONLY a nested list keeps the marker line bare — a
  ;; shared line ("- - b") would flatten the sublist to literal text (regression)
  (check-equal?
   (content->typst (elem 'ul (elem 'li (elem 'ul (elem 'li "b")))))
   "-\n  - b")
  ;; empty / whitespace-only paragraphs in an item do not crash and add no
  ;; spurious blank lines (regression: car of '() on the marker line)
  (check-equal? (content->typst (elem 'ul (elem 'li (elem 'p)))) "-")
  (check-equal? (content->typst (elem 'ul (elem 'li (elem 'p "   ")))) "-")
  (check-equal?
   (content->typst (elem 'ul (elem 'li (elem 'p) (elem 'p "y")))) "- y")
  (check-equal?
   (content->typst (elem 'ul (elem 'li "x" (elem 'p) (elem 'p "y")))) "- x\n\n  y")

  ;; --- table cell carrying block content renders inside the [..] cell,
  ;; instead of erroring in inline context (regression) ---
  (check-equal?
   (content->typst
    (elem 'table
          (elem 'tbody (elem 'tr (elem 'td (elem 'p "a") (elem 'p "b"))
                             (elem 'td "plain")))))
   (string-append "#table(\n  columns: 2,\n  [a\n\nb], [plain],\n)"))

  ;; --- blockquote / hr / sup ---
  (check-equal? (content->typst (elem 'blockquote "q"))
                "#quote(block: true)[\nq\n]")
  (check-equal? (content->typst (elem 'hr)) "#line(length: 100%)")
  (check-equal? (content->typst (elem 'sup "2")) "#super[2]")

  ;; --- linebreak (br) inline / pagebreak (styled div) block ---
  (check-equal? (content->typst (elem 'p "a" (elem 'br) "b")) "a#linebreak()b")
  (check-equal? (content->typst (elem 'div #:attrs '((style . "break-after: page"))))
                "#pagebreak()")
  ;; a div with any other style is unsupported (fails loudly, never dropped)
  (check-exn #rx"cannot render block element div"
             (lambda () (content->typst (elem 'div #:attrs '((style . "color: red"))))))

  ;; --- code block ---
  (check-equal?
   (content->typst
    (elem 'pre (elem 'code #:attrs '((class . "language-python"))
                     "def f():" (elem 'span #:attrs '((class . "tok-comment")) " # c"))))
   "```python\ndef f(): # c\n```")

  ;; --- table ---
  (check-equal?
   (content->typst
    (elem 'table
          (elem 'thead (elem 'tr (elem 'th "A") (elem 'th "B")))
          (elem 'tbody (elem 'tr (elem 'td "x") (elem 'td "y")))))
   (string-append "#table(\n  columns: 2,\n"
                  "  table.header([A], [B]),\n"
                  "  [x], [y],\n)"))

  ;; --- description list ---
  (check-equal?
   (content->typst (elem 'dl (elem 'dt "term") (elem 'dd "definition")))
   "/ term: definition")

  ;; --- math: inline vs block, structure ---
  (check-equal?
   (content->typst
    (elem 'math #:attrs '((display . "inline"))
          (elem 'mrow (elem 'mn "1") (elem 'mo "+") (elem 'mi "x"))))
   "$1 + x$")
  (check-equal?
   (content->typst
    (elem 'math #:attrs '((display . "block"))
          (elem 'msubsup (elem 'mo "∑") (elem 'mi "i") (elem 'mi "n"))))
   "$ ∑_(i)^(n) $")
  (check-equal?
   (content->typst
    (elem 'math #:attrs '((display . "inline"))
          (elem 'mfrac (elem 'mi "a") (elem 'mi "b"))))
   "$frac(a, b)$")
  ;; math-special operator glyphs become a string so they compile
  (check-equal?
   (content->typst
    (elem 'math #:attrs '((display . "inline")) (elem 'mo "&")))
   "$\"&\"$")

  ;; --- raw passthrough ---
  (check-equal? (content->typst (raw 'typst "#pagebreak()" #t)) "#pagebreak()")
  (check-equal? (content->typst (list "a " (raw 'typst "#h(1em)") "b"))
                "a #h(1em)b")

  ;; --- error paths ---
  (check-exn #rx"raw node for backend html"
             (lambda () (content->typst (raw 'html "<b>x</b>"))))
  (check-exn #rx"unresolved delayed node"
             (lambda () (content->typst (delayed (lambda (i) "")))))
  (check-exn #rx"internal element %para-break leaked"
             (lambda () (content->typst (element '%para-break '() '() #f)))))
