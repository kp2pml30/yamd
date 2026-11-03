#lang racket/base
;; Standard library, minimal core set. Everything here is an
;; ordinary function from Content arguments to IR; `section`/`fork-context`
;; take LAZY bodies (thunks) so they can force them inside a forked context
;; (see docs/architecture.md "Contexts"). The `yamd-reader` submodule at the bottom
;; registers those two as lazy body readers.

(require "../runtime.rkt"
         (only-in "../decode.rkt" para-break?)
         (only-in "../image.rkt" current-image-resolver image-src->path)
         (only-in racket/list append-map))

(provide emph strong tt
         linebreak
         para
         (rename-out [yamd-link link])
         image
         blockquote
         hr
         pagebreak
         section
         (rename-out [yamd-list list])
         item
         fork-context)

;; ---------------------------------------------------------------------------
;; Inline text

(define (emph . bodies)
  (apply elem 'em bodies))

(define (strong . bodies)
  (apply elem 'strong bodies))

(define (tt . bodies)
  (apply elem 'code bodies))

;; #linebreak — an inline hard line break inside a paragraph. Zero-arg, void
;; element (no body, no args), modeled on #hr. `br` is NOT a block tag, so
;; decode-flow keeps it in the paragraph flow (unlike #hr, which breaks it).
;; HTML: <br />. Markdown: a CommonMark hard break (backslash + newline),
;; falling back to inline <br /> inside pipe-table cells / headings where a
;; literal newline is destructive. Typst: #linebreak().
(define (linebreak)
  (elem 'br))

;; #link("https://…"){text} — the URL is the first (host) argument, the link
;; text is the body. Renders an <a href="…"> anchor (inline).
(define (yamd-link url . bodies)
  (unless (string? url)
    (error 'link "url must be a string, got ~e" url))
  (apply elem 'a #:attrs (list (cons 'href url)) bodies))

;; #image("url", alt: "description") — an inline <img>. Both the src and the
;; alt text are required. img is a void element (no body).
;;
;; alt is mandatory ON PURPOSE, not defaulted: HTML requires it for validity,
;; and a missing alt is invisible to sighted authors while breaking screen
;; readers, search indexers and no-image fallbacks. Rather than silently emit
;; an inaccessible <img>, we raise a located error. For a genuinely decorative
;; image that should be *skipped* by assistive tech, pass an explicit empty
;; alt: "" — that is the accessible way to say "no textual content", and the
;; explicitness keeps it a deliberate choice, not an oversight.
;;
;; A local src is resolved like #include and sized from the file's header
;; (yamd/image): width/height attributes reserve layout space before the image
;; loads. Explicit width:/height: win; one alone keeps the aspect ratio.
;; loading: "lazy" (default) / "eager", decoding="async" always. The attrs
;; come from current-image-resolver (the plugin hook); ours fill the rest.
(define (image url #:alt [alt #f] #:width [width #f] #:height [height #f]
               #:loading [loading "lazy"])
  (unless (string? url)
    (error 'image "url must be a string, got ~e" url))
  (unless alt
    (error 'image
           "image requires alt: text for accessibility (screen readers, indexing); pass alt: \"\" for a purely decorative image"))
  (unless (string? alt)
    (error 'image "alt must be a string, got ~e" alt))
  (for ([v (in-list (list width height))] [name '(width height)])
    (unless (or (not v) (exact-positive-integer? v))
      (error 'image "~a must be a positive integer (pixels), got ~e" name v)))
  (unless (member loading '("lazy" "eager"))
    (error 'image "loading must be \"lazy\" or \"eager\", got ~e" loading))
  (define attrs ((current-image-resolver) url (image-src->path url) width height))
  (define ours (list (cons 'alt alt) (cons 'loading loading) (cons 'decoding "async")))
  (elem 'img #:attrs (append attrs (filter (lambda (a) (not (assq (car a) attrs))) ours))))

;; ---------------------------------------------------------------------------
;; Blocks

;; #quote{…} — a <blockquote>. Exported as `blockquote`, NOT `quote`, on
;; purpose: `quote` is a core Racket special form, and the reader emits a
;; call form `(name body …)`, so `#quote{…}` would read as `(quote (yamd-body
;; …))` — the quote special form producing the literal list, never calling
;; us. Shadowing core `quote` in the document namespace would both be fragile
;; (rebinding a special form) and break legitimate host use like `#('foo)` /
;; `#(quote …)`, so we take a plain, unambiguous name instead. blockquote is
;; a block tag (decode.rkt), so decode-flow leaves it untouched. It forks no
;; context and needs no lazy body, so it is a plain (non-lazy) function.
(define (blockquote . bodies)
  (apply elem 'blockquote bodies))

;; #hr — a horizontal rule. Zero-arg, void element (no body, no args). Block
;; tag, so decode-flow keeps it out of paragraphs.
(define (hr)
  (elem 'hr))

;; #pagebreak — a block-level page break. Zero-arg. Represented as a plain
;; <div style="break-after: page"></div>: `div` is a decode block tag, so it is
;; NOT wrapped in <p>, and every backend can pattern-match a plain element tag.
;; HTML emits it verbatim; markdown emits it as a raw HTML block; the typst
;; backend special-cases this exact style to #pagebreak(). Rationale for the IR
;; choice: a dedicated `%`-tag would be rejected by the backends as an internal
;; leak, and a bespoke element tag (e.g. 'pagebreak) would serialize as literal
;; <pagebreak> in HTML/markdown — a styled div is valid, portable HTML and the
;; single thing all three backends already recognize.
(define (pagebreak)
  (elem 'div #:attrs (list (cons 'style "break-after: page"))))

;; Explicit paragraph. `#para{…}` / `#para: …` / `#para` + block. Emits a <p>;
;; because <p> is a block tag, decode-flow leaves it untouched (it is not
;; re-grouped and not swallowed into a surrounding paragraph).
(define (para . bodies)
  (apply elem 'p bodies))

;; ---------------------------------------------------------------------------
;; Sections (auto header depth via context nesting)
;;
;; `#section` is registered LAZY: it receives its bodies as thunks. The FIRST
;; body is the section title; any REMAINING bodies are its content. Canonical
;; forms (all supported by the reader):
;;   #section{Title}                 heading only, no content
;;   #section{Title}::               title + hanging content (rest of block)
;;   #section{Title}{inline content} title + inline content
;; (An indented block body would require the call to be the only thing on its
;; line — `#section` alone, with the title as the block's first content — so
;; a `{Title}` + indented block combination is a reader error, not a form.)
;; Header level = context nesting depth: a top-level section is <h1>; a section
;; nested inside another section's content is <h2>; and so on through <h6>.
;; Nesting past 6 (i.e. depth ≥ 7) is an explicit error. Depth is tracked in
;; the context key 'header-depth; forking per section is what makes nesting
;; determine the level (docs/architecture.md "Contexts").
;;
;; ---- Section info for #toc ----------------------
;; At COLLECT time (sections run during pass 1; they are NOT delayed nodes, so
;; the depth is captured directly here, never inside a delayed proc) each
;; section records itself under the info key documented here:
;;
;;   info key:   'sections
;;   value:      a list of ENTRIES, accumulated newest-first (info-add! conses).
;;               A consumer (#toc) must (reverse …) to get document order.
;;   entry:      (list depth anchor title)
;;                 depth : exact integer in 1..6 — the heading level
;;                 anchor: string — the heading element's `id` attribute value
;;                         (a link target; prefix "#" to build an href)
;;                 title : Content — the section title (the heading's children),
;;                         to render as the TOC link text
;;
;; Anchors are "sec-<n>" with <n> a per-document counter kept under the private
;; info key 'yamd-section-counter (info-set!/info-ref, single-valued).
(define (section . body-thunks)
  (when (null? body-thunks)
    (error 'section "a section requires a title body, e.g. #section{Title}"))
  (with-forked-context
    (define depth (context-update! 'header-depth add1 0))
    (when (> depth 6)
      (error 'section
             "section nested too deep: header level ~a exceeds h6 (max nesting is 6)"
             depth))
    ;; Assign the anchor and record the #toc entry BEFORE forcing the content,
    ;; so nested sections (forced inside the content) get later anchors and are
    ;; recorded after this one — keeping both in document order (the 'sections
    ;; list, newest-first, reverses to document order).
    (define anchor (next-section-anchor))
    (define title ((car body-thunks)))
    (define i (current-info))
    (when i
      (info-add! i 'sections (list depth anchor title)))
    (define content (map (lambda (t) (t)) (cdr body-thunks)))
    (elem 'section
          (elem (heading-tag depth) #:attrs (list (cons 'id anchor)) title)
          content)))

(define (heading-tag depth)
  (string->symbol (string-append "h" (number->string depth))))

(define (next-section-anchor)
  (define i (current-info))
  (cond
    [i
     (define n (add1 (info-ref i 'yamd-section-counter 0)))
     (info-set! i 'yamd-section-counter n)
     (string-append "sec-" (number->string n))]
    ;; No info table (e.g. a unit test calling section directly): still emit
    ;; a unique anchor (gensym — not stable across runs) rather than crashing.
    [else (symbol->string (gensym 'sec-))]))

;; ---------------------------------------------------------------------------
;; Lists

;; #list(style: s){#item…} — an <ol type="1"> by default, an <ol type=s> for
;; 1/a/A/i/I, or an <ul> for -/*. Direct children must be #item (<li>) elements; structural
;; whitespace and paragraph breaks between items are dropped, anything else is
;; an explicit error (no silent coercion). Nesting is by putting a #list inside
;; an #item body.
(define (yamd-list #:style [style "1"] . bodies)
  (define items (collect-list-items 'list bodies))
  (cond
    [(ul-type style) (apply elem 'ul items)]
    [(ol-type style)
     => (lambda (type) (apply elem 'ol #:attrs (list (cons 'type type)) items))]
    [else
     (error 'list "unknown list style ~s; use one of \"1\" \"a\" \"A\" \"i\" \"I\" \"-\" \"*\""
            style)]))

(define (ul-type style)
  (member style '("-" "*")))

(define (ol-type style)
  (and (member style '("1" "a" "A" "i" "I")) style))

(define (item . bodies)
  (apply elem 'li bodies))

;; Flattens the body splices and keeps only <li> children; drops whitespace
;; strings and %para-break markers; errors on any other content.
(define (collect-list-items who bodies)
  (for/fold ([acc '()] #:result (reverse acc))
            ([c (in-list (flatten-content* bodies))])
    (cond
      [(and (string? c) (regexp-match? #px"^\\s*$" c)) acc]
      [(para-break? c) acc]
      [(and (element? c) (eq? (element-tag c) 'li)) (cons c acc)]
      [else
       (error who "list body may only contain #item children, got ~e" c)])))

(define (flatten-content* c)
  (if (list? c) (append-map flatten-content* c) (list c)))

;; ---------------------------------------------------------------------------
;; Context forking

;; #fork-context:: — forks the context for the dynamic extent of its body, so
;; settings made inside the body do not leak out. Registered LAZY so the body
;; is forced inside the fork.
(define (fork-context body)
  (with-forked-context (body)))

;; ---------------------------------------------------------------------------
;; Lazy body-reader registrations (merged into current-body-readers by the
;; reader; seeded by eval.rkt since std/base is attached by default).

(module* yamd-reader #f
  (require "../reader/registry.rkt")
  (provide body-readers)
  (define body-readers
    (hasheq 'section      (body-reader 'markup #t)
            'fork-context (body-reader 'markup #t))))

(module+ test
  (require rackunit)

  ;; --- link ---------------------------------------------------------------
  (check-equal? (yamd-link "https://x" "t")
                (elem 'a #:attrs '((href . "https://x")) "t"))
  (check-exn #rx"url must be a string" (lambda () (yamd-link 42 "t")))

  ;; --- image: void img, alt required -------------------------------------
  ;; (local files, sizing and the resolver hook: tests/image.rkt)
  (define lazy '((loading . "lazy") (decoding . "async")))
  (check-equal? (image "https://x/cat.png" #:alt "a cat")
                (elem 'img #:attrs `((src . "https://x/cat.png") (alt . "a cat") ,@lazy)))
  ;; empty alt is the deliberate "decorative" escape hatch — allowed.
  (check-equal? (image "https://x/sep.png" #:alt "")
                (elem 'img #:attrs `((src . "https://x/sep.png") (alt . "") ,@lazy)))
  (check-equal? (image "https://x/a.png" #:alt "a" #:width 10 #:loading "eager")
                (elem 'img #:attrs '((src . "https://x/a.png") (width . "10") (alt . "a")
                                     (loading . "eager") (decoding . "async"))))
  (check-exn #rx"image requires alt" (lambda () (image "cat.png")))
  (check-exn #rx"alt must be a string" (lambda () (image "cat.png" #:alt 42)))
  (check-exn #rx"url must be a string" (lambda () (image 42 #:alt "x")))
  (check-exn #rx"width must be a positive integer"
             (lambda () (image "https://x/a.png" #:alt "a" #:width 0)))
  (check-exn #rx"height must be a positive integer"
             (lambda () (image "https://x/a.png" #:alt "a" #:height "10")))
  (check-exn #rx"loading must be"
             (lambda () (image "https://x/a.png" #:alt "a" #:loading "auto")))

  ;; --- linebreak / blockquote / hr / pagebreak ----------------------------
  (check-equal? (linebreak) (elem 'br))
  (check-equal? (blockquote "quoted") (elem 'blockquote "quoted"))
  (check-equal? (hr) (elem 'hr))
  (check-equal? (pagebreak) (elem 'div #:attrs '((style . "break-after: page"))))

  ;; --- lists --------------------------------------------------------------
  (check-equal? (yamd-list (item "a") (item "b"))
                (elem 'ol #:attrs '((type . "1")) (elem 'li "a") (elem 'li "b")))
  (check-equal? (yamd-list #:style "-" (item "a") (item "b"))
                (elem 'ul (elem 'li "a") (elem 'li "b")))
  (check-equal? (yamd-list #:style "*" (item "a") (item "b"))
                (elem 'ul (elem 'li "a") (elem 'li "b")))
  (check-equal? (yamd-list #:style "1" (item "a"))
                (elem 'ol #:attrs '((type . "1")) (elem 'li "a")))
  ;; structural whitespace / para-breaks between items are dropped
  (check-equal? (yamd-list #:style "-" "\n" (item "a") (para-break) "  " (item "b"))
                (elem 'ul (elem 'li "a") (elem 'li "b")))
  (check-exn #rx"only contain #item" (lambda () (yamd-list "stray")))
  (check-exn #rx"unknown list style" (lambda () (yamd-list #:style "z" (item "a"))))

  ;; --- section: title required, depth limit, info capture -----------------
  (check-exn #rx"requires a title" (lambda () (section)))

  ;; Builds n nested sections; the innermost is at header depth n.
  (define (build-depth n)
    (if (<= n 1)
        (section (lambda () "leaf"))
        (section (lambda () "t") (lambda () (build-depth (sub1 n))))))
  (check-not-exn
   (lambda () (call-with-fresh-context (lambda () (build-depth 6)))))
  (check-exn #rx"exceeds h6"
             (lambda () (call-with-fresh-context (lambda () (build-depth 7)))))

  ;; A single top-level section is <h1>, records one 'sections entry, and
  ;; gets anchor sec-1.
  (let ([i (make-info)])
    (define el
      (parameterize ([current-info i])
        (call-with-fresh-context (lambda () (section (lambda () "Title"))))))
    (check-equal? (element-tag el) 'section)
    (define h (car (element-children el)))
    (check-equal? (element-tag h) 'h1)
    (check-equal? (element-attrs h) '((id . "sec-1")))
    (check-equal? (info-ref i 'sections #f) (list (list 1 "sec-1" "Title"))))

  ;; --- fork-context is output-transparent ---------------------------------
  (check-equal? (call-with-fresh-context
                 (lambda () (fork-context (lambda () "body"))))
                "body"))
