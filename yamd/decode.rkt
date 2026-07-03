#lang typed/racket/base
;; Flow decoding: groups inline runs into <p> paragraphs, Scribble-decode
;; style. The reader emits an (element '%para-break '() '() loc) marker for
;; every blank source line; block-level elements also terminate a paragraph.

(require "ir.rkt")

(provide decode-flow
         block-tag?
         para-break
         para-break?)

(define para-break-tag : Symbol '%para-break)

(: para-break (-> element))
(define (para-break) (element para-break-tag '() '() #f))

(: para-break? (-> Content Boolean))
(define (para-break? c)
  (and (element? c) (eq? (element-tag c) para-break-tag)))

(define block-tags : (Listof Symbol)
  '(p div section h1 h2 h3 h4 h5 h6
    ul ol li dl dt dd blockquote pre hr
    table thead tbody tr figure figcaption main
    article aside nav header footer address details summary form
    video audio))

(: block-tag? (-> Symbol Boolean))
(define (block-tag? tag)
  (and (memq tag block-tags) #t))

;; Block-level content that must not be wrapped in <p> and that terminates the
;; surrounding paragraph: block elements and block-flagged raw nodes (e.g.
;; #html-block{<div>…</div>}). Inline raw stays part of the flow.
(: block-content? (-> Content Boolean))
(define (block-content? c)
  (or (and (element? c) (block-tag? (element-tag c)))
      (and (raw? c) (raw-block? c))))

(: whitespace-string? (-> Content Boolean))
(define (whitespace-string? c)
  (and (string? c) (regexp-match? #px"^\\s*$" c)))

;; Flattens splices, then partitions into paragraphs. A run of inline content
;; becomes (element 'p ...); block elements pass through untouched. Whitespace
;; runs at paragraph edges are dropped.
;;
;; Decoding recurses into element children (indented block / `::` hanging
;; bodies put %para-break markers inside the enclosing call's element):
;; children containing a %para-break are paragraph-grouped exactly like the
;; top-level flow; children without one are left untouched. Inline `{...}`
;; bodies never contain markers (a blank line there is literal text), so
;; this changes nothing for inline content. Guarantee: no %para-break marker
;; survives decoding anywhere in the tree.
(: decode-flow (-> (Listof Content) (Listof Content)))
(define (decode-flow flow)
  (group-flow (flatten-content (map decode-content flow))))

(: decode-content (-> Content Content))
(define (decode-content c)
  (cond
    [(element? c)
     (define kids (map decode-content (element-children c)))
     ;; Only a %para-break triggers grouping of an element's children; block
     ;; content alone (block elements or block raws) does not — a marker-less
     ;; body stays un-grouped, so a block child is left bare (never
     ;; <p>-wrapped either way). Consistent for block elements and block raws.
     (element (element-tag c)
              (element-attrs c)
              (if (ormap has-para-break? kids)
                  (group-flow (flatten-content kids))
                  kids)
              (element-loc c))]
    [(or (string? c) (raw? c) (delayed? c)) c]
    [else (map decode-content c)]))

;; Splice-transparent para-break search; nested elements are handled by
;; their own decode-content recursion, so don't look inside them.
(: has-para-break? (-> Content Boolean))
(define (has-para-break? c)
  (cond
    [(para-break? c) #t]
    [(element? c) #f]
    [(string? c) #f]
    [(raw? c) #f]
    [(delayed? c) #f]
    [else (ormap has-para-break? c)]))

;; Groups an already-flattened, already-decoded flow into paragraphs.
(: group-flow (-> (Listof Content) (Listof Content)))
(define (group-flow flat)
  (let loop ([items flat]
             [para : (Listof Content) '()]
             [out : (Listof Content) '()])
    (: flush (-> (Listof Content) (Listof Content)))
    (define (flush acc)
      (define body (trim-run (reverse para)))
      (if (null? body) acc (cons (element 'p '() body #f) acc)))
    (cond
      [(null? items) (reverse (flush out))]
      [(para-break? (car items)) (loop (cdr items) '() (flush out))]
      [(block-content? (car items)) (loop (cdr items) '() (cons (car items) (flush out)))]
      [else (loop (cdr items) (cons (car items) para) out)])))

(: trim-run (-> (Listof Content) (Listof Content)))
(define (trim-run items)
  (: drop-ws (-> (Listof Content) (Listof Content)))
  (define (drop-ws xs)
    (if (and (pair? xs) (whitespace-string? (car xs))) (drop-ws (cdr xs)) xs))
  (reverse (drop-ws (reverse (drop-ws items)))))

(: flatten-content (-> (Listof Content) (Listof Content)))
(define (flatten-content items)
  (for/fold ([out : (Listof Content) '()]
             #:result (reverse out))
            ([item (in-list items)])
    (if (or (string? item) (element? item) (raw? item) (delayed? item))
        (cons item out)
        (append (reverse (flatten-content item)) out))))

;; ---------------------------------------------------------------------------
;; Tests

(module+ test
  (require typed/rackunit)

  ;; Inline raw joins the paragraph flow (wrapped in <p>).
  (check-equal? (decode-flow (list "a " (raw 'html "<b>x</b>") " b"))
                (list (element 'p '() (list "a " (raw 'html "<b>x</b>") " b") #f)))

  ;; Block raw is NOT wrapped in <p>; it terminates the surrounding paragraph
  ;; and passes through as a standalone node, exactly like a block element.
  (check-equal? (decode-flow (list "before"
                                   (raw 'html "<div>x</div>" #t)
                                   "after"))
                (list (element 'p '() (list "before") #f)
                      (raw 'html "<div>x</div>" #t)
                      (element 'p '() (list "after") #f)))

  ;; Prose around a block raw, separated by blank lines, still forms paragraphs.
  (check-equal? (decode-flow (list "one" (para-break)
                                   (raw 'html "<div>x</div>" #t)
                                   (para-break) "two"))
                (list (element 'p '() (list "one") #f)
                      (raw 'html "<div>x</div>" #t)
                      (element 'p '() (list "two") #f)))

  ;; A block element and a block raw are treated identically by grouping.
  (check-equal? (decode-flow (list (raw 'html "<div>x</div>" #t)))
                (list (raw 'html "<div>x</div>" #t))))
