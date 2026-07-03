#lang typed/racket/base
;; Generic IR passes for the doc-set builder (docs/multi-doc.md). Typed, like
;; every IR traversal. None of these knows about toctrees, output modes or
;; backends — they are the four primitives the build strategies compose:
;;
;;   content-ids     collect every element `id` attribute (anchor registry)
;;   rewrite-hrefs   map every `href` attribute value (cross-page links)
;;   shift-headings  h1..h6 -> h(1+n).. (single-file toctree inlining)
;;   replace-tagged  substitute marker elements (e.g. %toctree)
;;
;; Sharp edges, on purpose:
;; - content-ids / rewrite-hrefs demand RESOLVED content (delayed = error):
;;   an unresolved node could hide anchors/links and silently break linking.
;; - shift-headings / replace-tagged run BEFORE resolution and pass delayed
;;   nodes through untouched — they cannot see inside one. Std plugins never
;;   emit headings or markers from delayed procs; a plugin that does opts out
;;   of shifting/replacement (documented in docs/multi-doc.md).
;; - raw nodes are opaque: ids/hrefs inside raw text are invisible to all
;;   four passes.

(require "../ir.rkt")

(provide content-ids
         rewrite-hrefs
         shift-headings
         number-headings
         replace-tagged)

;; All element `id` attribute values, in document order. Errors on a delayed
;; node — run only on resolved content.
(: content-ids (-> Content (Listof String)))
(define (content-ids c)
  (cond
    [(string? c) '()]
    [(raw? c) '()]
    [(delayed? c)
     (error 'content-ids "unresolved delayed node; resolve-content first")]
    [(element? c)
     (define own (assq 'id (element-attrs c)))
     (define below
       (for/fold ([acc : (Listof String) '()])
                 ([ch (in-list (element-children c))])
         (append acc (content-ids ch))))
     (if own (cons (cdr own) below) below)]
    [else
     (for/fold ([acc : (Listof String) '()])
               ([ch (in-list c)])
       (append acc (content-ids ch)))]))

;; Applies `f` to every `href` attribute value; #f keeps the old value.
;; Errors on a delayed node — run only on resolved content.
(: rewrite-hrefs (-> Content (-> String (U String False)) Content))
(define (rewrite-hrefs c f)
  (cond
    [(string? c) c]
    [(raw? c) c]
    [(delayed? c)
     (error 'rewrite-hrefs "unresolved delayed node; resolve-content first")]
    [(element? c)
     (element (element-tag c)
              (for/list : Attrs ([a (in-list (element-attrs c))])
                (if (eq? (car a) 'href)
                    (cons 'href (or (f (cdr a)) (cdr a)))
                    a))
              (for/list : (Listof Content) ([ch (in-list (element-children c))])
                (rewrite-hrefs ch f))
              (element-loc c))]
    [else
     (for/list : (Listof Content) ([ch (in-list c)])
       (rewrite-hrefs ch f))]))

(define heading-levels : (Listof (Pairof Symbol Integer))
  '((h1 . 1) (h2 . 2) (h3 . 3) (h4 . 4) (h5 . 5) (h6 . 6)))

;; Shifts every heading n levels deeper (h1 -> h(1+n)). Shifting past h6 is
;; an explicit error (`who` names the caller's subject, e.g. the inlined
;; document). n = 0 returns the content unchanged. Delayed nodes pass
;; through (see header comment).
(: shift-headings (-> Content Integer [#:who Any] Content))
(define (shift-headings c n #:who [who "content"])
  (cond
    [(zero? n) c]
    [(string? c) c]
    [(raw? c) c]
    [(delayed? c) c]
    [(element? c)
     (define lvl (assq (element-tag c) heading-levels))
     (define tag
       (cond
         [(not lvl) (element-tag c)]
         [(> (+ (cdr lvl) n) 6)
          (error 'shift-headings
                 "heading ~a in ~a shifted past h6 (by ~a levels; at ~a)"
                 (element-tag c) who n (element-loc c))]
         [else (string->symbol (format "h~a" (+ (cdr lvl) n)))]))
     (element tag
              (element-attrs c)
              (for/list : (Listof Content) ([ch (in-list (element-children c))])
                (shift-headings ch n #:who who))
              (element-loc c))]
    [else
     (for/list : (Listof Content) ([ch (in-list c)])
       (shift-headings ch n #:who who))]))

;; Prepends a section number to every element carrying an `id` present in
;; `numbers` (the map is built from section anchors, so only headings match).
;; The number is spliced as a leading text child ("1.2 "); the element's
;; attributes and tag are untouched, so the heading keeps its anchor. Runs on
;; RESOLVED content (both build strategies apply it after resolution); a
;; delayed node passes through (numbering never reaches inside one). An empty
;; map returns the content unchanged.
(: number-headings (-> Content (HashTable String String) Content))
(define (number-headings c numbers)
  (cond
    [(zero? (hash-count numbers)) c]
    [else
     (let walk : Content ([c : Content c])
       (cond
         [(string? c) c]
         [(raw? c) c]
         [(delayed? c) c]
         [(element? c)
          (define id (assq 'id (element-attrs c)))
          (define num (and id (hash-ref numbers (cdr id) #f)))
          (define kids
            (for/list : (Listof Content) ([ch (in-list (element-children c))])
              (walk ch)))
          (element (element-tag c)
                   (element-attrs c)
                   (if num (cons (string-append num " ") kids) kids)
                   (element-loc c))]
         [else
          (for/list : (Listof Content) ([ch (in-list c)])
            (walk ch))]))]))

;; Replaces every element whose tag is `tag` with (f element). The
;; replacement is spliced as-is and NOT re-scanned — a substitution that
;; introduces further markers must recurse in `f` itself (this is what lets
;; the single-file strategy expand nested toctrees with per-level heading
;; shifts). Delayed nodes pass through (see header comment).
(: replace-tagged (-> Content Symbol (-> element Content) Content))
(define (replace-tagged c tag f)
  (cond
    [(string? c) c]
    [(raw? c) c]
    [(delayed? c) c]
    [(element? c)
     (if (eq? (element-tag c) tag)
         (f c)
         (element (element-tag c)
                  (element-attrs c)
                  (for/list : (Listof Content) ([ch (in-list (element-children c))])
                    (replace-tagged ch tag f))
                  (element-loc c)))]
    [else
     (for/list : (Listof Content) ([ch (in-list c)])
       (replace-tagged ch tag f))]))

;; ---------------------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (define tree
    (list (element 'h1 '((id . "a")) (list "T") #f)
          (element 'p '()
                   (list (element 'a '((href . "#a")) (list "x") #f)
                         (element 'a '((id . "r1") (href . "http://x")) (list "y") #f))
                   #f)))

  ;; --- content-ids ---
  (check-equal? (content-ids tree) '("a" "r1"))
  (check-exn #rx"unresolved delayed"
             (lambda () (content-ids (delayed (lambda ([i : info]) "")))))

  ;; --- rewrite-hrefs: #f keeps, non-href attrs untouched ---
  (define rewritten
    (rewrite-hrefs tree (lambda ([h : String])
                          (and (equal? h "#a") "other.html#a"))))
  (check-equal? (content-ids rewritten) '("a" "r1"))
  (check-equal?
   rewritten
   (list (element 'h1 '((id . "a")) (list "T") #f)
         (element 'p '()
                  (list (element 'a '((href . "other.html#a")) (list "x") #f)
                        (element 'a '((id . "r1") (href . "http://x")) (list "y") #f))
                  #f)))
  (check-exn #rx"unresolved delayed"
             (lambda () (rewrite-hrefs (delayed (lambda ([i : info]) "")) (lambda (h) #f))))

  ;; --- shift-headings ---
  (check-equal? (shift-headings (element 'h2 '() (list "t") #f) 1)
                (element 'h3 '() (list "t") #f))
  ;; n=0 identity fast path (eq? computed here: typed/rackunit's boundary
  ;; chaperones arguments, so check-eq? would compare wrappers).
  (check-true (eq? (shift-headings tree 0) tree))
  (check-exn #rx"heading h6 in ch1 shifted past h6"
             (lambda () (shift-headings (element 'h6 '() '() #f) 1 #:who "ch1")))
  ;; delayed passes through untouched
  (let ([d (delayed (lambda ([i : info]) ""))])
    (check-true (eq? (shift-headings d 3) d)))

  ;; --- number-headings ---
  (define nums (make-hash (list (cons "a" "1"))))
  (check-equal?
   (number-headings tree nums)
   (list (element 'h1 '((id . "a")) (list "1 " "T") #f)
         (element 'p '()
                  (list (element 'a '((href . "#a")) (list "x") #f)
                        (element 'a '((id . "r1") (href . "http://x")) (list "y") #f))
                  #f)))
  ;; empty map = identity (no churn for un-numbered docs)
  (check-true (eq? (number-headings tree (make-hash)) tree))
  ;; delayed passes through
  (let ([d (delayed (lambda ([i : info]) ""))])
    (check-true (eq? (number-headings d nums) d)))

  ;; --- replace-tagged: substitution not re-scanned ---
  (define marked
    (element 'div '() (list (element '%m '((tt . "1")) '() #f)) #f))
  (check-equal?
   (replace-tagged marked '%m (lambda ([e : element]) (list "X" (element '%m '() '() #f))))
   (element 'div '() (list (list "X" (element '%m '() '() #f))) #f)))
