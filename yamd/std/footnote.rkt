#lang racket/base
;; Footnotes — a second two-pass showcase (see docs/architecture.md "Two-pass
;; evaluation"), mirroring std/xref.rkt. A plugin: nothing in core requires
;; it; documents get it via `#use(std/footnote)`.
;;
;;   #note{text}   emit a numbered superscript marker linking to the note,
;;                 and RECORD the note body at collect time (pass 1)
;;   #notes        delayed: render the numbered list of recorded notes, each
;;                 with a backlink to its marker
;;
;; Numbering is document order: `#note` allocates its number at collect time,
;; so markers read 1, 2, 3, … in the order they appear in the source. Because
;; `#notes` is a DELAYED node resolved in pass 2 (after the whole document has
;; been collected), it does not matter where `#notes` sits relative to the
;; notes: a `#note` appearing AFTER `#notes` is still collected in pass 1 and
;; still shows up in the list, in its document-order position. That
;; order-independence is the two-pass point.
;;
;; Decisions (documented + tested):
;;  - Zero notes: a `#notes` with no `#note`s in the document renders an
;;    EMPTY `<ol class="footnotes">` (not an error) — an empty footnote list
;;    is a benign, meaningful thing, exactly like an empty `#toc`/`#glossary`.
;;  - Two `#notes`: an explicit error. A second numbered list would re-emit
;;    every note's `id` and backlink anchor, producing duplicate element ids
;;    (invalid HTML, ambiguous fragment links). Rather than silently pick one,
;;    we reject it at collect time (so eval.rkt prefixes the srcloc).

(require "../runtime.rkt")

(provide note notes)

;; Info keys.
;;  'footnote-notes          : entries accumulated newest-first via info-add!;
;;                             reverse for document order. Entry = an `fn`
;;                             struct (below).
;;  'yamd-footnote-counter   : per-document note counter (single-valued;
;;                             yamd- prefix = private, per base/xref).
;;  'yamd-footnote-emitted?  : set once #notes has run, to reject a second.
(struct fn (num note-anchor ref-anchor bodies) #:transparent)

;; ---------------------------------------------------------------------------
;; #note — collect the body, emit the marker.

;; A superscript marker: <sup id="fn-ref-N"><a href="#fn-note-N">N</a></sup>.
;; The <sup> id is the backlink target the note's ↩ link points at; the <a>
;; jumps down to the note body.
(define (note . bodies)
  (define i (current-info))
  (cond
    [i
     (define n (allocate-note-number i))
     (define ref-anchor (string-append "fn-ref-" (number->string n)))
     (define note-anchor (string-append "fn-note-" (number->string n)))
     (info-add! i 'footnote-notes (fn n note-anchor ref-anchor bodies))
     (footnote-marker n ref-anchor note-anchor)]
    ;; No info table (a unit test calling note directly without a collect
    ;; pass): still emit a well-formed marker with a unique anchor rather than
    ;; crashing, mirroring section/ref. Numbering is meaningless here.
    [else
     (define tag (symbol->string (gensym 'fn-)))
     (footnote-marker 1 (string-append "fn-ref-" tag) (string-append "fn-note-" tag))]))

(define (allocate-note-number i)
  (define n (add1 (info-ref i 'yamd-footnote-counter 0)))
  (info-set! i 'yamd-footnote-counter n)
  n)

(define (footnote-marker n ref-anchor note-anchor)
  (elem 'sup #:attrs (list (cons 'id ref-anchor) (cons 'class "footnote-ref"))
        (elem 'a #:attrs (list (cons 'href (string-append "#" note-anchor)))
              (number->string n))))

;; ---------------------------------------------------------------------------
;; #notes — delayed: the numbered list, in document order, with backlinks.

(define (notes)
  (define i (current-info))
  (when i
    (when (info-ref i 'yamd-footnote-emitted? #f)
      (error 'notes
             "only one #notes is allowed per document; a second footnote list would duplicate note ids and backlink anchors"))
    (info-set! i 'yamd-footnote-emitted? #t))
  (delayed
   (lambda (info)
     (define entries (reverse (info-ref info 'footnote-notes '())))
     ;; Zero notes -> empty <ol> (documented: benign, not an error).
     (apply elem 'ol #:attrs '((class . "footnotes"))
            (map footnote-item entries)))))

;; <li id="fn-note-N">body… <a class="footnote-backref" href="#fn-ref-N">↩</a></li>
(define (footnote-item e)
  (apply elem 'li
         #:attrs (list (cons 'id (fn-note-anchor e)))
         (append (fn-bodies e)
                 (list " "
                       (elem 'a
                             #:attrs (list (cons 'href (string-append "#" (fn-ref-anchor e)))
                                           (cons 'class "footnote-backref"))
                             "↩")))))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; note returns a <sup> marker and records the body at collect time.
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (define m1 (note (list "first note")))
      (define m2 (note (list "second note")))
      ;; markers number in document order, with paired ref/note anchors
      (check-equal?
       m1
       (elem 'sup #:attrs '((id . "fn-ref-1") (class . "footnote-ref"))
             (elem 'a #:attrs '((href . "#fn-note-1")) "1")))
      (check-equal?
       m2
       (elem 'sup #:attrs '((id . "fn-ref-2") (class . "footnote-ref"))
             (elem 'a #:attrs '((href . "#fn-note-2")) "2")))
      ;; both recorded (newest-first)
      (check-equal? (length (info-ref i 'footnote-notes '())) 2)))

  ;; notes resolves to a numbered <ol> with backlinks, in document order.
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (note (list "alpha"))
      (note (list "beta"))
      (define n (notes))
      (check-pred delayed? n)
      (check-equal?
       ((delayed-proc n) i)
       (elem 'ol #:attrs '((class . "footnotes"))
             (elem 'li #:attrs '((id . "fn-note-1"))
                   (list "alpha") " "
                   (elem 'a #:attrs '((href . "#fn-ref-1") (class . "footnote-backref")) "↩"))
             (elem 'li #:attrs '((id . "fn-note-2"))
                   (list "beta") " "
                   (elem 'a #:attrs '((href . "#fn-ref-2") (class . "footnote-backref")) "↩"))))))

  ;; order independence: a note collected AFTER notes runs still appears.
  ;; (In a real document #notes is delayed, so pass-1 order is what matters;
  ;; here we call the note after building the delayed proc but resolve later.)
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (note (list "early"))
      (define n (notes))          ; #notes emitted here…
      (note (list "late"))        ; …a later #note still gets collected
      (define ol ((delayed-proc n) i))
      (check-equal? (length (element-children ol)) 2)
      (check-equal? (element-attrs (car (element-children ol))) '((id . "fn-note-1")))
      (check-equal? (element-attrs (cadr (element-children ol))) '((id . "fn-note-2")))))

  ;; zero notes: empty <ol>, not an error.
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (define n (notes))
      (check-equal? ((delayed-proc n) i)
                    (elem 'ol #:attrs '((class . "footnotes"))))))

  ;; two #notes is an explicit collect-time error.
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (notes)
      (check-exn #rx"only one #notes is allowed" (lambda () (notes))))))
