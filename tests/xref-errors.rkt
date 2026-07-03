#lang racket/base
;; Error paths for the xref plugin. Success cases live in the
;; golden .html files; here we assert the located failures.

(module+ test
  (require rackunit
           yamd)

  (define (run str)
    (yamd-string->html str))

  ;; A dangling #ref (no matching #def anywhere in the document) is an error,
  ;; not empty text. It surfaces at resolve time, naming the id.
  (check-exn #rx"dangling cross-reference"
             (lambda ()
               (run "#use(std/xref)\n\nsee #ref(\"ghost\") here\n")))

  ;; Two #def with the same id is a duplicate-definition error (collect time,
  ;; so it carries the offending form's srcloc via eval.rkt).
  (check-exn #rx"duplicate #def"
             (lambda ()
               (run (string-append "#use(std/xref)\n\n"
                                   "#def(\"x\"){one}{a}\n"
                                   "#def(\"x\"){two}{b}\n"))))

  ;; A non-string/symbol id is rejected.
  (check-exn #rx"must be a symbol or string"
             (lambda ()
               (run "#use(std/xref)\n\n#def(42){t}{d}\n"))))
