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
               (run "#use(std/xref)\n\n#def(42){t}{d}\n")))

  ;; Duplicate #def WITHIN one #xref-scope is an error (an id in an inner
  ;; scope shadowing an outer one is fine; two in the SAME scope are not).
  (check-exn #rx"duplicate #def for id .* in this #xref-scope"
             (lambda ()
               (run (string-append "#use(std/xref)\n\n"
                                   "#xref-scope\n"
                                   "\t#def(\"x\"){one}{a}\n"
                                   "\t#def(\"x\"){two}{b}\n"))))

  ;; The same id at root and inside a scope is NOT a duplicate — the inner one
  ;; shadows. This must render without error.
  (check-not-exn
   (lambda ()
     (run (string-append "#use(std/xref)\n\n"
                         "#def(\"x\"){root}{a}\n"
                         "#xref-scope\n"
                         "\t#def(\"x\"){scoped}{b}\n"
                         "\tsee #ref{x}\n"))))

  ;; #glossary(scope:) naming a scope that does not exist is a resolve-time
  ;; error.
  (check-exn #rx"no #xref-scope with that name"
             (lambda ()
               (run "#use(std/xref)\n\n#glossary(scope: \"nope\")\n")))

  ;; Two #xref-scope with the same name collide (collect time).
  (check-exn #rx"duplicate #xref-scope name"
             (lambda ()
               (run (string-append "#use(std/xref)\n\n"
                                   "#xref-scope(name: \"a\")\n\ttext\n"
                                   "#xref-scope(name: \"a\")\n\tmore\n"))))

  ;; #ref{...} with a non-plain-text id body is an error.
  (check-exn #rx"id must be plain text"
             (lambda ()
               (run "#use(std/xref)\n\n#ref{#emph{x}}\n"))))
