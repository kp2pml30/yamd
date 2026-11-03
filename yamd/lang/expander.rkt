#lang racket/base
;; Module language for `#lang yamd` documents. `yamd/lang/reader` emits
;;
;;   (module <name> yamd/lang/expander <doc-name> <source-text>)
;;
;; and this module's `#%module-begin` turns the captured source into a resolved
;; document. It `provide`s `doc` (resolved + decoded Content) and defines a
;; `main` submodule that renders it to HTML on stdout — so `racket file.rkt`
;; prints the document. Because the work happens at instantiation time via
;; `yamd-string->content`, the whole `eval.rkt` pipeline (fresh namespace,
;; #use, contexts, two-pass) is reused verbatim; see docs/architecture.md for the
;; deliberate tradeoff (the document is re-parsed/eval'd at run time, not
;; compiled to Racket).

(require (for-syntax racket/base)
         yamd/eval
         yamd/backend/html)

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [yamd-module-begin #%module-begin])
         content->html
         yamd-string->content
         doc-src)

;; The reader passes either the original path (string) or the fallback symbol.
(define (doc-src name)
  (if (string? name) name (or name 'yamd-document)))

(define-syntax (yamd-module-begin stx)
  (syntax-case stx ()
    [(_ doc-name source-text)
     #'(#%module-begin
        (define doc
          (let ([run (lambda ()
                       (yamd-string->content source-text #:src (doc-src doc-name)))])
            ;; a real file: relative #include / #image paths resolve next to it
            (if (string? doc-name)
                (call-with-document-source doc-name run)
                (run))))
        (provide doc)
        (module* main #f
          (display (content->html doc))
          (newline)))]))
