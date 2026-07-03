#lang racket/base
;; `#lang yamd` reader. Found by the module-reader machinery at
;; `yamd/lang/reader` for a file that begins with `#lang yamd`.
;;
;; Pragmatic path (see docs/architecture.md "#lang yamd"): rather than embedding the
;; indentation reader's forms into the compiled module — which cannot work,
;; because `#use(...)` performs a runtime `namespace-require` that only makes
;; sense in the fresh document namespace `eval.rkt` builds — we capture the raw
;; document text verbatim and hand it to the ordinary string pipeline
;; (`yamd-string->content`) at module-instantiation time. `#use`, plugins,
;; contexts and two-pass therefore behave identically to `yamd-string->html`.
;;
;; Srclocs: the port handed to us sits right after `#lang yamd`, so the leading
;; newline is still present; capturing from here keeps line/column numbers
;; aligned with the original file (the newline places the first content line on
;; line 2 in both the file and the captured string).

(require racket/port)

(provide (rename-out [yamd-lang-read read]
                     [yamd-lang-read-syntax read-syntax])
         get-info)

(define (yamd-lang-read in)
  (syntax->datum (yamd-lang-read-syntax #f in)))

(define (yamd-lang-read-syntax src in)
  (define text (port->string in))
  (datum->syntax
   #f
   `(module ,(src->module-name src) yamd/lang/expander
      ,(src->doc-name src)
      ,text)))

;; Module name derived from the file name (Racket needs a symbol here); falls
;; back to a fixed symbol when there is no path (e.g. reading from a string).
(define (src->module-name src)
  (cond
    [(path? src) (path->module-name src)]
    [(and (string? src) (not (eq? src 'string))) (path->module-name (string->path src))]
    [else 'yamd-document]))

(define (path->module-name p)
  (define-values (base name dir?) (split-path p))
  (string->symbol
   (path->string (path-replace-extension name #""))))

;; The srcloc `source` value threaded into the pipeline: keep the original
;; path (as a string, so it survives as a syntax datum), else a stable symbol.
(define (src->doc-name src)
  (cond
    [(path? src) (path->string src)]
    [(and (string? src) (not (eq? src 'string))) src]
    [else 'yamd-document]))

;; Minimal language info: default reader parameters. Kept deliberately small;
;; DrRacket features (indentation, coloring) can hang off this later.
(define (get-info in mod line col pos)
  (lambda (key default) default))
