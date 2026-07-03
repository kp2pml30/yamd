#lang racket/base
;; Golden test runner. Convention: tests/golden/<area>/<name>.yamd with a
;; sibling <name>.html (rendered output) and/or <name>.sexp (resolved IR via
;; content->sexp). Bless/update goldens with:
;;   YAMD_BLESS=1 nix develop --command raco test tests/run-golden.rkt
;; or
;;   nix develop --command racket tests/run-golden.rkt --bless
;; A .yamd without any expected file is an error, not a skip.

(require racket/file
         racket/path
         racket/pretty
         racket/runtime-path
         yamd)

(provide golden-cases compute-actual bless-all! case-name)

(define-runtime-path golden-root "golden")

;; A `_`-prefixed .yamd is an auxiliary source (e.g. a file #include'd by a
;; test root, sphinx-style), not a case of its own — it needs no expectation
;; and is never rendered directly.
(define (auxiliary? p)
  (regexp-match? #rx"^_" (path->string (file-name-from-path p))))

(define (find-yamd-files)
  (sort
   (for/list ([p (in-directory golden-root)]
              #:when (and (file-exists? p)
                          (equal? (path-get-extension p) #".yamd")
                          (not (auxiliary? p))))
     p)
   string<?
   #:key path->string))

(define (case-name p)
  (path->string (find-relative-path golden-root p)))

;; Extra backends: expected-file extension -> (module-path function).
;; The function takes what yamd-file->content returns (resolved content) and
;; returns the serialized document string (no trailing newline; we add one).
;; dynamic-require keeps the runner loadable while a backend is still absent.
(define backend-renderers
  (hash #".md" '(yamd/backend/markdown content->markdown)
        #".typ" '(yamd/backend/typst content->typst)))

(define expected-extensions
  (append '(#".html" #".sexp") (hash-keys backend-renderers)))

;; -> (listof (list yamd-path expected-path))
(define (golden-cases)
  (apply
   append
   (for/list ([y (in-list (find-yamd-files))])
     (define expected
       (filter file-exists?
               (for/list ([ext (in-list expected-extensions)])
                 (path-replace-extension y ext))))
     (when (null? expected)
       (error 'run-golden
              "~a has no expected file (one of ~a); create one (may be empty) and bless"
              (case-name y)
              expected-extensions))
     (for/list ([e (in-list expected)])
       (list y e)))))

(define (compute-actual yamd-path ext)
  (cond
    [(equal? ext #".html")
     (string-append (yamd-file->html yamd-path) "\n")]
    [(equal? ext #".sexp")
     (string-append
      (pretty-format (content->sexp (yamd-file->content yamd-path)) #:mode 'write)
      "\n")]
    [(hash-ref backend-renderers ext #f)
     => (lambda (renderer)
          (define render (dynamic-require (car renderer) (cadr renderer)))
          (string-append (render (yamd-file->content yamd-path)) "\n"))]
    [else (error 'run-golden "unknown golden kind: ~a" ext)]))

(define (bless-all!)
  (for ([c (in-list (golden-cases))])
    (define expected (cadr c))
    (define actual (compute-actual (car c) (path-get-extension expected)))
    (display-to-file actual expected #:exists 'replace)
    (printf "blessed ~a\n" (case-name expected))))

(define (bless-env?)
  (and (getenv "YAMD_BLESS") #t))

(module+ test
  (require rackunit)
  (cond
    [(bless-env?) (bless-all!)]
    [else
     ;; test-case isolates each golden: one broken document fails its own
     ;; case instead of aborting the whole suite.
     (for ([c (in-list (golden-cases))])
       (define yamd-path (car c))
       (define expected (cadr c))
       (test-case (case-name expected)
         (check-equal? (compute-actual yamd-path (path-get-extension expected))
                       (file->string expected)
                       (case-name expected))))]))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (cond
    [(member "--bless" args) (bless-all!)]
    [else
     (printf "run tests via: raco test tests/run-golden.rkt\n")
     (printf "bless goldens: racket tests/run-golden.rkt --bless\n")]))
