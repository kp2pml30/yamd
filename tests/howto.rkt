#lang racket/base
;; Compile/render tests for the docs/lang/howto doc-set (dogfooded .yamd
;; guides). Like tests/examples.rkt: no goldens — the guides evolve, so
;; rendering success in both build modes plus formatter-cleanliness is the
;; contract.

(module+ test
  (require rackunit
           racket/file
           racket/path
           racket/runtime-path
           yamd
           (only-in yamd/fmt format-yamd))

  (define-runtime-path howto-dir "../docs/lang/howto")
  (define-runtime-path howto-index "../docs/lang/howto/index.yamd")

  (define html-backend (build-backend 'html ".html" content->html))

  ;; Pages = every non-auxiliary .yamd (aux `_`-prefixed sources are spliced
  ;; via #include, not built).
  (define page-count
    (for/sum ([p (in-directory howto-dir)]
              #:when (and (file-exists? p)
                          (equal? (path-get-extension p) #".yamd")
                          (not (regexp-match? #rx"^_" (path->string (file-name-from-path p))))))
      1))

  (test-case "howto renders single-file"
    (define outs (build-docset howto-index
                               #:strategy single-file-strategy
                               #:backend html-backend))
    (check-equal? (length outs) 1)
    (check-true (positive? (string-length (build-output-text (car outs))))))

  (test-case "howto renders multi-file, one page per source"
    (define outs (build-docset howto-index
                               #:strategy multi-file-strategy
                               #:backend html-backend))
    (check-equal? (length outs) page-count)
    (check-true (for/or ([o (in-list outs)])
                  (equal? (build-output-path o) "index.html"))))

  (test-case "howto sources are formatter-clean"
    (for ([p (in-directory howto-dir)]
          #:when (and (file-exists? p) (equal? (path-get-extension p) #".yamd")))
      (define text (file->string p))
      (check-equal? (format-yamd text #:src p) text
                    (format "~a is not formatter-clean" p)))))
