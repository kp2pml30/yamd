#lang racket/base
;; Compile/render + formatter-clean tests for the whole docs/lang doc-set
;; (dogfooded .yamd guides + spec). No goldens — the guides evolve, so building
;; in both modes plus formatter-cleanliness is the contract. Every non-auxiliary
;; .yamd under docs/lang must be reachable from the index (a page in the set).

(module+ test
  (require rackunit
           racket/file
           racket/path
           racket/runtime-path
           yamd
           (only-in yamd/fmt format-yamd))

  (define-runtime-path docs-dir "../docs/lang")
  (define-runtime-path docs-index "../docs/lang/index.yamd")

  (define html-backend (build-backend 'html ".html" content->html))

  ;; Pages = every non-auxiliary .yamd (aux `_`-prefixed sources are spliced
  ;; via #include, not built as their own page).
  (define page-count
    (for/sum ([p (in-directory docs-dir)]
              #:when (and (file-exists? p)
                          (equal? (path-get-extension p) #".yamd")
                          (not (regexp-match? #rx"^_"
                                              (path->string (file-name-from-path p))))))
      1))

  (test-case "docs render single-file"
    (define outs (build-docset docs-index
                               #:strategy single-file-strategy
                               #:backend html-backend))
    (check-equal? (length outs) 1)
    (check-true (positive? (string-length (build-output-text (car outs))))))

  (test-case "docs render multi-file, one page per source"
    (define outs (build-docset docs-index
                               #:strategy multi-file-strategy
                               #:backend html-backend))
    ;; every non-aux .yamd is reachable from the index (a page of the set)
    (check-equal? (length outs) page-count)
    (check-true (for/or ([o (in-list outs)])
                  (equal? (build-output-path o) "index.html"))))

  (test-case "docs sources are formatter-clean"
    (for ([p (in-directory docs-dir)]
          #:when (and (file-exists? p) (equal? (path-get-extension p) #".yamd")))
      (define text (file->string p))
      (check-equal? (format-yamd text #:src p) text
                    (format "~a is not formatter-clean" p)))))
