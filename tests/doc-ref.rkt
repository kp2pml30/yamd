#lang racket/base
;; #doc-ref end-to-end: a link to another DOCUMENT of the set resolves to the
;; target's title anchor, routed per build mode (in-page in single-file, a
;; cross-page `page.ext#anchor` in multi-file — via the existing #anchor
;; rewriter). Hermetic: builds a two-file set in a temp directory.

(module+ test
  (require rackunit
           racket/file
           racket/path
           racket/runtime-path
           yamd)

  (define hb (build-backend 'html ".html" content->html))

  (define-runtime-path lang-index "../docs/lang/index.yamd")

  ;; The shipped docs: the tables guide's live #doc-ref resolves to a cross-page
  ;; link to the pipe-tables page (and the whole tree builds).
  (test-case "shipped docs: a live #doc-ref resolves cross-set"
    (define outs (build-docset lang-index #:strategy multi-file-strategy #:backend hb))
    (define tables
      (for/first ([o (in-list outs)]
                  #:when (equal? (build-output-path o) "stdlib/elements/tables.html")) o))
    (check-true (and tables #t) "stdlib/elements/tables.html was produced")
    (check-regexp-match
     #px"<a href=\"[^\"]*pipe-tables\\.html#[^\"]+\">pipe-tables page</a>"
     (build-output-text tables)))

  ;; Build a temp doc-set from the given sources, hand the root path to `proc`.
  (define (with-set root-body sub-body proc)
    (define dir (make-temporary-file "yamd-docref-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (display-to-file root-body (build-path dir "root.yamd") #:exists 'replace)
       (display-to-file sub-body  (build-path dir "sub.yamd")  #:exists 'replace)
       (proc (build-path dir "root.yamd")))
     (lambda () (delete-directory/files dir))))

  (define root-src
    (string-append
     "#use(std/xref)\n"
     "#use(std/toctree)\n\n"
     "#section{Root}::\n\n"
     "See #doc-ref(\"sub\"){the sub page} and #doc-ref(\"sub\").\n\n"
     "#toctree\n"
     "\tsub.yamd\n"))
  (define sub-src "#section{Sub Page}::\n\nBody.\n")

  ;; multi: a cross-page link to sub.html#<anchor>, custom and default text.
  (test-case "doc-ref multi-file: cross-page link to the target's anchor"
    (with-set root-src sub-src
      (lambda (root)
        (define outs (build-docset root #:strategy multi-file-strategy #:backend hb))
        (define idx (for/first ([o (in-list outs)]
                                #:when (equal? (build-output-path o) "root.html")) o))
        (check-true (and idx #t) "root.html was produced")
        (define html (build-output-text idx))
        (check-regexp-match #px"<a href=\"sub\\.html#[^\"]+\">the sub page</a>" html)
        ;; default link text = the target document's title
        (check-regexp-match #px"<a href=\"sub\\.html#[^\"]+\">Sub Page</a>" html)
        (check-true (for/or ([o (in-list outs)]) (equal? (build-output-path o) "sub.html"))
                    "sub.html was produced"))))

  ;; single: the whole set is one page, so the doc-ref is an in-page anchor.
  (test-case "doc-ref single-file: in-page anchor"
    (with-set root-src sub-src
      (lambda (root)
        (define one (car (build-docset root #:strategy single-file-strategy #:backend hb)))
        (check-regexp-match #px"<a href=\"#[^\"]+\">the sub page</a>"
                            (build-output-text one)))))

  ;; --root shifts the id namespace: a path written for the default root no
  ;; longer matches when the base is pinned to the parent directory.
  (test-case "doc-ref: --root changes the path namespace"
    (with-set root-src sub-src
      (lambda (root)
        (check-exn
         #rx"dangling document reference"
         (lambda ()
           (build-docset root #:strategy single-file-strategy #:backend hb
                         #:root-dir (simple-form-path
                                     (build-path (path-only root) 'up))))))))

  ;; dangling: #doc-ref to a document not in the set is a build error.
  (test-case "doc-ref to a missing document errors"
    (with-set
     "#use(std/xref)\n\n#section{Root}::\n\nSee #doc-ref(\"nope\").\n"
     sub-src
     (lambda (root)
       (check-exn #rx"dangling document reference"
                  (lambda ()
                    (build-docset root #:strategy single-file-strategy
                                  #:backend hb)))))))
