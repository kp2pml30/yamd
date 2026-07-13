#lang racket/base
;; Compile/render tests for examples/ (see examples/README.md). No goldens on
;; purpose: the examples evolve as showcases, so rendering success — every
;; backend, both build modes — is the contract, plus a few spot checks that
;; the interesting machinery (local plugin, includes, numbering) actually
;; fired. The raw-html example additionally asserts its DOCUMENTED failure:
;; raw 'html nodes are non-portable, so its typst render must error.

(module+ test
  (require rackunit
           racket/file
           racket/path
           racket/runtime-path
           yamd
           (only-in yamd/backend/markdown content->markdown)
           (only-in yamd/backend/typst content->typst)
           (only-in yamd/fmt format-yamd)
           ;; The book's local plugin ships its own unit tests; requiring the
           ;; test submodule runs them as part of this suite.
           (submod (file "../examples/book/ext/admonition.rkt") test))

  (define-runtime-path examples-dir "../examples")

  (define html-backend (build-backend 'html ".html" content->html))
  (define md-backend (build-backend 'markdown ".md" content->markdown))
  (define ty-backend (build-backend 'typst ".typ" content->typst))

  (define (build root mode #:backend [backend html-backend])
    (build-docset (build-path examples-dir root)
                  #:strategy (case mode
                               [(single) single-file-strategy]
                               [(multi) multi-file-strategy])
                  #:backend backend))

  (define (single-text root #:backend [backend html-backend])
    (define outs (build root 'single #:backend backend))
    (check-equal? (length outs) 1)
    (build-output-text (car outs)))

  ;; Write a multi-mode build into a tempdir; return the sorted relative
  ;; page paths that exist on disk.
  (define (write-multi root #:backend [backend html-backend])
    (define dir (make-temporary-file "yamd-examples-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (for ([o (in-list (build root 'multi #:backend backend))])
         (define p (build-path dir (build-output-path o)))
         (make-directory* (path-only p))
         (call-with-output-file p
           (lambda (out) (write-string (build-output-text o) out))))
       (sort (for/list ([p (in-directory dir)] #:when (file-exists? p))
               (path->string (find-relative-path dir p)))
             string<?))
     (lambda () (delete-directory/files dir))))

  ;; --- examples/book: all three backends + both modes --------------------

  (let ([out (single-text "book/index.yamd")])
    (check-true (positive? (string-length out)))
    ;; toctree numbering reached the inlined chapters
    (check-regexp-match #rx"<h2 id=\"[^\"]+\">1 Getting started</h2>" out)
    ;; the local ext/ plugin ran: admonition box + keys DSL
    (check-regexp-match #rx"admonition admonition-warning" out)
    (check-regexp-match #rx"<span class=\"keys\">" out)
    ;; context-scoped admonition default inside #fork-context
    (check-regexp-match #rx"admonition admonition-tip" out)
    ;; the #include'd colophon appears twice (index + last chapter)
    (check-equal? (length (regexp-match* #rx"shared fragment" out)) 2)
    ;; cross-file ref-before-def resolved
    (check-regexp-match #rx"href=\"#def-plan\"" out))

  (check-equal? (write-multi "book/index.yamd")
                '("extending.html" "index.html" "intro.html"
                  "scripting.html" "spec.html" "usage.html"))

  (check-true (positive? (string-length
                          (single-text "book/index.yamd" #:backend md-backend))))
  (check-true (positive? (string-length
                          (single-text "book/index.yamd" #:backend ty-backend))))

  (check-equal? (write-multi "book/index.yamd" #:backend md-backend)
                '("extending.md" "index.md" "intro.md"
                  "scripting.md" "spec.md" "usage.md"))
  (check-equal? (write-multi "book/index.yamd" #:backend ty-backend)
                '("extending.typ" "index.typ" "intro.typ"
                  "scripting.typ" "spec.typ" "usage.typ"))

  ;; --- examples/raw-html: html/markdown build; typst errors (documented) -

  (let ([out (single-text "raw-html/index.yamd")])
    (check-regexp-match #rx"<details>" out)
    (check-regexp-match #rx"<abbr title=" out))

  (check-equal? (write-multi "raw-html/index.yamd") '("index.html"))

  ;; markdown embeds raw HTML (CommonMark allows it), so this succeeds…
  (check-true (positive? (string-length
                          (single-text "raw-html/index.yamd" #:backend md-backend))))
  ;; …and typst refuses the foreign raw node rather than dropping content —
  ;; in both build modes.
  (check-exn #rx"raw node for backend html cannot be rendered to typst"
             (lambda () (build "raw-html/index.yamd" 'single #:backend ty-backend)))
  (check-exn #rx"raw node for backend html cannot be rendered to typst"
             (lambda () (build "raw-html/index.yamd" 'multi #:backend ty-backend)))

  ;; --- examples/control: host-language loops and conditionals -------------

  (let ([out (single-text "control.yamd")])
    ;; two 15-element loops → 30 <li>; the fizz/buzz cond and parity if fired
    (check-equal? (length (regexp-match* #rx"<li>" out)) 30)
    (check-regexp-match #rx"Item 15" out)
    (check-regexp-match #rx"even" out)
    (check-regexp-match #rx"odd" out)
    (check-regexp-match #rx"fizzbuzz" out)
    (check-regexp-match #rx"<strong>ready</strong>" out))

  ;; --- every example source is formatter-clean ----------------------------

  (for ([p (in-directory examples-dir)]
        #:when (and (file-exists? p) (equal? (path-get-extension p) #".yamd")))
    (define text (file->string p))
    (check-equal? (format-yamd text #:src p) text
                  (format "~a is not `--fmt --check` clean" p))))
