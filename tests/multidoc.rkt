#lang racket/base
;; Doc-set builder tests (docs/multi-doc.md). The golden runner maps one
;; .yamd to one expectation, so multi-output builds are tested here with
;; rackunit over fixture trees in tests/fixtures/multidoc/. Include-only
;; single-pipeline behavior is covered by tests/golden/multidoc/ goldens.

(module+ test
  (require rackunit
           racket/file
           racket/path
           racket/port
           racket/runtime-path
           racket/system
           yamd
           (only-in yamd/backend/markdown content->markdown)
           (only-in yamd/backend/typst content->typst))

  (define-runtime-path fixtures "fixtures/multidoc")

  (define html-backend (build-backend 'html ".html" content->html))
  (define md-backend (build-backend 'markdown ".md" content->markdown))
  (define ty-backend (build-backend 'typst ".typ" content->typst))

  (define (build fixture mode #:backend [backend html-backend])
    (build-docset (build-path fixtures fixture)
                  #:strategy (case mode
                               [(single) single-file-strategy]
                               [(multi) multi-file-strategy])
                  #:backend backend))

  ;; outputs -> hash path -> text
  (define (pages outputs)
    (for/hash ([o (in-list outputs)])
      (values (build-output-path o) (build-output-text o))))

  ;; --- single-file mode: inline children, shift headings ------------------

  (let* ([outs (build "book/index.yamd" 'single)]
         [out (build-output-text (car outs))])
    (check-equal? (length outs) 1)
    (check-equal? (build-output-path (car outs)) "index.html")
    ;; root at h1; children (toctree under #section{Book}, base depth 1)
    ;; shifted h1->h2; the grandchild (setup's toctree at ITS depth 1)
    ;; cumulatively h1->h3
    (check-regexp-match #rx"<h1 id=\"sec-1\">Book</h1>" out)
    (check-regexp-match #rx"<h2 id=\"sec-2\">Intro</h2>" out)
    (check-regexp-match #rx"<h2 id=\"sec-3\">Setup</h2>" out)
    (check-regexp-match #rx"<h3 id=\"sec-4\">Advanced</h3>" out)
    ;; inlining follows toctree order: Intro before Setup before Advanced
    (let ([i (regexp-match-positions #rx"Intro" out)]
          [s (regexp-match-positions #rx"Setup" out)])
      (check-true (< (caar i) (caar s))))
    ;; cross-file ref stays a local anchor
    (check-regexp-match #rx"<a id=\"ref-1\" href=\"#def-widget\">widget</a>" out)
    ;; #toc sees the SHIFTED section depths: Intro/Setup nested under Book,
    ;; Advanced nested under Setup
    (check-regexp-match
     #rx"<nav class=\"toc\"><ul><li><a href=\"#sec-1\">Book</a><ul><li><a href=\"#sec-2\">Intro</a></li><li><a href=\"#sec-3\">Setup</a><ul><li><a href=\"#sec-4\">Advanced</a></li></ul></li></ul></li></ul></nav>"
     out))

  ;; --- multi-file mode: pages, navs, cross-page links ---------------------

  (let* ([ps (pages (build "book/index.yamd" 'multi))])
    (check-equal? (sort (hash-keys ps) string<?)
                  '("guide/advanced.html" "guide/setup.html" "index.html" "intro.html"))
    (define index (hash-ref ps "index.html"))
    (define intro (hash-ref ps "intro.html"))
    (define setup (hash-ref ps "guide/setup.html"))
    (define advanced (hash-ref ps "guide/advanced.html"))
    ;; every page's own top section is h1 (standalone pages)
    (check-regexp-match #rx"<h1 id=\"sec-2\">Intro</h1>" intro)
    (check-regexp-match #rx"<h1 id=\"sec-3\">Setup</h1>" setup)
    ;; toctree marker became a nav; nested child pages recurse (no depth cap)
    (check-regexp-match
     #rx"<nav class=\"toctree\"><ul><li><a href=\"intro.html\">Intro</a></li><li><a href=\"guide/setup.html\">Setup</a><ul><li><a href=\"guide/advanced.html\">Advanced</a></li></ul></li></ul></nav>"
     index)
    ;; cross-page ref from the root page
    (check-regexp-match #rx"href=\"intro.html#def-widget\">widget</a>" index)
    ;; …and from a SUBDIRECTORY page (correct ../)
    (check-regexp-match #rx"href=\"../intro.html#def-widget\">widget</a>" setup)
    ;; #toc on the index page links into the other pages
    (check-regexp-match #rx"<a href=\"intro.html#sec-2\">Intro</a>" index)
    (check-regexp-match #rx"<a href=\"guide/setup.html#sec-3\">Setup</a>" index)
    ;; prev/next/up navigation (DFS order: index, intro, setup, advanced)
    (check-regexp-match #rx"<nav class=\"docnav\"><a class=\"next\" href=\"intro.html\">→ Intro</a></nav>" index)
    (check-regexp-match #rx"<a class=\"prev\" href=\"index.html\">← Book</a>" intro)
    (check-regexp-match #rx"<a class=\"up\" href=\"index.html\">↑ Book</a>" intro)
    (check-regexp-match #rx"<a class=\"next\" href=\"guide/setup.html\">→ Setup</a>" intro)
    (check-regexp-match #rx"<a class=\"prev\" href=\"setup.html\">← Setup</a>" advanced)
    (check-regexp-match #rx"<a class=\"up\" href=\"setup.html\">↑ Setup</a>" advanced)
    (check-false (regexp-match #rx"class=\"next\"" advanced)))

  ;; --- multi-file, markdown backend: extension adapts + link TARGETS exist -

  (let* ([ps (pages (build "book/index.yamd" 'multi #:backend md-backend))])
    (check-equal? (sort (hash-keys ps) string<?)
                  '("guide/advanced.md" "guide/setup.md" "index.md" "intro.md"))
    (define index (hash-ref ps "index.md"))
    (define intro (hash-ref ps "intro.md"))
    (define setup (hash-ref ps "guide/setup.md"))
    ;; the cross-page href (source side, already covered elsewhere)…
    (check-regexp-match #rx"\\.\\./intro\\.md#def-widget" setup)
    (check-regexp-match #rx"\\]\\(intro\\.md#def-widget\\)" index)
    (check-regexp-match #rx"\\]\\(intro\\.md#sec-2\\)" index)     ; #toc entry
    ;; …and the corresponding link TARGETS on the destination pages: a section
    ;; heading anchor and the xref def anchor (an inline-HTML <dfn> keeps its id)
    (check-regexp-match #rx"# <a id=\"sec-2\"></a>Intro" intro)
    (check-regexp-match #rx"<dfn id=\"def-widget\">widget</dfn>" intro)
    ;; a same-page toc entry resolves to the page's own heading anchor
    (check-regexp-match #rx"# <a id=\"sec-1\"></a>Book" index))

  ;; --- multi-file, typst backend: labels as link targets ------------------

  (let* ([ps (pages (build "book/index.yamd" 'multi #:backend ty-backend))])
    (check-equal? (sort (hash-keys ps) string<?)
                  '("guide/advanced.typ" "guide/setup.typ" "index.typ" "intro.typ"))
    (define index (hash-ref ps "index.typ"))
    (define intro (hash-ref ps "intro.typ"))
    (define setup (hash-ref ps "guide/setup.typ"))
    ;; destination pages carry invisible labelled anchors for their ids…
    (check-regexp-match #rx"#metadata\\(none\\)<sec-2>\n= Intro" intro)
    (check-regexp-match #rx"#metadata\\(none\\)<def-widget>#emph\\[widget\\]" intro)
    ;; a SAME-page fragment link resolves to a label (the #toc entry for the
    ;; current page); its target label is present on the page (so it compiles)
    (check-regexp-match #rx"#link\\(<sec-1>\\)\\[Book\\]" index)
    (check-regexp-match #rx"#metadata\\(none\\)<sec-1>\n= Book" index)
    ;; a CROSS-file fragment link stays a string link naming the right page
    ;; (typst's per-document labels can't be linked across files — documented)
    (check-regexp-match #rx"#link\\(\"intro\\.typ#def-widget\"\\)\\[widget\\]" index)
    (check-regexp-match #rx"#link\\(\"\\.\\./intro\\.typ#def-widget\"\\)\\[widget\\]" setup))

  ;; --- typst smoke-compile: every multi-file page must actually compile ----
  ;; Guarded on a `typst` binary being on PATH (skipped otherwise, e.g. under
  ;; the plain dev shell); proves the emitted labels/links are compile-safe —
  ;; in particular that same-page #link(<label>) targets all exist.

  (let ([typst-exe (find-executable-path "typst")])
    (when typst-exe
      (define dir (make-temporary-file "yamd-typst-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (for ([o (in-list (build "book/index.yamd" 'multi #:backend ty-backend))])
           (define p (build-path dir (build-output-path o)))
           (make-directory* (path-only p))
           (call-with-output-file p
             (lambda (out) (write-string (build-output-text o) out))
             #:exists 'replace))
         (for ([p (in-directory dir)] #:when (equal? (path-get-extension p) #".typ"))
           (define code
             (parameterize ([current-output-port (open-output-string)]
                            [current-error-port (open-output-string)])
               (system*/exit-code
                typst-exe "compile" "--root" (path->string dir)
                (path->string p) (path->string (path-replace-extension p #".pdf")))))
           (check-equal? code 0 (format "typst compile failed for ~a" p))))
       (lambda () (delete-directory/files dir)))))

  ;; --- depth: option limits multi-mode nav recursion ----------------------

  (let* ([ps (pages (build "depth/index.yamd" 'multi))]
         [index (hash-ref ps "index.html")])
    (check-regexp-match #rx"<a href=\"a.html\">A</a>" index)
    (check-false (regexp-match #rx"b\\.html" index))
    ;; …but B is still a page (depth limits the nav, not the set)
    (check-true (hash-has-key? ps "b.html")))

  ;; --- numbered: hierarchical section numbers (both modes) ----------------

  ;; single-file: child headings numbered (index's own #section{Guide} is NOT,
  ;; it is not part of the numbered toctree); #toc shows the same numbers.
  (let ([out (build-output-text (car (build "numbered/index.yamd" 'single)))])
    (check-regexp-match #rx"<h1 id=\"sec-1\">Guide</h1>" out)      ; unnumbered
    (check-regexp-match #rx"<h2 id=\"sec-2\">1 First</h2>" out)
    (check-regexp-match #rx"<h3 id=\"sec-3\">1.1 Details</h3>" out)
    (check-regexp-match #rx"<h2 id=\"sec-4\">2 Second</h2>" out)
    ;; #toc: numbers appear on the entries, nested by the merged tree
    (check-regexp-match
     #rx"<a href=\"#sec-2\">1 First</a><ul><li><a href=\"#sec-3\">1.1 Details</a>" out)
    (check-regexp-match #rx"<a href=\"#sec-4\">2 Second</a>" out))

  ;; multi-file: same numbers on each page's own headings, in the #toc, and in
  ;; the toctree nav (which now also lists the child pages' sub-sections).
  (let* ([ps (pages (build "numbered/index.yamd" 'multi))]
         [index (hash-ref ps "index.html")]
         [one (hash-ref ps "one.html")])
    (check-regexp-match #rx"<h1 id=\"sec-2\">1 First</h1>" one)
    (check-regexp-match #rx"<h2 id=\"sec-3\">1.1 Details</h2>" one)
    ;; nav: section-level entry (1.1 Details) nested under its page link
    (check-regexp-match
     #rx"<nav class=\"toctree\"><ul><li><a href=\"one.html\">1 First</a><ul><li><a href=\"one.html#sec-3\">1.1 Details</a></li></ul></li><li><a href=\"two.html\">2 Second</a></li></ul></nav>"
     index)
    ;; #toc: cross-page links carry the numbers and nest by the doc graph
    (check-regexp-match
     #rx"<a href=\"one.html#sec-2\">1 First</a><ul><li><a href=\"one.html#sec-3\">1.1 Details</a>" index)
    (check-regexp-match #rx"<a href=\"two.html#sec-4\">2 Second</a>" index))

  ;; numbers follow DOCUMENT order: a child page listed before a later sibling
  ;; section numbers ahead of it (1 A, 1.1 B, 2 Post) — in the merged page and
  ;; in the nav (child pages interleave at their marker site).
  (let ([out (build-output-text (car (build "numorder/index.yamd" 'single)))])
    (check-regexp-match #rx"<h1 id=\"sec-1\">1 A</h1>" out)
    (check-regexp-match #rx"<h1 id=\"sec-2\">1.1 B</h1>" out)
    (check-regexp-match #rx"<h1 id=\"sec-3\">2 Post</h1>" out)
    (let ([b (regexp-match-positions #rx"1.1 B" out)]
          [p (regexp-match-positions #rx"2 Post" out)])
      (check-true (< (caar b) (caar p)))))
  (let* ([ps (pages (build "numorder/index.yamd" 'multi))]
         [index (hash-ref ps "index.html")])
    (check-regexp-match
     #rx"<nav class=\"toctree\"><ul><li><a href=\"a.html\">1 A</a><ul><li><a href=\"b.html\">1.1 B</a></li><li><a href=\"a.html#sec-3\">2 Post</a></li></ul></li></ul></nav>"
     index)
    ;; numbered page titles also reach the prev/up/next crumbs
    (check-regexp-match #rx"<a class=\"up\" href=\"a.html\">↑ 1 A</a>"
                        (hash-ref ps "b.html")))

  ;; sibling numbered toctrees restart independently (sphinx parity)
  (let ([out (build-output-text (car (build "numsib/index.yamd" 'single)))])
    (check-regexp-match #rx"<h2 id=\"sec-2\">1 A</h2>" out)
    (check-regexp-match #rx"<h2 id=\"sec-3\">1 B</h2>" out))

  ;; a numbered page with children but no heading of its own: explicit error
  ;; (its children's numbers would have no parent number), both modes
  (check-exn #rx"mid\\.yamd is under a numbered toctree but has no section heading"
             (lambda () (build "numheadless/index.yamd" 'single)))
  (check-exn #rx"mid\\.yamd is under a numbered toctree but has no section heading"
             (lambda () (build "numheadless/index.yamd" 'multi)))

  ;; numbered:/hidden: must be booleans
  (check-exn #rx"numbered: must be a boolean"
             (lambda () (yamd-string->html "#use(std/toctree)\n#toctree(numbered: 1)\n\tx.yamd\n")))
  (check-exn #rx"hidden: must be a boolean"
             (lambda () (yamd-string->html "#use(std/toctree)\n#toctree(hidden: \"yes\")\n\tx.yamd\n")))

  ;; --- nested multi-file #toc reflects the doc graph -------
  ;; The un-numbered book set: multi-file #toc used to list every page's top
  ;; section flat; now Intro/Setup nest under Book and Advanced under Setup.
  (let* ([ps (pages (build "book/index.yamd" 'multi))]
         [index (hash-ref ps "index.html")])
    (check-regexp-match
     #rx"<nav class=\"toc\"><ul><li><a href=\"#sec-1\">Book</a><ul><li><a href=\"intro.html#sec-2\">Intro</a></li><li><a href=\"guide/setup.html#sec-3\">Setup</a><ul><li><a href=\"guide/advanced.html#sec-4\">Advanced</a></li></ul></li></ul></li></ul></nav>"
     index))

  ;; --- hidden: register children, emit no nav (multi); still inline (single) -

  ;; multi: the toctree emits nothing, but the child is still a navigable page.
  (let ([ps (pages (build "hidden/index.yamd" 'multi))])
    (check-equal? (sort (hash-keys ps) string<?) '("a.html" "index.html"))
    (check-false (regexp-match #rx"class=\"toctree\"" (hash-ref ps "index.html")))
    (check-regexp-match #rx"<a class=\"up\" href=\"index.html\">↑ Home</a>"
                        (hash-ref ps "a.html")))
  ;; single: content is still inlined (dropping it would lose the document)
  (check-regexp-match #rx"<h2 id=\"sec-2\">Alpha</h2>"
                      (build-output-text (car (build "hidden/index.yamd" 'single))))

  ;; --- nav depth: prunes section-level entries too ------------------------
  ;; depth:1 shows only the page title (level 1), not its Sub section (level 2).
  (let* ([ps (pages (build "navdepth/index.yamd" 'multi))]
         [index (hash-ref ps "index.html")])
    (check-regexp-match
     #rx"<nav class=\"toctree\"><ul><li><a href=\"page.html\">Page</a></li></ul></nav>" index)
    (check-false (regexp-match #rx"Sub" index))
    (check-true (hash-has-key? ps "page.html")))

  ;; --- single-mode parity with the plain pipeline -------------------------

  ;; A document without #toctree builds byte-identically to yamd-file->html.
  (let ([doc (simplify-path
              (build-path fixtures 'up 'up "golden" "multidoc" "include-basic.yamd"))])
    (check-equal? (build-output-text
                   (car (build-docset doc
                                      #:strategy single-file-strategy
                                      #:backend html-backend)))
                  (yamd-file->html doc)))

  ;; --- strategies do not mutate the shared docset --------------------------

  ;; One evaluated docset, several builds: single twice (idempotent — the
  ;; 'sections shift is applied to a restored-afterwards copy), then multi,
  ;; which must match a fresh multi build.
  (let ([ds (evaluate-docset (build-path fixtures "book/index.yamd"))])
    (define (texts strategy)
      (map build-output-text ((build-strategy-build strategy) ds html-backend)))
    (define first-single (texts single-file-strategy))
    (check-equal? (texts single-file-strategy) first-single)
    (check-equal? (texts multi-file-strategy)
                  (map build-output-text (build "book/index.yamd" 'multi)))
    (check-equal? (texts single-file-strategy) first-single))

  ;; --- #toctree inside an #include'd fragment -------------------------------

  ;; The toctree lands in index's page (current-docset-parent, not the
  ;; included file), under #section{Top} (base depth 1 via the include's
  ;; context fork).
  (let ([out (build-output-text (car (build "incbook/index.yamd" 'single)))])
    (check-regexp-match #rx"<h1 id=\"sec-1\">Top</h1>" out)
    (check-regexp-match #rx"<h2 id=\"sec-2\">Leaf</h2>" out))
  (let ([ps (pages (build "incbook/index.yamd" 'multi))])
    ;; the included nav.yamd is part of index's page, NOT a page of its own
    (check-equal? (sort (hash-keys ps) string<?) '("index.html" "leaf.html"))
    (check-regexp-match #rx"<a class=\"up\" href=\"index.html\">↑ Top</a>"
                        (hash-ref ps "leaf.html"))
    (check-regexp-match #rx"<nav class=\"toctree\"><ul><li><a href=\"leaf.html\">Leaf</a></li></ul></nav>"
                        (hash-ref ps "index.html")))

  ;; --- the same file #include'd twice sequentially is legal -----------------

  (let ([out (build-output-text (car (build "twice/index.yamd" 'single)))])
    (check-equal? (length (regexp-match* #rx"Repeated part\\." out)) 2))

  ;; --- two-pass content in a section title ----------------------------------

  ;; In the page itself the title resolves fine (single mode)…
  (check-regexp-match #rx"About the <a id=\"ref-1\" href=\"#def-w\">widget</a>"
                      (build-output-text (car (build "reftitle/index.yamd" 'single))))
  ;; …but it cannot be reused as nav link text on other pages (multi mode):
  ;; explicit error naming the document, not a contract violation.
  (check-exn #rx"section title of .*child\\.yamd contains two-pass content"
             (lambda () (build "reftitle/index.yamd" 'multi)))

  ;; --- error paths ---------------------------------------------------------

  ;; two sources mapping to one output file (extension swap collision)
  (check-exn #rx"documents .*ch\\.(yamd|md) and .*ch\\.(yamd|md) both map to output ch\\.html"
             (lambda () (build "coll/index.yamd" 'multi)))
  ;; …single-file mode does not place outputs per source, so it builds
  (check-not-exn (lambda () (build "coll/index.yamd" 'single)))

  ;; a symlink alias of a listed file is the SAME document (identity is the
  ;; symlink-resolved path), so re-listing it through `sub -> .` errors
  (check-exn #rx"already in the document set"
             (lambda () (build "alias/index.yamd" 'single)))

  ;; include cycle: chain named in the error
  (check-exn #rx"include: document cycle: .*a\\.yamd -> .*b\\.yamd -> .*a\\.yamd"
             (lambda () (build "cycle/a.yamd" 'single)))

  ;; a document may appear in only one toctree
  (check-exn #rx"toctree: document .*child\\.yamd is already in the document set; .*dup/index\\.yamd lists it again"
             (lambda () (build "dup/index.yamd" 'single)))

  ;; missing include target (string-fed doc: resolves against cwd)
  (check-exn #rx"include: no such document: \"missing-nope.yamd\""
             (lambda ()
               (yamd-string->html "#use(std/include)\n#include(\"missing-nope.yamd\")\n")))

  ;; single-mode inlining cannot shift a child's sections past h6
  (check-exn #rx"past h6"
             (lambda () (build "deep/index.yamd" 'single)))
  ;; …but the same set is fine multi-file (child page starts at h1)
  (check-not-exn (lambda () (build "deep/index.yamd" 'multi)))

  ;; a toctree child outside the root's directory has no output slot
  (check-exn #rx"outside the root document's directory"
             (lambda () (build "escape/sub/index.yamd" 'multi)))
  ;; …single-file mode has no output paths to place, so it still builds
  (check-regexp-match #rx"<h1 id=\"sec-1\">Outside</h1>"
                      (build-output-text (car (build "escape/sub/index.yamd" 'single))))

  ;; unbuilt toctree document through the plain pipeline: loud %-tag error
  (check-exn #rx"internal element %toctree leaked"
             (lambda () (yamd-file->html (build-path fixtures "depth/index.yamd"))))

  ;; --- CLI: --mode end to end ----------------------------------------------

  (define racket-exe (find-system-path 'exec-file))
  (define (run-cli . args)
    (define out (open-output-string))
    (define err (open-output-string))
    (define code
      (parameterize ([current-output-port out]
                     [current-error-port err])
        (apply system*/exit-code racket-exe "-l-" "yamd/cli" args)))
    (values (get-output-string out) (get-output-string err) code))

  (define book-index (path->string (build-path fixtures "book/index.yamd")))

  ;; default mode is single: one document on stdout
  (let-values ([(out err code) (run-cli book-index)])
    (check-equal? code 0)
    (check-equal? err "")
    (check-regexp-match #rx"<h2 id=\"sec-2\">Intro</h2>" out))

  ;; --mode multi writes one page per source under -o
  (let ([dir (make-temporary-file "yamd-multidoc-~a" 'directory)])
    (dynamic-wind
     void
     (lambda ()
       (let-values ([(out err code)
                     (run-cli book-index "--mode" "multi" "-o" (path->string dir))])
         (check-equal? code 0)
         (check-equal? err ""))
       (define written
         (sort (for/list ([p (in-directory dir)] #:when (file-exists? p))
                 (path->string (find-relative-path dir p)))
               string<?))
       (check-equal? written
                     '("guide/advanced.html" "guide/setup.html" "index.html" "intro.html"))
       (check-regexp-match #rx"href=\"\\.\\./intro\\.html#def-widget\""
                           (file->string (build-path dir "guide/setup.html"))))
     (lambda () (delete-directory/files dir))))

  ;; --mode multi without -o, and unknown mode: clean errors
  (let-values ([(out err code) (run-cli book-index "--mode" "multi")])
    (check-equal? code 1)
    (check-regexp-match #rx"requires -o" err))
  (let-values ([(out err code) (run-cli book-index "--mode" "pages")])
    (check-equal? code 1)
    (check-regexp-match #rx"unknown mode \"pages\"; known modes: multi, single" err)))
