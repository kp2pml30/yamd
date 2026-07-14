#lang racket/base
;; Tests for the `#lang yamd` module language (yamd/lang/reader +
;; yamd/lang/expander). Fixtures live in tests/lang/*.rkt (real `#lang yamd`
;; modules — NOT under tests/golden/, whose runner globs *.yamd). Each is
;; exercised two ways: dynamic-require its `doc` binding and render it, and run
;; its `main` submodule capturing stdout (the `racket file.rkt` path).

(require rackunit
         racket/runtime-path
         yamd/backend/html)

(define-runtime-path lang-dir "lang")

(define (fixture name)
  (build-path lang-dir name))

;; Render the provided `doc` of a `#lang yamd` fixture to HTML.
(define (doc->html name)
  (content->html (dynamic-require (fixture name) 'doc)))

;; Instantiate the fixture's `main` submodule, capturing what it prints.
(define (main-output name)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (dynamic-require (list 'submod (fixture name) 'main) #f))
  (get-output-string out))

;; --- basic: std/base default (section, emph/strong, list) -----------------
(define basic-html
  (string-append
   "<section><h1 id=\"sec-1\">Hello</h1>"
   "<p>Some <em>emphasis</em> and <strong>bold</strong> text.</p>"
   "<ol type=\"1\"><li>First</li><li>Second</li></ol></section>"))

(test-case "doc binding renders the resolved document"
  (check-equal? (doc->html "basic.rkt") basic-html))

(test-case "main submodule prints rendered HTML to stdout"
  ;; main appends a trailing newline for terminal friendliness.
  (check-equal? (main-output "basic.rkt") (string-append basic-html "\n")))

;; --- nested: contexts drive auto heading depth (h1 then h2) ----------------
(test-case "contexts: nested sections auto-number heading depth"
  (check-equal?
   (doc->html "nested.rkt")
   (string-append
    "<section><h1 id=\"sec-1\">Top</h1><p>Intro.</p>"
    "<section><h2 id=\"sec-2\">Nested</h2><p>Deeper.</p></section></section>")))

;; --- xref: #use, plugins, and two-pass order-independence ------------------
;; `#ref("vm")` appears BEFORE `#def`, yet resolves to the def anchor — the
;; whole eval.rkt pipeline (namespace, #use, two-pass) runs identically.
(test-case "use + two-pass: ref before def resolves; glossary backlinks"
  (define html (doc->html "xref.rkt"))
  (check-regexp-match #rx"<a class=\"xref-ref\" id=\"ref-1\" href=\"#def-vm\">virtual machine</a>" html)
  (check-regexp-match #rx"<dfn id=\"def-vm\">virtual machine</dfn>" html)
  (check-regexp-match #rx"<dl class=\"glossary\">" html)
  (check-regexp-match #rx"<a href=\"#ref-1\">\\[1\\]</a>" html))

;; main and doc must agree for a #use document too.
(test-case "main output matches doc rendering for a #use document"
  (check-equal? (main-output "xref.rkt")
                (string-append (doc->html "xref.rkt") "\n")))
