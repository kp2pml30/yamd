#lang racket/base
;; Integration tests: cross-plugin behavior that no single
;; plugin's own goldens exercise. Rendered success cases for full documents
;; live in tests/golden/integration/*.yamd; here we assert (1) the
;; `yamd-string->html` entry point works end-to-end when several plugins are
;; combined in ONE document, and (2) errors from one plugin still surface
;; (with their message and, where the reader owns it, srcloc) when the failing
;; call is nested inside another plugin's construct.

(module+ test
  (require rackunit
           yamd)

  (define (run str) (yamd-string->html str))

  ;; --- yamd-string->html across plugins (success) ------------------------

  ;; A single string pulling in xref + code + math + section + toc + glossary.
  ;; We assert on stable fragments rather than the whole document (that is what
  ;; the golden .html files are for) — the point here is that the entry point
  ;; wires every plugin's #use into one namespace and both passes run.
  (let ([out (run (string-append
                   "#use(std/xref)\n"
                   "#use(std/code)\n"
                   "#use(std/math: asciimath)\n"
                   "#toc\n"
                   "#section{S}{A #ref(\"a\"){t} and #m{x^2} and #code{v}."
                   " #def(\"a\"){A}{d}}\n"
                   "#glossary\n"))])
    (check-regexp-match #rx"<nav class=\"toc\">" out)          ; toc (xref)
    (check-regexp-match #rx"<h1 id=\"sec-1\">S</h1>" out)      ; section (base)
    (check-regexp-match #rx"href=\"#def-a\">t</a>" out)        ; ref custom text
    (check-regexp-match #rx"<math[^>]*><msup>" out)            ; math plugin
    (check-regexp-match #rx"<code>v</code>" out)               ; inline code
    (check-regexp-match #rx"<dl class=\"glossary\">" out))     ; glossary

  ;; The raw-HTML escape hatch survives the WHOLE pipeline (reader raw body →
  ;; collect → resolve → decode → html backend), verbatim and unescaped, while
  ;; the surrounding prose is still wrapped in a normal <p>.
  (let ([out (run "#use(std/html)\nBefore #html{<b onclick=\"x\">raw & <ok></b>} after\n")])
    (check-regexp-match #rx"<p>Before <b onclick=\"x\">raw & <ok></b> after</p>" out))

  ;; `yamd-string->html` handles a document with no plugins / no markup calls.
  (check-equal? (run "just text\n") "<p>just text</p>")

  ;; --- cross-plugin error propagation ------------------------------------

  ;; A dangling #ref that lives INSIDE a #table cell must still error at
  ;; resolve time — the table plugin does not swallow the xref failure.
  (check-exn #rx"dangling cross-reference"
             (lambda ()
               (run (string-append "#use(std/xref)\n#use(std/table)\n"
                                   "#table\n\t#row{#ref(\"ghost\")}{x}\n"))))

  ;; Using a plugin function whose module was never #use'd is an undefined
  ;; identifier, located at the call site by the reader (#m at col 4).
  (check-exn #rx"string:1:4: m: undefined"
             (lambda () (run "The #m{x^2} here\n")))

  ;; Header auto-depth is shared context state: nesting sections past h6 via
  ;; `::` is an explicit error, and it fires even though the deepest section is
  ;; produced by the same machinery as the shallow ones (integration of the
  ;; reader's :: transform with base's context-driven depth).
  (check-exn #rx"exceeds h6"
             (lambda ()
               (run (apply string-append
                           (for/list ([t '("a" "b" "c" "d" "e" "f" "g")])
                             (string-append "#section{" t "}::\n"))))))

  ;; A raw node addressed to a DIFFERENT backend is rejected by the html
  ;; backend rather than silently dropped (IR contract, exercised through the
  ;; public API).
  (check-exn #rx"raw node for backend latex"
             (lambda () (content->html (raw 'latex "\\textbf{x}"))))

  ;; Duplicate #def ids across two different sections still collide — collect
  ;; state is one document-wide info table, not per-section.
  (check-exn #rx"duplicate #def"
             (lambda ()
               (run (string-append "#use(std/xref)\n"
                                   "#section{One}{#def(\"k\"){t}{a}}\n"
                                   "#section{Two}{#def(\"k\"){t}{b}}\n")))))
