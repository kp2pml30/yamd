#lang racket/base
;; Tests for yamd/cli. Exercises the real entry point as a subprocess
;; (`racket -l- yamd/cli ...`) so command-line parsing, exit codes and stdout
;; are covered end to end. Only the html backend is tested here; markdown/typst
;; are dynamic-required at dispatch time and get an integration pass later.

(require rackunit
         racket/port
         racket/system
         racket/file
         yamd
         (only-in yamd/backend/markdown content->markdown)
         (only-in yamd/backend/typst content->typst))

(define racket-exe (find-system-path 'exec-file))

;; Run the CLI with `args`; return (values stdout stderr exit-code).
(define (run-cli . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code racket-exe "-l-" "yamd/cli" args)))
  (values (get-output-string out) (get-output-string err) code))

(define doc-src "#section{Hi}::\n\nHello #emph{world}.\n")

(define (with-doc proc)
  (define f (make-temporary-file "yamd-cli-~a.yamd"))
  (dynamic-wind
   (lambda () (display-to-file doc-src f #:exists 'replace))
   (lambda () (proc f))
   (lambda () (delete-file f))))

(test-case "html backend to stdout matches the library API"
  (with-doc
   (lambda (f)
     (define-values (out err code) (run-cli (path->string f)))
     (check-equal? code 0)
     (check-equal? out (string-append (yamd-file->html (path->string f)) "\n"))
     (check-equal? err ""))))

(test-case "explicit --backend html, flags after the input file"
  (with-doc
   (lambda (f)
     (define-values (out err code) (run-cli (path->string f) "--backend" "html"))
     (check-equal? code 0)
     (check-equal? out (string-append (yamd-file->html (path->string f)) "\n")))))

(test-case "--backend markdown matches the markdown backend and renders content"
  (with-doc
   (lambda (f)
     (define-values (out err code) (run-cli (path->string f) "--backend" "markdown"))
     (check-equal? code 0)
     (check-equal? err "")
     (check-equal? out
                   (string-append
                    (content->markdown (yamd-file->content (path->string f))) "\n"))
     ;; sanity: it is actually markdown, not the html serialization (the
     ;; section heading carries its id as an inline-HTML anchor target)
     (check-equal? out "# <a id=\"sec-1\"></a>Hi\n\nHello *world*.\n"))))

(test-case "--backend typst matches the typst backend and renders content"
  (with-doc
   (lambda (f)
     (define-values (out err code) (run-cli (path->string f) "--backend" "typst"))
     (check-equal? code 0)
     (check-equal? err "")
     (check-equal? out
                   (string-append
                    (content->typst (yamd-file->content (path->string f))) "\n"))
     ;; the section heading is preceded by an invisible labelled anchor target
     (check-equal? out "#metadata(none)<sec-1>\n= Hi\n\nHello _world_.\n"))))

(test-case "-o writes to a file, nothing on stdout"
  (with-doc
   (lambda (f)
     (define outf (make-temporary-file "yamd-cli-out-~a.html"))
     (dynamic-wind
      void
      (lambda ()
        (define-values (out err code)
          (run-cli (path->string f) "-o" (path->string outf)))
        (check-equal? code 0)
        (check-equal? out "")
        (check-equal? (file->string outf)
                      (string-append (yamd-file->html (path->string f)) "\n")))
      (lambda () (delete-file outf))))))

(test-case "unknown backend: clean error, nonzero exit, lists known backends"
  (with-doc
   (lambda (f)
     (define-values (out err code) (run-cli (path->string f) "--backend" "bogus"))
     (check-not-equal? code 0)
     (check-regexp-match #rx"unknown backend" err)
     (check-regexp-match #rx"html" err)
     ;; clean error, not a Racket stack trace
     (check-false (regexp-match? #rx"context\\.\\.\\." err)))))

(test-case "trailing value-flag: clean error, does not swallow the input file"
  (with-doc
   (lambda (f)
     (define-values (out err code) (run-cli (path->string f) "-o"))
     (check-not-equal? code 0)
     (check-regexp-match #rx"option -o requires a value" err)
     (check-false (regexp-match? #rx"context\\.\\.\\." err)))))

;; ---------------------------------------------------------------------------
;; --fmt / --check

;; A dirty-but-valid document: extra blank lines and a non-normalized final
;; newline, which the formatter must fix without changing the rendered output.
(define dirty-src "#section{Hi}\n\n\n\n\nHello #emph{world}.\n\n\n")

(define (with-dirty-doc proc)
  (define f (make-temporary-file "yamd-fmt-~a.yamd"))
  (dynamic-wind
   (lambda () (display-to-file dirty-src f #:exists 'replace))
   (lambda () (proc f))
   (lambda () (delete-file f))))

(test-case "--fmt formats in place, changing the file, nothing on stdout"
  (with-dirty-doc
   (lambda (f)
     (define-values (out err code) (run-cli "--fmt" (path->string f)))
     (check-equal? code 0)
     (check-equal? out "")
     (define formatted (file->string f))
     (check-not-equal? formatted dirty-src)
     ;; render is preserved
     (define g (make-temporary-file "yamd-fmt-src-~a.yamd"))
     (dynamic-wind
      (lambda () (display-to-file dirty-src g #:exists 'replace))
      (lambda ()
        (check-equal? (yamd-string->html formatted)
                      (yamd-string->html dirty-src)))
      (lambda () (delete-file g)))
     ;; idempotent: a second --fmt makes no change
     (define-values (o2 e2 c2) (run-cli "--fmt" (path->string f)))
     (check-equal? c2 0)
     (check-equal? (file->string f) formatted))))

(test-case "--fmt -o writes elsewhere, leaves the input untouched"
  (with-dirty-doc
   (lambda (f)
     (define outf (make-temporary-file "yamd-fmt-out-~a.yamd"))
     (dynamic-wind
      void
      (lambda ()
        (define-values (out err code)
          (run-cli "--fmt" "-o" (path->string outf) (path->string f)))
        (check-equal? code 0)
        (check-equal? (file->string f) dirty-src)      ; input untouched
        (check-not-equal? (file->string outf) dirty-src)) ; output formatted
      (lambda () (delete-file outf))))))

(test-case "--fmt --check: exit 1 on a dirty file, writes nothing"
  (with-dirty-doc
   (lambda (f)
     (define-values (out err code) (run-cli "--fmt" "--check" (path->string f)))
     (check-equal? code 1)
     (check-regexp-match #rx"is not formatted" err)
     (check-equal? (file->string f) dirty-src))))       ; unchanged

(test-case "--fmt --check: exit 0 on an already-formatted file"
  (define f (make-temporary-file "yamd-fmt-clean-~a.yamd"))
  (dynamic-wind
   (lambda () (display-to-file "#section{Hi}\n\nHello #emph{world}.\n" f #:exists 'replace))
   (lambda ()
     (define-values (out err code) (run-cli "--fmt" "--check" (path->string f)))
     (check-equal? code 0)
     (check-equal? err ""))
   (lambda () (delete-file f))))

(test-case "--check without --fmt is a clean error"
  (with-dirty-doc
   (lambda (f)
     (define-values (out err code) (run-cli "--check" (path->string f)))
     (check-not-equal? code 0)
     (check-regexp-match #rx"--check is only meaningful with --fmt" err)
     (check-false (regexp-match? #rx"context\\.\\.\\." err)))))

(test-case "--fmt with --backend/--mode is a clean error, not silently ignored"
  (with-dirty-doc
   (lambda (f)
     (define-values (out err code)
       (run-cli "--fmt" "--backend" "markdown" (path->string f)))
     (check-not-equal? code 0)
     (check-regexp-match #rx"make no sense with --fmt" err)
     (check-equal? (file->string f) dirty-src)          ; nothing written
     (check-false (regexp-match? #rx"context\\.\\.\\." err)))))

(test-case "--fmt on an unparseable document: clean error, no stack trace"
  (define f (make-temporary-file "yamd-fmt-bad-~a.yamd"))
  (dynamic-wind
   (lambda () (display-to-file "#emph{unterminated\n" f #:exists 'replace))
   (lambda ()
     (define-values (out err code) (run-cli "--fmt" (path->string f)))
     (check-not-equal? code 0)
     (check-regexp-match #rx"cannot format" err)
     (check-false (regexp-match? #rx"context\\.\\.\\." err)))
   (lambda () (delete-file f))))

(test-case "nonexistent input file: clean error, nonzero exit, no stack trace"
  (define-values (out err code) (run-cli "/no/such/file-xyz.yamd"))
  (check-not-equal? code 0)
  (check-regexp-match #rx"no such input file" err)
  (check-false (regexp-match? #rx"context\\.\\.\\." err)))
