#lang racket/base
;; Command-line renderer:
;;
;;   racket -l- yamd/cli file.yamd [--backend html|markdown|typst]
;;                                 [--mode single|multi] [-o out]
;;   racket -l- yamd/cli --fmt [--check] file.yamd ...
;;
;; Builds a document (set) with the chosen backend and build mode
;; (docs/multi-doc.md): `single` (default) produces one output — a file via
;; -o or stdout, exactly the historical behavior, with #toctree children
;; inlined; `multi` produces one page per source document under the REQUIRED
;; -o directory. Backends are looked up in a table mapping name ->
;; (module-path function-name extension) and `dynamic-require`d only at
;; dispatch time, so the markdown and typst backends need not be present
;; unless actually selected.

(require racket/cmdline
         racket/file
         racket/path
         racket/string
         yamd/build
         yamd/fmt)

(provide run)

;; name -> (list module-path exported-function output-extension). The
;; function takes resolved Content and returns a string. Only the selected
;; backend is loaded.
(define backends
  (hash "html"     '(yamd/backend/html     content->html     ".html")
        "markdown" '(yamd/backend/markdown content->markdown ".md")
        "typst"    '(yamd/backend/typst    content->typst    ".typ")))

;; mode -> build strategy (docs/multi-doc.md). Adding a strategy is one
;; table entry.
(define modes
  (hash "single" single-file-strategy
        "multi"  multi-file-strategy))

;; Clean, located failure — message to stderr, exit 1, no Racket stack trace.
(define (die fmt . args)
  (eprintf "yamd: ~a\n" (apply format fmt args))
  (exit 1))

(define (known-backends)
  (string-join (sort (hash-keys backends) string<?) ", "))

(define (load-backend name)
  (define entry
    (hash-ref backends name
              (lambda () (die "unknown backend ~s; known backends: ~a"
                              name (known-backends)))))
  (define modpath (car entry))
  (define fn (cadr entry))
  (define render
    (with-handlers ([exn:fail? (lambda (e)
                                 (die "backend ~s could not be loaded (~a): ~a"
                                      name modpath (exn-message e)))])
      (dynamic-require modpath fn)))
  (build-backend (string->symbol name) (caddr entry) render))

(define (lookup-mode name)
  (hash-ref modes name
            (lambda () (die "unknown mode ~s; known modes: ~a"
                            name
                            (string-join (sort (hash-keys modes) string<?) ", ")))))

;; Flags that consume the following token as their value.
(define value-flags '("-o" "--output" "-b" "--backend" "-m" "--mode" "--root"))

;; Racket's `command-line` stops recognizing flags at the first positional
;; argument, but the documented usage puts flags after the input file. Permute
;; argv so every flag (and its value) precedes the positional args. `--` ends
;; flag processing, GNU-style.
(define (permute argv)
  (let loop ([xs (vector->list argv)] [flags '()] [pos '()])
    (cond
      [(null? xs) (list->vector (append (reverse flags) (reverse pos)))]
      [(string=? (car xs) "--")
       (list->vector (append (reverse flags) (reverse pos) (cdr xs)))]
      [(member (car xs) value-flags)
       ;; A trailing value-flag would otherwise swallow the input file after
       ;; permutation and yield a misleading "given 0 arguments" — reject it.
       (when (null? (cdr xs))
         (die "option ~a requires a value" (car xs)))
       (loop (cddr xs) (list* (cadr xs) (car xs) flags) pos)]
      [(let ([a (car xs)])
         (and (> (string-length a) 1) (char=? #\- (string-ref a 0))))
       ;; zero-arg flag (e.g. --help) — let command-line handle/report it.
       (loop (cdr xs) (cons (car xs) flags) pos)]
      [else (loop (cdr xs) flags (cons (car xs) pos))])))

(define (run [argv (current-command-line-arguments)])
  ;; backend/mode stay #f until explicitly given, so --fmt can reject them
  ;; loudly instead of silently ignoring a meaningless combination.
  (define backend #f)
  (define mode #f)
  (define out-file #f)
  (define root-dir #f)
  (define fmt? #f)
  (define check? #f)
  (define inputs
    (command-line
     #:program "yamd"
     #:argv (permute argv)
     #:once-each
     [("-b" "--backend") b
                         "Output backend: html (default), markdown, typst"
                         (set! backend b)]
     [("-m" "--mode") m
                      "Build mode: single (default, one output), multi (one page per source)"
                      (set! mode m)]
     [("-o" "--output") o
                        "single: write to this file instead of stdout; multi: output directory (required)"
                        (set! out-file o)]
     [("--root") r
                 "Base directory for #doc-ref ids (default: the input file's directory) and root-relative /image srcs"
                 (set! root-dir r)]
     [("--fmt") "Format the .yamd sources (conservative, semantics-preserving); writes in place, or to -o"
                (set! fmt? #t)]
     [("--check") "With --fmt: exit 1 if any file is not already formatted (writes nothing)"
                  (set! check? #t)]
     #:args input-files input-files))
  (when (null? inputs)
    (die "expected an input file"))
  (for ([i (in-list inputs)])
    (unless (file-exists? i)
      (die "no such input file: ~a" i)))
  (when (and check? (not fmt?))
    (die "--check is only meaningful with --fmt"))
  (when fmt?
    (when (or backend mode root-dir)
      (die "--backend/--mode/--root make no sense with --fmt (formatting rewrites source, it does not render)"))
    (when (and out-file (not (null? (cdr inputs))))
      (die "-o with --fmt takes exactly one input file (got ~a)" (length inputs)))
    (define ok?
      (for/fold ([ok? #t]) ([i (in-list inputs)])
        (and (run-fmt i out-file check?) ok?)))
    (exit (if ok? 0 1)))
  (unless (null? (cdr inputs))
    (die "rendering takes exactly one input file (got ~a); several files only make sense with --fmt"
         (length inputs)))
  (define input (car inputs))
  (set! backend (or backend "html"))
  (set! mode (or mode "single"))
  (define strategy (lookup-mode mode))
  (define be (load-backend backend))
  (when root-dir
    (unless (directory-exists? root-dir)
      (die "--root ~a is not a directory" root-dir)))
  (define outputs
    (with-handlers ([exn:fail? (lambda (e) (die "~a" (exn-message e)))])
      (build-docset input #:strategy strategy #:backend be
                    #:root-dir (and root-dir (path->complete-path root-dir)))))
  (cond
    [(equal? mode "multi") (write-pages out-file outputs)]
    [else
     ;; one-output modes: file or stdout, as before. A custom strategy
     ;; registered under a non-"multi" mode that yields several outputs must
     ;; fail loudly, never have pages silently dropped.
     (unless (= 1 (length outputs))
       (die "mode ~s produced ~a outputs; only a directory-writing mode like multi can place several pages"
            mode (length outputs)))
     (define text (build-output-text (car outputs)))
     (cond
       [out-file
        (when (directory-exists? out-file)
          (die "-o ~a is a directory; --mode ~a writes one file (use --mode multi for a directory of pages)"
               out-file mode))
        (with-handlers ([exn:fail:filesystem?
                         (lambda (e) (die "~a" (exn-message e)))])
          (call-with-output-file out-file #:exists 'replace
            (lambda (o) (displayln text o))))]
       [else (displayln text)])]))

;; multi-file mode writer: -o names a directory; each page keeps its
;; source-relative path, subdirectories created as needed.
(define (write-pages out-dir outputs)
  (unless out-dir
    (die "--mode multi requires -o <directory> (multiple pages cannot go to stdout)"))
  (when (file-exists? out-dir)
    (die "-o ~a exists and is a file; --mode multi needs a directory" out-dir))
  (for ([o (in-list outputs)])
    (define dest (build-path out-dir (build-output-path o)))
    ;; same clean-error policy as everywhere else in the CLI: filesystem
    ;; failures (permissions, -o under a file, …) die without a stack trace
    (with-handlers ([exn:fail:filesystem?
                     (lambda (e) (die "~a" (exn-message e)))])
      (make-directory* (path-only dest))
      (call-with-output-file dest #:exists 'replace
        (lambda (p) (displayln (build-output-text o) p))))))

;; `--fmt`: conservative source formatting (yamd/fmt). Reads the document with
;; the real reader; a document that does not parse is reported and never
;; rewritten. `--check` writes nothing and returns #f when the file is not
;; already formatted (the caller exits 1 after checking every file —
;; CI-friendly). Otherwise the formatted text is written in place (only when
;; it actually changes) or to `-o`, returning #t.
(define (run-fmt input out-file check?)
  (define text (file->string input))
  (define formatted
    (with-handlers ([exn:fail? (lambda (e)
                                 (die "cannot format ~a: ~a" input (exn-message e)))])
      (format-yamd text #:src input)))
  (cond
    [check?
     (cond
       [(string=? text formatted) #t]
       [else (eprintf "yamd: ~a is not formatted\n" input)
             #f])]
    [out-file
     (when (directory-exists? out-file)
       (die "-o ~a is a directory; --fmt writes a single file" out-file))
     (with-handlers ([exn:fail:filesystem? (lambda (e) (die "~a" (exn-message e)))])
       (call-with-output-file out-file #:exists 'replace
         (lambda (o) (write-string formatted o))))
     #t]
    [(not (string=? text formatted))
     (with-handlers ([exn:fail:filesystem? (lambda (e) (die "~a" (exn-message e)))])
       (call-with-output-file input #:exists 'replace
         (lambda (o) (write-string formatted o))))
     #t]
    [else #t]))

(module+ main
  (run))
