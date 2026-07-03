#lang racket/base
;; Tests for yamd/fmt, the conservative source formatter.
;;
;; Three layers:
;;   * unit corner cases — exact formatting of each normalization AND each
;;     verbatim region that must be left alone (raw blocks, fences, inline
;;     bodies, hanging blocks, comments, ##, CRLF);
;;   * idempotency + no-op — format is a fixed point on every committed .yamd
;;     (they are already clean) and format(format x) = format x;
;;   * semantic preservation — for every renderable golden, rendering a dirtied
;;     copy after formatting equals rendering the original (the formatter never
;;     changes output bytes).

(require rackunit
         racket/file
         racket/path
         racket/string
         racket/runtime-path
         yamd
         yamd/fmt)

;; ---------------------------------------------------------------------------
;; Unit corner cases: exact outputs.

(define (chk label in out)
  (check-equal? (format-yamd in) out label)
  ;; every normalization must be idempotent
  (check-equal? (format-yamd out) out (string-append label " [idempotent]")))

;; final newline / trailing edge. A trailing blank-line RUN collapses to one
;; blank line but is never removed: fully stripping it would change the render
;; of a file spliced mid-flow by #include (a trailing blank there is a real
;; paragraph separator), and the formatter cannot know a file is a fragment.
(chk "add final newline" "a" "a\n")
(chk "keep single final newline" "a\n" "a\n")
(chk "keep one trailing blank line" "a\n\n" "a\n\n")
(chk "collapse trailing blank run to one" "a\n\n\n\n" "a\n\n")
(chk "empty stays empty" "" "")
(chk "all-blank -> one blank line" "\n\n\n" "\n")

;; blank-line collapse (interior 3+ -> 2)
(chk "collapse interior 4 blanks" "a\n\n\n\n\nb\n" "a\n\n\nb\n")
(chk "keep two blank lines" "a\n\n\nb\n" "a\n\n\nb\n")
(chk "keep one blank line" "a\n\nb\n" "a\n\nb\n")

;; whitespace-only lines are emptied (they are paragraph breaks either way)
(chk "empty a whitespace-only line" "a\n   \nb\n" "a\n\nb\n")
(chk "empty tab-only line" "a\n\t\nb\n" "a\n\nb\n")

;; trailing whitespace on CONTENT lines is significant and MUST be kept
(chk "keep trailing whitespace on text line" "hello   \nworld\n" "hello   \nworld\n")

;; CRLF -> LF (the reader normalizes it, so it is render-neutral)
(chk "crlf to lf" "a\r\nb\r\n" "a\nb\n")

;; `:` body spacing normalized to one space (reader skips that whitespace)
(chk "colon many spaces -> one" "#emph:    hi\n" "#emph: hi\n")
(chk "colon no space -> one" "#emph:hi\n" "#emph: hi\n")
(chk "colon with args" "#link(\"u\"):  text\n" "#link(\"u\"): text\n")
(chk "colon keeps body trailing text" "#emph:  a b c\n" "#emph: a b c\n")

;; `::` hanging blocks are NOT colon-normalized (nothing follows on the line)
(chk "hanging colon untouched" "#emph::\nbody\n" "#emph::\nbody\n")
(chk "titled hanging untouched" "#section{T}::\nbody\n" "#section{T}::\nbody\n")

;; mid-line and prose colons are not touched
(chk "prose colon untouched" "see a:b here\n" "see a:b here\n")

;; ## literal hash is untouched
(chk "double-hash literal" "##foo\n" "##foo\n")

;; ---------------------------------------------------------------------------
;; Verbatim regions: NOTHING inside them may change.

;; raw block body: interior blank lines AND trailing whitespace are literal.
(define raw-block
  (string-append
   "#use(std/code)\n"
   "#code(lang: python)\n"
   "\tdef f():   \n"      ; trailing spaces significant in raw
   "\n\n\n\n"             ; 4 interior blank lines significant in raw
   "\t\treturn 1\n"))
(check-equal? (format-yamd raw-block) raw-block "raw block left verbatim")

;; blank lines inside a fence are literal text
(check-equal? (format-yamd "#emph|{a\n\n\n\nb}|\n") "#emph|{a\n\n\n\nb}|\n"
              "fence body left verbatim")

;; blank lines inside a multi-line inline markup body are literal text
(check-equal? (format-yamd "#emph{a\n\n\n\nb}\n") "#emph{a\n\n\n\nb}\n"
              "inline markup body left verbatim")

;; a `#;{...}` block comment spanning blank lines is left intact (its content
;; is discarded, but we never touch it) — and the surrounding text still formats
(check-equal? (format-yamd "a\n#;{comment\n\n\nstill comment}\nb\n")
              "a\n#;{comment\n\n\nstill comment}\nb\n"
              "block comment left verbatim")

;; ---------------------------------------------------------------------------
;; Documents that don't parse must NOT be rewritten (format raises).

(check-exn exn:fail? (lambda () (format-yamd "#emph{unterminated\n"))
           "unterminated body raises rather than mangling")

;; yamd-formatted? predicate
(check-true (yamd-formatted? "a\n"))
(check-false (yamd-formatted? "a\n\n\n\n"))

;; ---------------------------------------------------------------------------
;; Idempotency + no-op over every committed .yamd, and semantic preservation.

(define-runtime-path golden-dir "golden")
(define-runtime-path samples-dir "../editors/vscode/samples")

;; collect .yamd files under tests/golden and editors/vscode/samples —
;; including `_`-prefixed include fragments: the formatter's per-file rules are
;; fragment-safe by design (trailing blank presence is preserved), so they must
;; be idempotent and clean too. Non-renderable ones just skip the render check.
(define (yamd-files)
  (define roots (list golden-dir samples-dir))
  (for*/list ([root (in-list roots)]
              #:when (directory-exists? root)
              [p (in-directory root)]
              #:when (path-has-extension? p #".yamd"))
    p))

;; A document is standalone-renderable if yamd-string->html succeeds (toctree
;; roots and unimplemented-syntax samples are skipped — they are covered by
;; other suites); when it renders, formatting must preserve the output.
(define (renders? text src)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (yamd-string->html text #:src src)
    #t))

(for ([f (in-list (yamd-files))])
  (define src (path->string f))
  (define text (file->string f))
  (define once
    (with-handlers ([exn:fail? (lambda (_) #f)]) (format-yamd text #:src src)))
  (cond
    [(not once)
     ;; unparseable by the current reader (e.g. samples using future syntax):
     ;; the formatter raised rather than mangling — nothing more to assert.
     (void)]
    [else
     (define twice (format-yamd once #:src src))
     ;; idempotent
     (check-equal? twice once (string-append src " [idempotent]"))
     ;; committed files are already clean: format is a no-op
     (check-equal? once text (string-append src " [already formatted]"))
     ;; Semantic preservation is the formatter's core contract: for ANY input
     ;; it accepts, the rendered output is unchanged. We check it on the
     ;; pristine text and on a dirtied copy (leading/trailing blank lines +
     ;; CRLF) that exercises the blank-collapse / final-newline / CRLF paths.
     ;; (Note we assert render(format X) = render X, NOT = render text: for a
     ;; doc ending inside a `::` hanging block, appended trailing blanks are
     ;; themselves render-significant, so the dirtied doc renders differently
     ;; from the original — the formatter still preserves ITS input's output.)
     (when (renders? text src)
       (define dirty
         (string-append "\n\n" (string-replace text "\n" "\r\n") "\n\n\n"))
       (for ([x (in-list (list text dirty))])
         (check-equal? (yamd-string->html (format-yamd x #:src src) #:src src)
                       (yamd-string->html x #:src src)
                       (string-append src " [render preserved]"))))]))

;; ---------------------------------------------------------------------------
;; Cross-file preservation: formatting an #include FRAGMENT must not change the
;; INCLUDING document's render. A fragment's trailing blank line is spliced
;; mid-flow where it is a real paragraph separator — this is why the formatter
;; collapses a trailing blank run to one blank line instead of stripping it
;; (regression test for exactly that bug).

(let ()
  (define dir (make-temporary-directory "yamd-fmt-include-~a"))
  (dynamic-wind
   void
   (lambda ()
     (define child (build-path dir "child.yamd"))
     (define parent (build-path dir "parent.yamd"))
     (display-to-file "CHILD\n\n\n\n" child)   ; trailing blank run: separator
     (display-to-file "#use(std/include)\n#include(\"child.yamd\")\nafter\n" parent)
     (define before (yamd-file->html (path->string parent)))
     ;; sanity: the separator matters — CHILD and after are separate <p>s
     (check-regexp-match #rx"<p>CHILD</p>" before)
     (define formatted (format-yamd (file->string child) #:src (path->string child)))
     (check-equal? formatted "CHILD\n\n" "fragment keeps one trailing blank line")
     (display-to-file formatted child #:exists 'replace)
     (check-equal? (yamd-file->html (path->string parent)) before
                   "formatting an included fragment preserves the parent's render"))
   (lambda () (delete-directory/files dir))))
