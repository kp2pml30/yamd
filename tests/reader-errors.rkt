#lang racket/base
;; Reader error paths. Successes live in golden .sexp
;; files; here we assert that malformed input is rejected with a located
;; error rather than silently mis-parsed.

(module+ test
  (require rackunit
           yamd)

  ;; Reads + evaluates a string through the full pipeline; reader errors
  ;; surface during the read step.
  (define (run str)
    (yamd-string->html str))

  (define-syntax-rule (check-reader-error rx str)
    (check-exn rx (lambda () (run str))))

  ;; --- indentation -------------------------------------------------------

  (check-reader-error #rx"leading spaces are not allowed"
                      "text\n  spaced\n")

  ;; Inside a RAW block body leading spaces are verbatim content (goldens
  ;; code/python-indent, reader/raw-spaces), but the tabs-only rule is NOT
  ;; weakened at structure positions: a line with fewer structural tabs than
  ;; the raw block's depth is a DEDENT that closes the block, and the
  ;; enclosing markup context then sees the leading space as an error. Here
  ;; the #code body sits at depth 2 (inside #emph); `\t x` has one tab, so it
  ;; dedents out of the raw body and the depth-1 markup rejects its space.
  (check-reader-error #rx"leading spaces are not allowed"
                      "#use(std/code)\n#emph\n\t#code(lang: \"plain\")\n\t\tinner\n\t x\n")

  ;; jumping more than one level deeper
  (check-reader-error #rx"unexpected indentation"
                      "#emph\n\t\tjump\n")

  ;; an indented line with no call opening a body (orphan)
  (check-reader-error #rx"unexpected indentation"
                      "plain\n\torphan\n")

  ;; --- :: hanging blocks -------------------------------------------------

  (check-reader-error #rx"nothing may follow `::`"
                      "#emph:: trailing\n")

  ;; :: used inline (mid text) rather than on its own line
  (check-reader-error #rx"only valid on its own line"
                      "text #emph::\n")

  ;; at line start, :: after an inline body is hanging syntax — trailing
  ;; text is rejected (mid-line it stays literal text, see the
  ;; inline-colons golden)
  (check-reader-error #rx"nothing may follow `::`"
                      "#emph{x}::vector\n")

  ;; --- fences & braces ---------------------------------------------------

  (check-reader-error #rx"unterminated `\\|\\{` fence"
                      "x #tt|{ never closed\n")

  (check-reader-error #rx"unterminated `\\{` body"
                      "x #emph{ never closed\n")

  (check-reader-error #rx"unterminated raw `\\{` body"
                      "#use(std/html)\nx #html{ never closed\n")

  ;; --- comments ----------------------------------------------------------

  (check-reader-error #rx"unterminated `#;\\{` comment"
                      "before #;{ never closed\n")

  ;; --- stray sigils ------------------------------------------------------

  (check-reader-error #rx"stray `#`"
                      "a #! b\n")

  (check-reader-error #rx"empty host escape"
                      "value #() here\n")

  ;; --- #use --------------------------------------------------------------

  (check-reader-error #rx"variant key needs a following"
                      "#use(std/math:)\n")

  (check-reader-error #rx"unsupported #use module spec"
                      "#use(42)\n")

  ;; --- host forms that are illegal inside a body --------------------------

  (check-reader-error #rx"require/provide is not allowed inside a markup body"
                      "#emph{#(require racket/math)x}\n")

  ;; --- host-mode markup re-entry -----------------------------------------

  ;; unterminated `{` re-entry body inside host code
  (check-reader-error #rx"unterminated `\\{` body"
                      "#use(std/base)\n#(list #emph{ never closed\n")

  ;; unterminated raw `{` re-entry body inside host code (custom/raw reader)
  (check-reader-error #rx"unterminated raw `\\{` body"
                      "#use(std/code)\n#(list #code{ never closed\n")

  ;; unterminated fence re-entry body inside host code
  (check-reader-error #rx"unterminated `\\|\\{` fence"
                      "#use(std/base)\n#(list #emph|{ never closed\n")

  ;; a re-entry body is still parsed for `#`: a require inside it is rejected
  (check-reader-error #rx"require/provide is not allowed inside a markup body"
                      "#(list #emph{#(require racket/math)x})\n")

  ;; a re-entry call to a truly undefined function is a real eval-time error
  ;; (the reader accepts it — dangling calls surface at evaluation)
  (check-exn exn:fail?
             (lambda () (run "#(list #no-such-fn{x})\n")))

  ;; reserved names stay Racket's: a sized flvector literal is not a call —
  ;; it hits Racket's own (clear) "literal flvectors not allowed" error
  (check-reader-error #rx"flvector"
                      "#(list #fl3(0.0))\n")

  ;; #use is a top-level reader directive, not a host-mode call
  (check-reader-error #rx"#use is not allowed in host mode"
                      "#(list #use(std/base))\n")

  ;; re-entry does not fire inside a self-quoting Racket literal read via
  ;; fallback: the vector's elements are read by Racket's default reader,
  ;; which rejects `#emph{` (explicit read error — here Racket's number
  ;; reader chokes on the `#e` prefix — never a silently embedded call)
  (check-reader-error #rx"read-syntax"
                      "#(vector-length #(1 #emph{x} 2))\n")

  ;; --- block-local scoping: a block-local define does not leak -----------

  (check-exn exn:fail?
             (lambda ()
               (run "#emph\n\t#(define local-only 1)\n\tinside #(local-only)\nout #(local-only)\n"))))
