#lang racket/base
;; The `:`-introduced indented block body, and its interaction with the older
;; forms. A bare `#name` alone on a line taking the following indented block is
;; sugar for `#name:` with the `:` elided — the two must parse identically. A
;; trailing `:` after an inline brace body is a block introducer ONLY when a
;; deeper line follows; otherwise it is literal punctuation (`#code{x}:`).

(module+ test
  (require rackunit
           yamd)

  (define (html str) (yamd-string->html str))

  ;; --- bare block == `:` block (the sugar relationship) ------------------
  (test-case "bare #name block equals #name: block"
    (check-equal? (html "#emph\n\ttext\n")
                  (html "#emph:\n\ttext\n")))

  ;; --- `:` lets a titled call (inline body) still take an indented block --
  (test-case "#section{Title}: takes the following indented block as content"
    ;; Same shape as the hanging form `#section{Title}::` with equal content.
    (check-equal? (html "#section{T}:\n\tbody\n")
                  (html "#section{T}::\nbody\n")))

  ;; --- the trailing `:` is optional: brace body + block, colon or not -----
  (test-case "#section{Title} block equals #section{Title}: block"
    ;; The two spellings are the same form (the `:` is optional sugar).
    (check-equal? (html "#section{T}\n\tbody\n")
                  (html "#section{T}:\n\tbody\n")))

  (test-case "no-colon titled block renders title + block content"
    (check-equal? (html "#section{Alpha}\n\tBody.\n")
                  "<section><h1 id=\"sec-1\">Alpha</h1>Body.\n</section>"))

  (test-case "no-colon multi-body call still takes the block"
    ;; A brace-bodied call with more than one inline body also grabs the block.
    (check-equal? (html "#emph{a}\n\tbody\n")
                  (html "#emph{a}:\n\tbody\n")))

  (test-case "#section{Title}: renders title + block content"
    ;; A single content line is not <p>-wrapped (matches the hanging form);
    ;; multi-paragraph content is (see the colon-titled golden).
    (check-equal? (html "#section{Alpha}:\n\tBody.\n")
                  "<section><h1 id=\"sec-1\">Alpha</h1>Body.\n</section>"))

  ;; --- punctuation safety: trailing `:` with no deeper line stays text ----
  (test-case "brace body + trailing colon, no block below, is literal colon"
    (check-equal? (html "#emph{note}:\nnext\n")
                  "<p><em>note</em>:\nnext</p>"))

  (test-case "brace body + `: text` on the line is literal, not a body"
    ;; `: text` after a brace body is not a second body (that would break
    ;; prose like `see #code{f}: below`); it stays literal punctuation.
    (check-equal? (html "#emph{note}: and more\n")
                  "<p><em>note</em>: and more</p>"))

  ;; --- back-compat of the no-prior-body colon forms ----------------------
  (test-case "#name: text is still a rest-of-line body"
    (check-equal? (html "#emph: hi\n") "<p><em>hi</em></p>"))

  (test-case "#name: at eol with no block is still an empty body"
    (check-equal? (html "#emph:\ntail\n") "<p><em></em>\ntail</p>"))

  ;; --- `::` hanging is unaffected by the single-colon changes ------------
  (test-case "#section{Title}:: hanging still folds the rest of the block"
    (check-equal? (html "#section{T}::\nrest\n")
                  "<section><h1 id=\"sec-1\">T</h1>rest\n</section>")))
