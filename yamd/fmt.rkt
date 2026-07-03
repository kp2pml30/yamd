#lang racket/base
;; Conservative, semantics-preserving source formatter for .yamd documents.
;;
;; YAMD is whitespace- and indentation-sensitive: tabs are structural, raw and
;; custom bodies are verbatim, blank lines are paragraph breaks, and inside an
;; inline `{...}` body or a `|{...}|` fence a blank line is *literal* text. So
;; the formatter does almost nothing, and proves that the little it does is
;; safe rather than trusting hand-written line rules.
;;
;; Safety model — the reader is the oracle. We parse the ORIGINAL and the
;; candidate with the real reader (yamd/reader) and accept the candidate only
;; if it reads to a *canonically equivalent* list of top-level forms. The one
;; canonicalization we apply — collapsing runs of consecutive `(para-break)`
;; forms to a single one, and dropping para-breaks at the document edges — is
;; render-neutral by construction: `decode-flow` (yamd/decode) groups any run
;; of >=1 blank line into exactly one paragraph split and trims edges. Any
;; edit that changes a real string, a call, or host code changes the datum and
;; is therefore REJECTED, and the candidate is discarded in favour of a more
;; conservative one. This makes correctness independent of how clever the
;; line-level pass is.
;;
;; Normalizations actually performed (all gated as above):
;;   * CRLF -> LF (the reader does this internally, so it is render-neutral).
;;   * whitespace-only lines -> empty (a blank line is a blank line regardless
;;     of the spaces on it; datum-invariant).
;;   * runs of 3+ blank lines -> 2, OUTSIDE verbatim regions.
;;   * the file ends with a newline; a trailing run of blank lines collapses
;;     to ONE blank line — never to zero. Removing it entirely would be safe
;;     for a standalone document (decode-flow trims top-level edges) but NOT
;;     for a file spliced mid-flow by `#include`, where a trailing blank line
;;     is a load-bearing paragraph separator — and the formatter cannot know
;;     which kind of file it has. Presence is preserved everywhere; only
;;     multiplicity is normalized, which is render-neutral in every context.
;;   * the space after a leading call's `:` body introducer -> one space
;;     (`#name(args): body`); the reader skips that whitespace, so it is
;;     insignificant.
;;
;; Deliberately NOT performed:
;;   * Stripping trailing whitespace on non-blank (content) lines. In YAMD a
;;     text line's trailing spaces become *literal output* — `"hello   \n"` and
;;     `"hello\n"` render to different bytes (`<p>hello   \n...` vs
;;     `<p>hello\n...`). This is the opposite of most languages; the usual
;;     "strip trailing whitespace" rule is UNSAFE here, so we only touch lines
;;     that are entirely whitespace (which are paragraph breaks either way).
;;   * Anything to indentation levels (they are semantic).
;;   * Anything inside raw/custom bodies, fences, or inline `{...}` bodies.
;;
;; Verbatim regions are located from the reader's own srclocs (never by
;; regex-guessing structure): any top-level form whose datum contains a
;; multi-line string literal has its WHOLE source span protected. That covers
;; raw block bodies, inline raw / fence bodies and multi-line inline markup
;; bodies — deliberately over-approximate: a big `::` section containing one
;; multi-line inline body is protected in full and its interior blank runs are
;; left alone (safe, just less formatting). Custom body readers that discard
;; their source text (e.g. a math parser) are the converse gap — their blocks
;; are NOT detected as verbatim, so a blank-collapse inside them rides on the
;; oracle: accepted when the custom parse is insensitive to the blanks,
;; rejected (falling back) when it is not.

(require racket/string
         "reader/reader.rkt"
         "reader/registry.rkt")

(provide format-yamd
         yamd-formatted?)

;; ---------------------------------------------------------------------------
;; Reading with the same body-reader seeding as the evaluator.

;; Only yamd/std/base is attached by default (see eval.rkt); #use inside the
;; document merges the rest at read time, exactly as during evaluation.
(define default-modules '(yamd/std/base))

(define (seed-body-readers)
  (for/fold ([acc (hasheq)]) ([m (in-list default-modules)])
    (define subm `(submod ,m yamd-reader))
    (if (module-declared? subm #t)
        (for/fold ([a acc]) ([(k v) (in-hash (dynamic-require subm 'body-readers))])
          (hash-set a k v))
        acc)))

;; Read a .yamd source string into the reader's top-level forms. `src` is used
;; for srclocs and for resolving relative `#use` paths, so pass the real file
;; path when formatting a file.
(define (read-forms text src)
  (parameterize ([current-body-readers (seed-body-readers)])
    (yamd-read-syntax src (open-input-string text))))

;; ---------------------------------------------------------------------------
;; Canonicalization: the render-neutral equivalence the oracle compares under.

(define (strip x)
  (cond
    [(syntax? x) (strip (syntax->datum x))]
    [(pair? x) (cons (strip (car x)) (strip (cdr x)))]
    [else x]))

(define (para-break-datum? d) (equal? d '(para-break)))

;; Collapse maximal runs of consecutive para-breaks to a single one. Sound at
;; every nesting level: paragraph grouping depends only on the *presence* of a
;; break in a run, never its multiplicity.
(define (collapse-pb xs)
  (let loop ([xs xs] [prev-pb? #f] [acc '()])
    (cond
      [(null? xs) (reverse acc)]
      [(para-break-datum? (car xs))
       (if prev-pb? (loop (cdr xs) #t acc) (loop (cdr xs) #t (cons (car xs) acc)))]
      [else (loop (cdr xs) #f (cons (car xs) acc))])))

(define (drop-edge-pb xs)
  (define (drop-lead l)
    (if (and (pair? l) (para-break-datum? (car l))) (drop-lead (cdr l)) l))
  (reverse (drop-lead (reverse (drop-lead xs)))))

;; A non-para-break proper list is recursed into and its para-break runs
;; collapsed (a call's bodies may hold para-breaks from block / hanging markup
;; bodies). Improper/dotted pairs come only from quoted host data (e.g. attr
;; alists) and never carry para-breaks, so they are recursed structurally
;; without run-collapsing. Only the TOP level drops edge para-breaks:
;; decode-flow always paragraph-groups the top level (so its edge breaks
;; vanish), whereas a nested body is grouped only when it contains a break —
;; dropping a nested edge break there could flip <p>-wrapping.
(define (canon-elem x)
  (cond
    [(para-break-datum? x) x]
    [(list? x) (collapse-pb (map canon-elem x))]
    [(pair? x) (cons (canon-elem (car x)) (canon-elem (cdr x)))]
    [else x]))

(define (canon-forms forms)
  (drop-edge-pb (collapse-pb (map canon-elem (strip forms)))))

;; ---------------------------------------------------------------------------
;; Verbatim regions, from the reader's srclocs.

;; A string literal that spans lines (has an interior newline) can only come
;; from a verbatim body: a raw/custom block, an inline raw / fence body, or a
;; multi-line inline markup body. Per-line separators are their own "\n"
;; strings (length 1) and don't trip this, so ordinary markup block bodies stay
;; unprotected and their blank lines remain collapsible.
(define (datum-multiline-string? d)
  (cond
    [(string? d) (and (> (string-length d) 1) (regexp-match? #rx"\n" d))]
    [(pair? d) (or (datum-multiline-string? (car d)) (datum-multiline-string? (cdr d)))]
    [else #f]))

;; List of (start-index . end-index) 0-based half-open char ranges to leave
;; untouched, derived from top-level forms whose datum carries verbatim text.
(define (protected-ranges forms)
  (for/list ([f (in-list forms)]
             #:when (and (syntax? f)
                         (syntax-position f)
                         (syntax-span f)
                         (datum-multiline-string? (syntax->datum f))))
    (cons (sub1 (syntax-position f))
          (sub1 (+ (syntax-position f) (syntax-span f))))))

;; A line (whose first char is at 0-based `start`) is protected if that char
;; falls inside a verbatim span. Interior lines of a raw block satisfy this;
;; the line after the block does not.
(define (line-protected? start ranges)
  (for/or ([r (in-list ranges)])
    (and (<= (car r) start) (< start (cdr r)))))

;; ---------------------------------------------------------------------------
;; Line-level normalization.

(define (crlf->lf text) (string-replace text "\r\n" "\n"))

(define (whitespace-only? str) (regexp-match? #px"^[ \t]*$" str))

;; Lines as (list content-string start-index end-index); end-index is the
;; newline position (or the string length for the trailing segment). One entry
;; per line, including a final empty segment when the text ends with a newline.
(define (line-spans text)
  (define n (string-length text))
  (let loop ([i 0] [start 0] [acc '()])
    (cond
      [(= i n) (reverse (cons (list (substring text start i) start i) acc))]
      [(char=? (string-ref text i) #\newline)
       (loop (add1 i) (add1 i) (cons (list (substring text start i) start i) acc))]
      [else (loop (add1 i) start acc)])))

;; Normalize the space after a LEADING call's `:` body introducer to one
;; space. The reader `skip-hspace`s there, so the amount is insignificant.
;; Anchored at line start (after tabs) so mid-line `#x:` and prose `a:b` are
;; untouched; `(?!:)` skips the `::` hanging form; the optional `(...)` group
;; is dumb about nested parens on purpose — the oracle gate is the safety net.
(define colon-rx
  #px"^(\t*#[A-Za-z][A-Za-z0-9/-]*(?:\\([^\n]*\\))?):(?!:)[ \t]*(\\S.*)$")

(define (colon-normalize str)
  (define m (regexp-match colon-rx str))
  (if m (string-append (cadr m) ": " (caddr m)) str))

;; Build a candidate. Each toggle is an independently-safe normalization the
;; fallback ladder can drop when the oracle rejects a fuller candidate:
;;   collapse? — runs of 3+ blank lines -> 2 (outside verbatim regions);
;;   colon?    — normalize the space after a leading call's `:`.
;; Emptying whitespace-only lines and the trailing-edge rule (finish-lines)
;; are always applied — both are render-neutral in every context (see header).
(define (build lf ranges #:collapse? collapse? #:colon? colon?)
  (define out
    (let loop ([ls (line-spans lf)] [run 0] [acc '()])
      (cond
        [(null? ls) (reverse acc)]
        [else
         (define entry (car ls))
         (define str (car entry))
         (define start (cadr entry))
         (define protected? (line-protected? start ranges))
         (cond
           [(and (not protected?) (whitespace-only? str))
            ;; blank line: empty it, and (outside verbatim) keep at most 2.
            (if (and collapse? (>= run 2))
                (loop (cdr ls) (add1 run) acc)
                (loop (cdr ls) (add1 run) (cons "" acc)))]
           [else
            (define line
              (if (and colon? (not protected?)) (colon-normalize str) str))
            (loop (cdr ls) 0 (cons line acc))])])))
  (finish-lines out))

;; Trailing edge: end with a newline, and collapse a trailing run of blank
;; lines to exactly ONE blank line — never remove it. A trailing blank line is
;; render-neutral for a standalone document but is a paragraph separator when
;; the file is spliced mid-flow by `#include`; collapsing multiplicity while
;; preserving presence is safe in both roles (paragraph grouping only ever
;; cares whether a blank run exists, never how long it is). An all-blank
;; document becomes a single blank line for the same reason; only a genuinely
;; empty document stays empty.
(define (finish-lines lines)
  (define joined (string-join lines "\n"))
  (cond
    [(string=? joined "") ""]
    [(regexp-match? #px"^\n+$" joined) "\n"]
    [(regexp-match? #px"\n\n+$" joined) (regexp-replace #px"\n\n+$" joined "\n\n")]
    [(regexp-match? #rx"\n$" joined) joined]
    [else (string-append joined "\n")]))

;; ---------------------------------------------------------------------------
;; The oracle gate and the public entry points.

(define (gate-ok? candidate src want)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (equal? (canon-forms (read-forms candidate src)) want)))

;; Format `text`. Raises if the document cannot be read (a document that does
;; not parse must not be rewritten). Never returns output that would change the
;; rendered result: (collapse? colon?) candidates are tried most- to least-
;; normalizing and only an oracle-approved one is returned. The all-off entry
;; only empties blank lines and applies the trailing-edge rule, so it is
;; effectively always safe; `lf` is a final belt-and-suspenders fallback.
(define ladder '((#t #t) (#t #f) (#f #t) (#f #f)))

(define (format-yamd text #:src [src 'yamd-fmt])
  (define lf (crlf->lf text))
  (define forms (read-forms lf src))
  (define want (canon-forms forms))
  (define ranges (protected-ranges forms))
  (let try ([opts ladder])
    (cond
      [(null? opts) lf]
      [else
       (define o (car opts))
       (define cand (build lf ranges #:collapse? (car o) #:colon? (cadr o)))
       (if (gate-ok? cand src want) cand (try (cdr opts)))])))

;; True iff `text` is already in formatted form (used by `--fmt --check`).
(define (yamd-formatted? text #:src [src 'yamd-fmt])
  (string=? text (format-yamd text #:src src)))
