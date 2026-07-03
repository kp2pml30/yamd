#lang racket/base
;; The YAMD indentation reader. Turns a .yamd port into a list of top-level
;; syntax objects evaluated by yamd/eval. Every application syntax carries a
;; srcloc pointing at the `#` of the call.
;;
;; Implemented: plain text, blank-line paragraph breaks, `##`, `#`-space /
;; `#;` / `#;{...}` comments, `#(expr)` host escape (statement and inline),
;; `#name`, `#name(args)` with `key:` keyword sugar, `#name{body}` (markup /
;; raw / custom / lazy per registry, nestable, multi-body), `#name(args):
;; rest-of-line`, indented block bodies (markup / raw / custom), `::` hanging
;; blocks (recursive, top level = rest of file), `|{ }|` / `|==={ }===|`
;; fences for inline bodies, `#use(...)` module imports + reader-submodule
;; body-reader merging, block-local definitions via the yamd-body splicing
;; macro (runtime.rkt), and host-mode markup re-entry (`#name{markup}` inside
;; `(args)` / `#(expr)`, via a `#`-overriding readtable — see "Host-mode
;; markup re-entry" below).

(require racket/port
         racket/string
         racket/list
         syntax/strip-context
         "registry.rkt")

(provide yamd-read-syntax
         yamd-name-char?)

(define (reader-error/loc src line col msg . args)
  (apply error 'yamd-reader
         (string-append "~a:~a:~a: " msg)
         src line col args))

(define (reader-error src in msg . args)
  (define-values (line col pos) (port-next-location in))
  (apply reader-error/loc src line col msg args))

(define (loc-syntax src datum line col pos span)
  (strip-context
   (datum->syntax #f datum (list src line col pos span))))

;; ---------------------------------------------------------------------------
;; Top level

(define (yamd-read-syntax src in0)
  (define in (normalize-input in0))
  ;; Body-reader merges from `#use` stay local to this file.
  (parameterize ([current-body-readers (current-body-readers)])
    (read-block src in 0)))

;; CRLF -> LF so `\r` never reaches the scanners. Reads the whole port up
;; front — documents are files, not streams.
(define (normalize-input in0)
  (define s (string-replace (port->string in0) "\r\n" "\n"))
  (define in (open-input-string s (object-name in0)))
  (port-count-lines! in)
  in)

;; ---------------------------------------------------------------------------
;; Indentation primitives (all peek-only unless named consume-*)

;; True if from the current position to the next newline/eof everything is
;; whitespace. Byte-offset peeking is fine: skipped chars are 1-byte ASCII.
(define (line-blank? in)
  (let loop ([k 0])
    (define ch (peek-char in k))
    (cond
      [(eof-object? ch) #t]
      [(eqv? ch #\newline) #t]
      [(or (eqv? ch #\space) (eqv? ch #\tab)) (loop (add1 k))]
      [else #f])))

;; Tab-depth of a NON-blank line (peek-only). Leading spaces before content
;; are a reader error (indentation is tabs only). Used for MARKUP structure,
;; where a leading space is always a structure position and thus illegal.
(define (line-tab-depth src in)
  (let loop ([k 0])
    (define ch (peek-char in k))
    (cond
      [(eqv? ch #\tab) (loop (add1 k))]
      [(eqv? ch #\space)
       (define-values (line col pos) (port-next-location in))
       (reader-error/loc src line (and col (+ col k))
                         "leading spaces are not allowed; indent with tabs only")]
      [else k])))

;; Leading-tab count of a NON-blank line (peek-only), stopping at the first
;; non-tab char WITHOUT erroring on a space. Used only for RAW/custom block
;; bodies: there the block's structural tabs are the only structure and
;; everything after them — including leading spaces and deeper tabs — is
;; verbatim content. The count is used solely to detect dedent (fewer tabs
;; than the block's `stop` closes it); a `[tab…][space]` line thus counts
;; only its tabs, so with fewer tabs than `stop` it is a DEDENT — the block
;; closes and the enclosing markup context re-scans the line, where the
;; leading space is a normal structure-position error.
(define (raw-line-leading-tabs in)
  (let loop ([k 0])
    (if (eqv? (peek-char in k) #\tab) (loop (add1 k)) k)))

(define (consume-chars in n)
  (for ([_ (in-range n)]) (read-char in)))

(define (consume-line in)
  (let loop ()
    (define ch (read-char in))
    (unless (or (eof-object? ch) (eqv? ch #\newline)) (loop))))

(define (consume-newline in)
  (when (eqv? (peek-char in) #\newline) (read-char in)))

(define (skip-hspace in)
  (let loop ()
    (define ch (peek-char in))
    (when (or (eqv? ch #\space) (eqv? ch #\tab))
      (read-char in)
      (loop))))

;; After consuming a call line's newline: peek past blank spacer lines to
;; the next non-blank line ("blank lines never close blocks", so they cannot
;; separate a bare call from its block body either). Returns (values
;; tab-depth chars-to-skip) where chars-to-skip covers the blank lines
;; (consumed iff a block is attached), or (values #f #f) at eof. All
;; peeked-over chars are 1-byte ASCII, so byte-offset peeking is safe; the
;; non-blank line's indentation is re-validated by line-tab-depth (space
;; errors) when it is actually read.
(define (peek-next-line-depth in)
  (let loop ([k 0] [line-start 0] [depth 0] [counting-tabs? #t])
    (define ch (peek-char in k))
    (cond
      [(eof-object? ch) (values #f #f)]
      [(eqv? ch #\newline) (loop (add1 k) (add1 k) 0 #t)]
      [(eqv? ch #\tab)
       (loop (add1 k) line-start (if counting-tabs? (add1 depth) depth) counting-tabs?)]
      [(eqv? ch #\space)
       (loop (add1 k) line-start depth #f)]
      [else (values depth line-start)])))

;; ---------------------------------------------------------------------------
;; Block reader

;; Reads the forms of a block at indentation `depth`. Stops (without
;; consuming) at eof or a non-blank line with fewer tabs. Blank lines never
;; close a block; they become (para-break). A deeper line here is an orphan
;; indent — the only legal deeper content is a call's block body, consumed by
;; read-line-call while processing the opening line.
(define (read-block src in depth)
  (let loop ([forms '()])
    (cond
      [(eof-object? (peek-char in)) (reverse forms)]
      [(line-blank? in)
       (consume-line in)
       (loop (cons '(para-break) forms))]
      [else
       (define d (line-tab-depth src in))
       (cond
         [(< d depth) (reverse forms)]
         [(> d depth)
          (reader-error src in
            "unexpected indentation: deeper than the enclosing block (no call opened a body here)")]
         [else
          (consume-chars in d)
          (loop (append (reverse (read-line-forms src in depth)) forms))])])))

;; Reads one logical source line (leading tabs already consumed) at `depth`,
;; returning the forms to splice into the block. Handles line-start host
;; escapes, block-opening / hanging calls, and plain text lines.
(define (read-line-forms src in depth)
  (define ch (peek-char in))
  (cond
    [(and (eqv? ch #\#) (eqv? (peek-char in 1) #\())
     (define-values (line col pos) (port-next-location in))
     (define expr (read-host-escape src in))
     (cond
       ;; `#(...)` alone on the line is a statement (this is how block-local
       ;; #(define ...) works); the trailing "\n" separates it from the next
       ;; line's content.
       [(line-blank? in)
        (skip-hspace in)
        (consume-newline in)
        (list expr "\n")]
       [else
        ;; `#(...)` starts the line but text follows: an inline host escape.
        ;; Emit it raw (see read-hash-form) so it composes like any other.
        (read-text-line src in (list expr))])]
    [(and (eqv? ch #\#)
          (let ([c (peek-char in 1)]) (and (char? c) (char-alphabetic? c))))
     (read-line-call src in depth)]
    [else
     (read-text-line src in '())]))

;; A call at the start of a line: eligible for an indented block body (bare
;; call, nothing else on the line) or a `::` hanging body.
(define (read-line-call src in depth)
  (define-values (line col pos) (port-next-location in))
  (read-char in) ; #
  (define-values (kind payload) (read-call-name+args src in line col pos))
  (cond
    [(eq? kind 'use) (list payload "\n")]
    [else
     (define name (car payload))
     (define breader (cadr payload))
     (define args (caddr payload))
     (define-values (bodies bkind)
       (read-inline-bodies src in name breader #:line-start? #t))
     (cond
       [(eq? bkind 'hanging)
        ;; `bodies` holds any inline bodies that preceded `::`
        ;; (e.g. the title in `#section{Title}::`); the hanging body follows.
        (define hbody (read-hanging-body src in depth breader name))
        (list (assemble-call src in name args (append bodies (list hbody)) line col pos))]
       [(and (eq? bkind 'none) (line-blank? in))
        ;; Bare call alone on its line: take the following indented block as
        ;; its body. Blank spacer lines between the call and its block are
        ;; consumed (they separate nothing); without a deeper line the call
        ;; stays bare and the blanks remain paragraph breaks.
        (skip-hspace in)
        (consume-newline in)
        (define-values (nd nskip) (peek-next-line-depth in))
        (cond
          [(and nd (>= nd (add1 depth)))
           (consume-chars in nskip) ; the blank spacer lines (ASCII, 1 byte each)
           (define bbody (read-block-body src in depth breader name))
           (list (assemble-call src in name args (list bbody) line col pos))]
          [else
           (list (assemble-call src in name args bodies line col pos) "\n")])]
       [else
        ;; Inline body and/or trailing text: continue reading the line.
        (read-text-line src in
                        (list (assemble-call src in name args bodies line col pos)))])]))

(define (assemble-call src in name args bodies line col pos)
  (define-values (eline ecol epos) (port-next-location in))
  (loc-syntax src
              (cons name (append args bodies))
              line col pos (and pos epos (- epos pos))))

;; ---------------------------------------------------------------------------
;; Block / hanging bodies

;; Indented block body of a bare call; dispatched by the call's body reader.
(define (read-block-body src in depth breader name)
  (define child (add1 depth))
  (define parse (body-reader-parse breader))
  (wrap-body
   breader
   (case parse
     [(markup) (cons 'yamd-body (read-block src in child))]
     [(raw) (read-raw-block in child child)]
     [else
      (define-values (line col pos) (port-next-location in))
      (define text (read-raw-block in child child))
      (call-custom-parser parse text
                          (srcloc src line col pos (string-length text)))])))

;; `::` hanging body: the rest of the enclosing block (same indent level).
;; Recursive nesting falls out naturally — a later `::` lives inside the
;; remainder captured by the earlier one.
(define (read-hanging-body src in depth breader name)
  (define parse (body-reader-parse breader))
  (wrap-body
   breader
   (case parse
     [(markup) (cons 'yamd-body (read-block src in depth))]
     [(raw) (read-raw-block in depth depth)]
     [else
      (define-values (line col pos) (port-next-location in))
      (define text (read-raw-block in depth depth))
      (call-custom-parser parse text
                          (srcloc src line col pos (string-length text)))])))

;; Verbatim block text: lines with `strip` leading tabs removed, gathered
;; until a non-blank line with fewer than `stop` tabs (or eof). Interior
;; blank lines are kept; trailing blank lines are dropped. No delimiter or
;; `#` scanning at all. Only the block's structural tabs are structure:
;; everything after them on a content line — leading spaces, deeper tabs — is
;; verbatim, so we count leading tabs WITHOUT the markup space-indent error
;; (see raw-line-leading-tabs). `strip` always equals `stop` (both callers
;; pass the same value), so a content line has at least `strip` tabs to peel.
;; A whitespace-only line is blank regardless of any spaces on it (blank
;; lines never close a block); its verbatim content normalizes to empty.
(define (read-raw-block in strip stop)
  (let loop ([acc '()] [pending 0])
    (cond
      [(eof-object? (peek-char in))
       (string-join (reverse acc) "\n")]
      [(line-blank? in)
       (consume-line in)
       (loop acc (add1 pending))]
      [else
       (define d (raw-line-leading-tabs in))
       (cond
         [(< d stop) (string-join (reverse acc) "\n")]
         [else
          (define text (read-stripped-line in strip))
          (loop (cons text (append (make-list pending "") acc)) 0)])])))

;; Consumes `strip` leading tabs then the rest of the line (verbatim, keeping
;; any deeper tabs) and its terminating newline; returns the line content.
(define (read-stripped-line in strip)
  (consume-chars in strip)
  (define out (open-output-string))
  (let loop ()
    (define ch (peek-char in))
    (cond
      [(or (eof-object? ch) (eqv? ch #\newline))
       (when (eqv? ch #\newline) (read-char in))
       (get-output-string out)]
      [else (write-char (read-char in) out) (loop)])))

;; ---------------------------------------------------------------------------
;; Host escapes

;; Reads `(datum)` from the port with location info; used for both (args)
;; and #(expr). Consumes the `(`-form; may span lines. The host readtable is
;; installed so `#name{markup}` re-enters text mode as a Content expression
;; (see "Host-mode markup re-entry" below).
(define (read-paren-datum src in)
  (define stx (parameterize ([current-readtable host-readtable])
                (read-syntax src in)))
  (when (eof-object? stx)
    (reader-error src in "unexpected eof while reading host expression"))
  stx)

;; `#(...)`: caller has seen `#` `(`; consumes `#` then the datum. The parens
;; delimit host code: a single datum inside is the expression itself
;; (`#(price)` is a variable reference), multiple datums form the compound
;; form (`#(define price 42)`, `#(item-name x)`).
(define (read-host-escape src in)
  (read-char in) ; #
  (unwrap-host-datum src in (read-paren-datum src in)))

;; ---------------------------------------------------------------------------
;; Host-mode markup re-entry (Scribble semantics, DESIGN "Host mode").
;;
;; Inside host code — the `(args)` of a call and the `#(expr)` escape — a
;; `#name{markup}` form re-enters text mode and reads as an expression that
;; evaluates to a Content value. This is implemented with a readtable that
;; overrides `#`: `read-paren-datum` reads the host s-expression with it
;; installed, so every `#` the Racket reader meets while scanning host code is
;; routed through `host-reentry` first.
;;
;; Supported subset (documented in docs/architecture.md):
;;   #name{body}          — markup body (re-enters text mode; nestable)
;;   #name(args){body}    — args + one-or-more markup bodies
;;   #name(args)          — args, no body (keyword `key:` sugar applies)
;; The body reader dispatches exactly as in text mode (markup / raw / custom /
;; lazy), so `#m{...}` in host code still hits the AsciiMath parser, etc.
;; Fences `|{ ... }|` are accepted as an alternative body delimiter. Line-
;; oriented body forms (`: rest-of-line`, `::` hanging) are NOT available in
;; host mode — there is no "rest of line" inside an s-expression.
;;
;; Collision policy — everything else after `#` falls back verbatim to
;; Racket's default reader (booleans `#t`/`#f`, chars `#\c`, keywords `#:kw`,
;; numbers `#x1F`, regexps `#px"…"`, block/datum comments `#| |#` / `#;`,
;; vectors `#(…)`, hashes `#hash(…)`, prefab structs `#s(…)`, …). The trigger
;; is a name followed by `{`, `(`, or a fence opener `|=*{` — UNLESS the name
;; is reserved (`reserved-hash-name?`): Racket reader tokens shaped `#word`
;; keep their native meaning before ALL of those openers, because `{` is a
;; paren-equivalent in Racket's reader (`#hash{…}`, `#s{…}`, `#t{a}` are
;; valid Racket). A bare `#name` in host code is deliberately NOT re-entry:
;; it would collide with `#t`/`#f`/`#px…`, and inside host mode a zero-arg
;; call is just `(name)`.
;; Note: re-entry is recognized only in host code proper; inside a Racket
;; literal read via fallback (e.g. `#(vector …)` or `#hash(…)`) the default
;; readtable is in force and `#name{}` is not recognized there.

;; Racket reader tokens of the form `#word` that must keep their native
;; meaning when a body/args opener follows (see the collision policy above).
;; Kept lowercase; lookup downcases (Racket accepts e.g. `#Ci(`).
(define reserved-hash-names
  (for/hash ([s (in-list '("t" "f" "true" "false"
                           "hash" "hasheq" "hasheqv" "hashalw"
                           "s" "fl" "fx" "cs" "ci" "reader" "lang"
                           "e" "i" "x" "o" "b" "d" "rx" "px"))])
    (values s #t)))

;; True iff `nm` (the peeked name after `#`) must be left to Racket's reader:
;; a reserved token, a sized flvector/fxvector literal (`#fl3(…)`/`#fx2(…)`),
;; or a name that lexes as a Racket number literal (`#x1F`, `#e5`, `#b101`,
;; `#x1/2` — a number-prefix letter followed by number-ish chars including at
;; least one decimal digit). Best effort on the number forms: a literal with
;; no decimal digit (`#xFF`) or an exotic one (`#i+inf.0`) directly followed
;; by `(`/`{` is NOT recognized and misreads as re-entry — put a space after
;; a number literal (documented in docs/architecture.md).
(define (reserved-hash-name? nm)
  (define s (string-downcase nm))
  (or (hash-ref reserved-hash-names s #f)
      (regexp-match? #px"^(?:fl|fx)[0-9]+$" s)
      (regexp-match? #px"^[eixobd](?=[0-9a-f.+/-]*[0-9])[0-9a-f.+/-]+$" s)))

;; Peeks (does not consume) the run of yamd-name chars starting at the port.
(define (peek-name-string in)
  (let loop ([k 0] [acc '()])
    (define ch (peek-char in k))
    (if (yamd-name-char? ch)
        (loop (add1 k) (cons ch acc))
        (list->string (reverse acc)))))

;; True iff a fence opener `|=*{` begins at peek offset `start` (which is a
;; `#\|`). Generalizes `peek-fence-open` to an arbitrary offset.
(define (peek-fence-open-at in start)
  (let loop ([k (add1 start)])
    (define ch (peek-char in k))
    (cond
      [(eqv? ch #\=) (loop (add1 k))]
      [(eqv? ch #\{) #t]
      [else #f])))

;; Port is positioned just after `#`. True iff a YAMD re-entry call begins
;; here (peek-only; consumes nothing).
(define (host-reentry-trigger? in)
  (define c0 (peek-char in))
  (and (char? c0) (char-alphabetic? c0)
       (let ([nm (peek-name-string in)])
         (define k (string-length nm))
         (define follow (peek-char in k))
         (and (not (reserved-hash-name? nm))
              (or (eqv? follow #\{)
                  (eqv? follow #\()
                  (and (eqv? follow #\|) (peek-fence-open-at in k)))))))

;; The `#` reader-macro used while scanning host code. `ch` is `#`; the port
;; is positioned just after it. Either read a YAMD re-entry call or hand the
;; form back to Racket's default reader (reinserting the consumed `#`). The
;; fallback parameterizes current-readtable to #f: the readtable argument of
;; read-syntax/recursive applies only to the LEADING token — nested elements
;; (e.g. inside a `#(…)` vector or `#hash(…)` literal) are read with
;; current-readtable, and a self-quoting literal must not have re-entry fire
;; inside it (the call would be embedded as an unevaluated datum).
;; Consequence: a `#;`-commented re-entry form inside host code is a read
;; error, not skipped — use `#|…|#` instead.
(define (host-reentry* ch in src line col pos)
  (if (host-reentry-trigger? in)
      (read-host-call src in line col pos)
      (parameterize ([current-readtable #f])
        (read-syntax/recursive src in #\# #f))))

;; Readtable procs must accept the 2-arg (`read`) and 6-arg (`read-syntax`)
;; arities; the reader only ever uses `read-syntax`, but be robust: in plain
;; `read` mode the proc must return a datum, so strip the syntax wrapper.
(define host-reentry-proc
  (case-lambda
    [(ch in)
     (define v (host-reentry* ch in (object-name in) #f #f #f))
     (if (syntax? v) (syntax->datum v) v)]
    [(ch in src line col pos) (host-reentry* ch in src line col pos)]))

(define host-readtable
  (make-readtable #f #\# 'non-terminating-macro host-reentry-proc))

;; Reads `#name`, optional `(args)`, and any `{body}` / fence bodies (the `#`
;; already consumed, port at the name start). Line-oriented `:`/`::` bodies do
;; not apply in host mode.
(define (read-host-call src in line col pos)
  (define name (read-name src in))
  (when (eq? name 'use)
    (reader-error/loc src line col
      "#use is not allowed in host mode; put it on its own line at markup level"))
  (define breader (lookup-body-reader name))
  (define args
    (if (eqv? (peek-char in) #\()
        (rewrite-keyword-sugar (read-paren-datum src in))
        '()))
  (define bodies (read-host-bodies src in name breader))
  (assemble-call src in name args bodies line col pos))

(define (read-host-bodies src in name breader)
  (let loop ([bodies '()])
    (define ch (peek-char in))
    (cond
      [(eqv? ch #\{)
       (loop (cons (read-inline-body src in name breader) bodies))]
      [(and (eqv? ch #\|) (peek-fence-open in))
       (loop (cons (read-fence-body src in name breader) bodies))]
      [else (reverse bodies)])))

(define (unwrap-host-datum src in stx)
  (define elems (syntax->list stx))
  (cond
    [(and elems (null? elems))
     (reader-error src in "empty host escape `#()`")]
    [(and elems (null? (cdr elems))) (car elems)]
    [else stx]))

;; ---------------------------------------------------------------------------
;; Text lines / inline scanning

;; Reads until end of line, returning a list of expressions (strings and
;; calls) followed by a "\n" separator string. `seed` is prepended content.
(define (read-text-line src in seed)
  (let loop ([acc (reverse seed)] [buf '()])
    (define (flush)
      (if (null? buf) acc (cons (list->string (reverse buf)) acc)))
    (define ch (peek-char in))
    (cond
      [(eof-object? ch) (reverse (cons "\n" (flush)))]
      [(eqv? ch #\newline)
       (read-char in)
       (reverse (cons "\n" (flush)))]
      [(eqv? ch #\#)
       (define parts (read-hash-form src in))
       (loop (append (reverse parts) (flush)) '())]
      [else
       (read-char in)
       (loop acc (cons ch buf))])))

(define (yamd-name-char? ch)
  (and (char? ch)
       (or (char-alphabetic? ch)
           (char-numeric? ch)
           (memv ch '(#\- #\/)))))

;; Dispatches on the character after `#`. Returns a list of expressions to
;; splice into the current content sequence (possibly empty, for comments).
(define (read-hash-form src in)
  (define-values (line col pos) (port-next-location in))
  (read-char in) ; #
  (define ch (peek-char in))
  (cond
    [(eqv? ch #\#) (read-char in) (list "#")]
    [(eqv? ch #\;) (read-char in) (skip-comment src in) '()]
    [(or (eof-object? ch) (eqv? ch #\space) (eqv? ch #\newline))
     (skip-line-comment in)
     '()]
    [(eqv? ch #\()
     ;; Emit the host datum RAW: whatever consumes this form — top-level
     ;; eval, or the yamd-body splicing macro for `{...}`/block bodies —
     ;; applies ->content to an expression's value and hoists a definition.
     ;; That is what lets `#(define ...)` work in any markup body position.
     (list (unwrap-host-datum src in (read-paren-datum src in)))]
    [(and (char? ch) (char-alphabetic? ch))
     (define-values (kind payload) (read-call-name+args src in line col pos))
     (cond
       [(eq? kind 'use) (list payload)]
       [else
        (define name (car payload))
        (define breader (cadr payload))
        (define args (caddr payload))
        (define-values (bodies bkind) (read-inline-bodies src in name breader))
        (when (eq? bkind 'hanging)
          (reader-error src in
            "`::` hanging block is only valid on its own line, not inline"))
        (list (assemble-call src in name args bodies line col pos))])]
    [else
     (reader-error src in "stray `#` before ~v (use `##` for a literal `#`)" ch)]))

(define (skip-line-comment in)
  (let loop ()
    (define ch (peek-char in))
    (unless (or (eof-object? ch) (eqv? ch #\newline))
      (read-char in)
      (loop))))

;; After `#;`: `{...}` balanced block comment (may span lines), else to EOL.
(define (skip-comment src in)
  (cond
    [(eqv? (peek-char in) #\{)
     (define-values (oline ocol opos) (port-next-location in))
     (read-char in)
     (let loop ([depth 1])
       (define ch (read-char in))
       (cond
         [(eof-object? ch)
          (reader-error/loc src oline ocol "unterminated `#;{` comment")]
         [(eqv? ch #\{) (loop (add1 depth))]
         [(eqv? ch #\}) (unless (= depth 1) (loop (sub1 depth)))]
         [else (loop depth)]))]
    [else (skip-line-comment in)]))

;; ---------------------------------------------------------------------------
;; Calls

;; Reads a call's name and optional (args) (the `#` already consumed). `#use`
;; is a reader directive, not an ordinary call — it is handled here.
;; Returns (values 'use  use-syntax)
;;      or (values 'call (list name body-reader args)).
(define (read-call-name+args src in line col pos)
  (define name (read-name src in))
  (cond
    [(eq? name 'use) (values 'use (read-use src in line col pos))]
    [else
     (define breader (lookup-body-reader name))
     (define args
       (if (eqv? (peek-char in) #\()
           (rewrite-keyword-sugar (read-paren-datum src in))
           '()))
     (values 'call (list name breader args))]))

(define (read-name src in)
  (let loop ([chars '()])
    (define ch (peek-char in))
    (cond
      [(yamd-name-char? ch) (read-char in) (loop (cons ch chars))]
      [(null? chars) (reader-error src in "expected a name after `#`")]
      [else (string->symbol (list->string (reverse chars)))])))

;; `key: val` sugar in args: an identifier ending in `:` becomes a keyword.
(define (rewrite-keyword-sugar args-stx)
  (define elems (syntax->list args-stx))
  (unless elems (error 'yamd-reader "args must be a proper list: ~s" args-stx))
  (for/list ([e (in-list elems)])
    (define d (syntax-e e))
    (define s (and (symbol? d) (symbol->string d)))
    (cond
      ;; require at least one char before the `:` — a bare `:` stays a symbol
      [(and s (> (string-length s) 1)
            (char=? #\: (string-ref s (sub1 (string-length s)))))
       (datum->syntax e
                      (string->keyword (substring s 0 (sub1 (string-length s))))
                      e e)]
      [else e])))

;; Zero or more inline bodies: repeated `{...}` and/or `|{...}|` fences
;; (multi-body, e.g. #row{a}{b}), or a single `: rest-of-line`, or `::`
;; hanging. Returns (values bodies kind) where kind is 'none | 'inline |
;; 'hanging.
(define (read-inline-bodies src in name breader #:line-start? [line-start? #f])
  (let loop ([bodies '()])
    (define ch (peek-char in))
    (cond
      [(eqv? ch #\{)
       (loop (cons (read-inline-body src in name breader) bodies))]
      [(and (eqv? ch #\|) (peek-fence-open in))
       (loop (cons (read-fence-body src in name breader) bodies))]
      [(and (or (null? bodies) line-start?)
            (eqv? ch #\:) (eqv? (peek-char in 1) #\:))
       ;; When the call starts its line, `::` may follow inline `{...}` bodies
       ;; (`#section{Title}::` per DESIGN "Hanging blocks"): those bodies are
       ;; kept and the hanging body is appended by the caller (read-line-call).
       ;; Mid-line, `::` after a body is plain text (`#tt{std}::vector`), so
       ;; only the no-bodies case reaches here — and read-hash-form rejects it
       ;; as an inline hanging block.
       (read-char in) (read-char in) ; ::
       (skip-hspace in)
       (define nx (peek-char in))
       (unless (or (eof-object? nx) (eqv? nx #\newline))
         (reader-error src in "nothing may follow `::` on the line"))
       (consume-newline in)
       (values (reverse bodies) 'hanging)]
      [(and (null? bodies) (eqv? ch #\:))
       (read-char in) ; :
       (skip-hspace in)
       (values (list (wrap-body breader (rest-of-line-body src in name breader)))
               'inline)]
      [else
       (values (reverse bodies) (if (null? bodies) 'none 'inline))])))

(define (wrap-body breader body-expr)
  (if (body-reader-lazy? breader)
      (list 'lambda '() body-expr)
      body-expr))

;; Runs a custom body parser under the default readtable: during host-mode
;; re-entry the host readtable is installed for the dynamic extent of
;; read-paren-datum, and a parser that itself calls `read` must not observe
;; the overridden `#` (nor any ambient readtable of our caller).
(define (call-custom-parser parse text loc)
  (parameterize ([current-readtable #f])
    (parse text loc)))

;; Rest-of-line bodies leave the terminating newline in the port so the
;; enclosing text line still emits its "\n" separator.
(define (rest-of-line-body src in name breader)
  (case (body-reader-parse breader)
    [(markup) (cons 'yamd-body (read-markup-until-eol src in))]
    [(raw) (read-raw-until-eol in)]
    [else
     (define-values (line col pos) (port-next-location in))
     (define text (read-raw-until-eol in))
     (call-custom-parser (body-reader-parse breader) text
                         (srcloc src line col pos (string-length text)))]))

(define (read-raw-until-eol in)
  (define out (open-output-string))
  (let loop ()
    (define ch (peek-char in))
    (unless (or (eof-object? ch) (eqv? ch #\newline))
      (write-char (read-char in) out)
      (loop)))
  (get-output-string out))

(define (read-markup-until-eol src in)
  (let loop ([acc '()] [buf '()])
    (define (flush)
      (if (null? buf) acc (cons (list->string (reverse buf)) acc)))
    (define ch (peek-char in))
    (cond
      [(or (eof-object? ch) (eqv? ch #\newline)) (reverse (flush))]
      [(eqv? ch #\#)
       (define parts (read-hash-form src in))
       (loop (append (reverse parts) (flush)) '())]
      [else
       (read-char in)
       (loop acc (cons ch buf))])))

;; `{...}`: markup bodies re-enter the inline parser (nestable, may span
;; lines); raw/custom bodies are a brace-balanced verbatim scan with no `#`
;; recognition.
(define (read-inline-body src in name breader)
  (define-values (line col pos) (port-next-location in))
  (read-char in) ; {
  (define parse (body-reader-parse breader))
  (wrap-body
   breader
   (case parse
     [(markup) (cons 'yamd-body (read-inline-markup src in name line col))]
     [(raw) (read-balanced-text src in name line col)]
     [else
      (define text (read-balanced-text src in name line col))
      (call-custom-parser parse text
                          (srcloc src line col pos (string-length text)))])))

(define (read-inline-markup src in name oline ocol)
  (let loop ([acc '()] [buf '()] [depth 0])
    (define (flush)
      (if (null? buf) acc (cons (list->string (reverse buf)) acc)))
    (define ch (peek-char in))
    (cond
      [(eof-object? ch)
       (reader-error/loc src oline ocol "unterminated `{` body of #~a" name)]
      [(eqv? ch #\})
       (read-char in)
       (if (zero? depth)
           (reverse (flush))
           (loop acc (cons ch buf) (sub1 depth)))]
      [(eqv? ch #\{)
       (read-char in)
       (loop acc (cons ch buf) (add1 depth))]
      [(eqv? ch #\#)
       (define parts (read-hash-form src in))
       (loop (append (reverse parts) (flush)) '() depth)]
      [else
       (read-char in)
       (loop acc (cons ch buf) depth)])))

(define (read-balanced-text src in name oline ocol)
  (define out (open-output-string))
  (let loop ([depth 0])
    (define ch (read-char in))
    (cond
      [(eof-object? ch)
       (reader-error/loc src oline ocol "unterminated raw `{` body of #~a" name)]
      [(eqv? ch #\})
       (unless (zero? depth)
         (write-char ch out)
         (loop (sub1 depth)))]
      [(eqv? ch #\{)
       (write-char ch out)
       (loop (add1 depth))]
      [else
       (write-char ch out)
       (loop depth)]))
  (get-output-string out))

;; ---------------------------------------------------------------------------
;; Fences: `|{ ... }|`, `|==={ ... }===|`. Opener `|X{` (X a possibly-empty
;; run of `=`) closes only at `}X|`; raw scan, no `#` recognition, no brace
;; counting. Regardless of the body reader kind the content is captured
;; verbatim (markup body readers receive the literal string, not re-parsed).

;; The next char is `#\|`; true iff it begins a fence opener `|=*{`.
(define (peek-fence-open in)
  (let loop ([k 1])
    (define ch (peek-char in k))
    (cond
      [(eqv? ch #\=) (loop (add1 k))]
      [(eqv? ch #\{) #t]
      [else #f])))

(define (read-fence-body src in name breader)
  (define-values (line col pos) (port-next-location in))
  (read-char in) ; |
  (define eqs
    (let loop ([n 0])
      (if (eqv? (peek-char in) #\=)
          (begin (read-char in) (loop (add1 n)))
          n)))
  (read-char in) ; {
  (define close (string-append "}" (make-string eqs #\=) "|"))
  (define text (read-until-literal src in close line col name))
  (wrap-body
   breader
   (case (body-reader-parse breader)
     [(markup) (cons 'yamd-body (list text))]
     [(raw) text]
     [else
      (call-custom-parser (body-reader-parse breader) text
                          (srcloc src line col pos (string-length text)))])))

(define (read-until-literal src in close oline ocol name)
  (define out (open-output-string))
  (let loop ()
    (define ch (peek-char in))
    (cond
      [(eof-object? ch)
       (reader-error/loc src oline ocol "unterminated `|{` fence body of #~a" name)]
      [(and (eqv? ch (string-ref close 0)) (fence-close-here? in close))
       (consume-chars in (string-length close))
       (get-output-string out)]
      [else (write-char (read-char in) out) (loop)])))

(define (fence-close-here? in close)
  (let loop ([k 0])
    (cond
      [(= k (string-length close)) #t]
      [(eqv? (peek-char in k) (string-ref close k)) (loop (add1 k))]
      [else #f])))

;; ---------------------------------------------------------------------------
;; #use(...) : import modules and merge their reader submodule body readers.

(define (read-use src in line col pos)
  (unless (eqv? (peek-char in) #\()
    (reader-error src in "#use requires a parenthesized module list"))
  (define specs (syntax->list (read-paren-datum src in)))
  (unless specs
    (reader-error src in "#use module list must be a proper list"))
  (define modpaths (parse-use-specs src specs))
  ;; Read-time effect: merge each module's `yamd-reader` body readers so the
  ;; rest of THIS file reads plugin bodies correctly.
  (for ([mp (in-list modpaths)])
    (merge-reader-submodule! mp))
  ;; Runtime form: import the modules' functions into the document namespace.
  (loc-syntax src
              (cons 'yamd-use (map (lambda (mp) (list 'quote mp)) modpaths))
              line col pos #f))

;; Maps a symbol module spec to a Racket module path datum:
;;   std/x -> yamd/std/x ; any other symbol is a verbatim collection path.
(define (map-module-symbol sym)
  (define s (symbol->string sym))
  (if (string-prefix? s "std/")
      (string->symbol (string-append "yamd/" s))
      sym))

(define (variant-key-symbol? d)
  (and (symbol? d)
       (let ([s (symbol->string d)])
         (and (> (string-length s) 1)
              (char=? #\: (string-ref s (sub1 (string-length s))))))))

;; Strip the trailing `:` of a `mod:` variant key.
(define (strip-trailing-colon d)
  (define s (symbol->string d))
  (substring s 0 (sub1 (string-length s))))

;; A string spec is a module file path relative to the document's directory.
(define (use-file-path src str)
  (define dir
    (if (path-string? src)
        (let-values ([(base name must?) (split-path (path->complete-path src))])
          (if (path? base) base (current-directory)))
        (current-directory)))
  (list 'file (path->string (path->complete-path str dir))))

;; Commas between #use specs are allowed (DESIGN: `#use(std/table,
;; std/math: asciimath)`). `read-syntax` turns `, x` into `(unquote x)` —
;; peel that wrapper before dispatching on the spec.
(define (strip-unquote s)
  (define l (syntax->list s))
  (if (and l (= 2 (length l))
           (identifier? (car l))
           (eq? 'unquote (syntax-e (car l))))
      (strip-unquote (cadr l))
      s))

(define (parse-use-specs src specs)
  (let loop ([specs specs] [acc '()])
    (cond
      [(null? specs) (reverse acc)]
      [else
       (define s (strip-unquote (car specs)))
       (define d (syntax-e s))
       (cond
         [(string? d)
          (loop (cdr specs) (cons (use-file-path src d) acc))]
         [(variant-key-symbol? d)
          (when (null? (cdr specs))
            (reader-error/loc src (syntax-line s) (syntax-column s)
                              "#use `mod:` variant key needs a following variant name"))
          (define variant-stx (strip-unquote (cadr specs)))
          (define variant (syntax-e variant-stx))
          (unless (symbol? variant)
            (reader-error/loc src (syntax-line variant-stx) (syntax-column variant-stx)
                              "#use variant name must be a symbol, got ~s" variant))
          (define combined
            (string->symbol
             (string-append (strip-trailing-colon d) "/" (symbol->string variant))))
          (loop (cddr specs) (cons (map-module-symbol combined) acc))]
         [(symbol? d)
          (loop (cdr specs) (cons (map-module-symbol d) acc))]
         [else
          (reader-error/loc src (syntax-line s) (syntax-column s)
                            "unsupported #use module spec: ~s" d)])])))

(define (merge-reader-submodule! mp)
  (define subm (list 'submod mp 'yamd-reader))
  (when (module-declared? subm #t)
    (define brs (dynamic-require subm 'body-readers))
    (merge-body-readers! brs)))
