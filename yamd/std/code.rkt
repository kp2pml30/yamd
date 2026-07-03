#lang racket/base
;; std/code — syntax-highlighting for code blocks and inline code.
;;
;; Usage:
;;   #use(std/code)
;;   #code(lang: "python")        ; block: raw body, verbatim, tabs dedented
;;       def f():  # a comment, not a call
;;           return {"x": 1}
;;   Inline #code{x = 1} keeps braces/`#` raw.
;;
;; Output IR: a block form emits <pre><code class="language-<lang>">…</code></pre>;
;; the inline form emits a bare <code>…</code>. Highlighted spans are
;; <span class="tok-<kind>">…</span>; runs the lexer leaves unclassified stay
;; as plain (escaped) text, so whitespace/punctuation is not span-wrapped.
;;
;; Block vs inline is decided by the presence of the `lang:` keyword — this
;; matches the two documented forms (`#code(lang: ...)` block, `#code{...}`
;; inline). A plain (unhighlighted) *block* is written `#code(lang: "plain")`.
;;
;; Lexers are total: any input tokenizes; an unrecognized character becomes a
;; plain-text token, never an error. The only error paths are a non-string
;; body (impossible via the raw reader) and an unknown `lang:`.

(require racket/string
         "../runtime.rkt")

(provide code)

;; --------------------------------------------------------------------------
;; Generic driver: scan `str` left-to-right, at each position taking the first
;; rule (kind . anchored-rx) whose regexp matches a non-empty prefix; if none
;; match, consume a single character as a 'text token. Total by construction.
;; The empty #"" input-prefix makes `^` anchor at `pos` rather than at the
;; string start, so matching happens in place without rebuilding tail
;; substrings (keeps the scan linear).

(define (lex-with rules str)
  (define len (string-length str))
  (let loop ([pos 0] [acc '()])
    (cond
      [(>= pos len) (coalesce (reverse acc))]
      [else
       (let rule-loop ([rs rules])
         (cond
           [(null? rs)
            (loop (add1 pos)
                  (cons (cons 'text (substring str pos (add1 pos))) acc))]
           [else
            (define m (regexp-match (cdar rs) str pos #f #f #""))
            (cond
              [(and m (positive? (string-length (car m))))
               (loop (+ pos (string-length (car m)))
                     (cons (cons (caar rs) (car m)) acc))]
              [else (rule-loop (cdr rs))])]))])))

;; Merge adjacent tokens of the same kind so highlighting yields the fewest
;; spans and goldens stay readable.
(define (coalesce toks)
  (cond
    [(null? toks) '()]
    [else
     (let loop ([rest (cdr toks)] [k (caar toks)] [s (cdar toks)] [acc '()])
       (cond
         [(null? rest) (reverse (cons (cons k s) acc))]
         [(eq? (caar rest) k)
          (loop (cdr rest) k (string-append s (cdar rest)) acc)]
         [else
          (loop (cdr rest) (caar rest) (cdar rest) (cons (cons k s) acc))]))]))

;; --------------------------------------------------------------------------
;; Python: regex-level. Kinds: comment, string, number, keyword; rest text.

(define python-rules
  (list
   (cons 'text    #px"^[ \t\r\n]+")
   (cons 'comment #px"^#[^\n]*")
   (cons 'string  #px"^\"\"\"(?:.|\n)*?\"\"\"")
   (cons 'string  #px"^'''(?:.|\n)*?'''")
   (cons 'string  #px"^\"(?:\\\\.|[^\"\\\\\n])*\"")
   (cons 'string  #px"^'(?:\\\\.|[^'\\\\\n])*'")
   (cons 'number  #px"^[0-9][0-9_]*\\.?[0-9]*(?:[eE][+-]?[0-9]+)?")
   (cons 'keyword #px"^(?:False|None|True|and|as|assert|async|await|break|class|continue|def|del|elif|else|except|finally|for|from|global|if|import|in|is|lambda|nonlocal|not|or|pass|raise|return|try|while|with|yield)\\b")
   (cons 'text    #px"^[A-Za-z_][A-Za-z0-9_]*")))

(define (python-lexer str) (lex-with python-rules str))

;; --------------------------------------------------------------------------
;; Racket: Lisp-style. Grab whole symbol tokens, then classify against a
;; keyword set / number shape. Keywords are not reserved, so this is
;; approximate by design (a `define` used as a value still reads as keyword).

(define racket-keywords
  (for/hash ([k (in-list
                 '("define" "define-values" "define-syntax" "define-syntax-rule"
                   "lambda" "λ" "let" "let*" "letrec" "let-values" "let*-values"
                   "cond" "case" "if" "when" "unless" "begin" "begin0" "and" "or"
                   "not" "set!" "quote" "quasiquote" "unquote" "else"
                   "require" "provide" "module" "module+" "module*"
                   "struct" "define-struct" "syntax-rules" "syntax-case"
                   "for" "for/list" "for/fold" "for/hash" "for/vector"
                   "match" "match-define" "parameterize" "with-handlers"
                   "#%module-begin" "#lang"))])
    (values k #t)))

(define racket-rules
  (list
   (cons 'text     #px"^[ \t\r\n]+")
   (cons 'comment  #px"^;[^\n]*")
   (cons 'comment  #px"^#\\|(?:.|\n)*?\\|#")
   (cons 'string   #px"^\"(?:\\\\.|[^\"\\\\])*\"")
   (cons 'constant #px"^#(?:t|f|true|false)\\b")
   (cons 'text     #px"^[()\\[\\]{}'`,]")
   (cons 'symbol   #px"^[^()\\[\\]{}\"'`,;\\s]+")))

(define (classify-racket-symbol s)
  (cond
    [(hash-ref racket-keywords s #f) 'keyword]
    [(regexp-match? #px"^[+-]?[0-9][0-9.eE+/-]*$" s) 'number]
    [else 'text]))

(define (racket-lexer str)
  (coalesce
   (for/list ([tok (in-list (lex-with racket-rules str))])
     (if (eq? (car tok) 'symbol)
         (cons (classify-racket-symbol (cdr tok)) (cdr tok))
         tok))))

;; --------------------------------------------------------------------------
;; GDScript: Godot's python-like language. Same structure as the python lexer
;; (indentation-based, `#` comments, def-ish keywords), plus GDScript-specific
;; kinds:
;;   annotation — `@export`, `@onready`, `@tool`, `@export_range` …
;;   nodepath   — `$Node`, `$Path/To/Node`, `$"quoted"`, `%UniqueName`, `%"…"`
;; and prefixed string literals `&"StringName"` / `^"NodePath"` (kept as
;; strings, prefix included). `true`/`false`/`null` and the built-in float
;; consts (PI/TAU/INF/NAN) are `constant`, matching how the racket lexer tags
;; `#t`/`#f`.
;;
;; Approximations (documented, never fatal — this is a regex lexer): `%` is the
;; modulo operator too, so a space-less `a%b` misreads `%b` as a nodepath; the
;; `&`/`^` bitwise operators likewise only collide when written flush against a
;; quote (`a^"x"`), which idiomatic code never does. Unknown constructs fall
;; through to plain text, never an error.

(define gdscript-rules
  (list
   (cons 'text       #px"^[ \t\r\n]+")
   (cons 'comment    #px"^#[^\n]*")
   ;; triple-quoted (docstrings / multiline)
   (cons 'string     #px"^\"\"\"(?:.|\n)*?\"\"\"")
   (cons 'string     #px"^'''(?:.|\n)*?'''")
   ;; StringName &"…" / NodePath ^"…" prefixed literals (prefix kept)
   (cons 'string     #px"^&\"(?:\\\\.|[^\"\\\\\n])*\"")
   (cons 'string     #px"^&'(?:\\\\.|[^'\\\\\n])*'")
   (cons 'string     #px"^\\^\"(?:\\\\.|[^\"\\\\\n])*\"")
   (cons 'string     #px"^\\^'(?:\\\\.|[^'\\\\\n])*'")
   ;; plain single/double
   (cons 'string     #px"^\"(?:\\\\.|[^\"\\\\\n])*\"")
   (cons 'string     #px"^'(?:\\\\.|[^'\\\\\n])*'")
   ;; numbers: hex, binary, then decimal/float (underscore separators allowed)
   (cons 'number     #px"^0[xX][0-9a-fA-F_]+")
   (cons 'number     #px"^0[bB][01_]+")
   (cons 'number     #px"^[0-9][0-9_]*\\.?[0-9_]*(?:[eE][+-]?[0-9]+)?")
   ;; annotations: @export, @onready, @export_range …
   (cons 'annotation #px"^@[A-Za-z_][A-Za-z0-9_]*")
   ;; node paths: $Node, $Path/To, $"quoted", %Unique, %"quoted"
   (cons 'nodepath   #px"^\\$(?:\"(?:\\\\.|[^\"\\\\\n])*\"|[A-Za-z_][A-Za-z0-9_/]*)?")
   (cons 'nodepath   #px"^%(?:\"(?:\\\\.|[^\"\\\\\n])*\"|[A-Za-z_][A-Za-z0-9_]*)")
   (cons 'keyword    #px"^(?:breakpoint|break|class_name|class|const|continue|elif|else|enum|extends|for|func|if|in|is|match|namespace|not|or|pass|preload|return|self|signal|static|super|trait|tool|var|void|while|when|yield|and|as|assert|await)\\b")
   (cons 'constant   #px"^(?:true|false|null|PI|TAU|INF|NAN)\\b")
   (cons 'text       #px"^[A-Za-z_][A-Za-z0-9_]*")))

(define (gdscript-lexer str) (lex-with gdscript-rules str))

;; --------------------------------------------------------------------------
;; Plain: one text token, no highlighting.

(define (plain-lexer str)
  (if (zero? (string-length str)) '() (list (cons 'text str))))

(define lexers
  (hash "racket"   racket-lexer
        "python"   python-lexer
        "gdscript" gdscript-lexer
        "plain"    plain-lexer))

(define (known-langs)
  (string-join (sort (hash-keys lexers) string<?) ", "))

;; --------------------------------------------------------------------------
;; Rendering: tokens -> Content. 'text tokens pass through as plain strings
;; (the HTML backend escapes them); every other kind becomes a tok-<kind> span.

(define (render-tokens tokens)
  (for/list ([tok (in-list tokens)])
    (define kind (car tok))
    (define str (cdr tok))
    (if (eq? kind 'text)
        str
        (elem 'span
              #:attrs (list (cons 'class (string-append "tok-" (symbol->string kind))))
              str))))

(define (normalize-lang lang)
  (cond
    [(string? lang) lang]
    [(symbol? lang) (symbol->string lang)]
    [else (error 'code "lang: must be a string, got ~e" lang)]))

;; The runtime function. `text` is the raw body (string, from the raw reader).
;; `#:lang` present => highlighted block; absent => inline plain code.
(define (code text #:lang [lang #f])
  (unless (string? text)
    (error 'code "expected a string body, got ~e" text))
  (cond
    [lang
     (define lname (normalize-lang lang))
     (define lexer
       (hash-ref lexers lname
                 (lambda ()
                   (error 'code "unknown lang ~s; known langs: ~a"
                          lname (known-langs)))))
     (elem 'pre
           (apply elem 'code
                  #:attrs (list (cons 'class (string-append "language-" lname)))
                  (render-tokens (lexer text))))]
    [else
     (apply elem 'code (render-tokens (plain-lexer text)))]))

;; Body-reader registration: `code` uses a raw reader for both forms so
;; `#`, braces and operators in the source are never parsed as YAMD.
(module* yamd-reader #f
  (require "../reader/registry.rkt")
  (provide body-readers)
  (define body-readers
    (hasheq 'code (body-reader 'raw #f))))

;; --------------------------------------------------------------------------
(module+ test
  (require rackunit
           "../backend/html.rkt")

  ;; inline plain: escaped, bare <code>, braces/# preserved
  (check-equal? (content->html (code "x = {1} # ok"))
                "<code>x = {1} # ok</code>")

  ;; unknown lang lists known langs
  (check-exn #rx"unknown lang \"cpp\"; known langs: gdscript, plain, python, racket"
             (lambda () (code "x" #:lang "cpp")))

  ;; non-string body
  (check-exn #rx"expected a string body"
             (lambda () (code 42 #:lang "plain")))

  ;; python: keyword + comment + string + number highlighted; # is not a call
  (let ([h (content->html (code "def f(): # c\n  return 1" #:lang "python"))])
    (check-true (regexp-match? #rx"<pre><code class=\"language-python\">" h))
    (check-true (regexp-match? #rx"<span class=\"tok-keyword\">def</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-comment\"># c</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-keyword\">return</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-number\">1</span>" h)))

  ;; racket: keyword classification and delimiter/text handling
  (let ([h (content->html (code "(define x 42) ; hi" #:lang "racket"))])
    (check-true (regexp-match? #rx"<span class=\"tok-keyword\">define</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-number\">42</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-comment\">; hi</span>" h)))

  ;; gdscript: keywords, annotation, nodepath, prefixed strings, numbers, const
  (let ([h (content->html
            (code (string-append
                   "extends Node\n"
                   "@export var speed := 200.0\n"
                   "@onready var sprite = $Sprite2D\n"
                   "var who := %Player\n"
                   "var anim := &\"idle\"\n"
                   "var hex = 0xFF\n"
                   "var bin = 0b1010\n"
                   "var big = 1_000\n"
                   "if speed and not null: # go\n"
                   "\tpass")
                  #:lang "gdscript"))])
    (check-true (regexp-match? #rx"<pre><code class=\"language-gdscript\">" h))
    (check-true (regexp-match? #rx"<span class=\"tok-keyword\">extends</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-annotation\">@export</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-annotation\">@onready</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-nodepath\">.Sprite2D</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-nodepath\">%Player</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-string\">&amp;\"idle\"</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-number\">0xFF</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-number\">0b1010</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-number\">1_000</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-keyword\">pass</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-constant\">null</span>" h))
    (check-true (regexp-match? #rx"<span class=\"tok-comment\"># go</span>" h)))

  ;; gdscript nodepath vs modulo approximation is non-fatal; totality holds
  (check-true (string? (content->html (code "a % b\n@#$%^\n" #:lang "gdscript"))))

  ;; plain block: no spans, escaped
  (check-equal? (content->html (code "a < b & c" #:lang "plain"))
                "<pre><code class=\"language-plain\">a &lt; b &amp; c</code></pre>")

  ;; totality: arbitrary junk tokenizes without error
  (check-true (string? (content->html (code "@#$%^&*()\n\t~`" #:lang "python")))))
