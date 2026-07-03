#lang racket/base
;; std/math/asciimath — AsciiMath notation as a custom body reader.
;;
;;   #use(std/math: asciimath)
;;   Inline: #m{sum_(i=0)^n i^2}
;;   Block:  #math
;;             x = (-b +- sqrt(b^2 - 4ac)) / (2a)
;;
;; #m (inline) and #math (block) parse AsciiMath at READ time into a MathML
;; element IR (math / mrow / msub / msup / msubsup / mfrac / msqrt / mroot /
;; mi / mn / mo / mtext). The practical subset covered: numbers, identifiers,
;; operators, `_`/`^`, parens/grouping, `a/b` fractions, `sqrt`/`root`,
;; `frac`, and a table of common greek letters, big operators, relations and
;; function names. Anything not in the table degrades gracefully to single
;; `mi` (letters) or `mo` (other characters) tokens rather than erroring; the
;; only hard error is a genuinely unparseable body (an unterminated `"..."`
;; string literal), reported with the reader-supplied srcloc plus the byte
;; offset inside the body.
;;
;; Deliberate simplifications (see tests/golden/math): sum/prod/lim limits use
;; msub/msup/msubsup (side-set), not munderover; matrices, font commands
;; (bb/cc/…) and accents (hat/vec/…) are not supported.

(require yamd/runtime)

(provide m math)

;; ---------------------------------------------------------------------------
;; Runtime: MathML tree -> element IR.
;;
;; The read-time parser emits a quoted MathML tree: a node is either a string
;; (leaf text) or (list tag attrs child ...) where attrs is an assoc list and
;; each child is again a node. `m`/`math` are the functions the reader binds
;; `#m`/`#math` to; they receive that tree and build the IR.

(define (mml->content node)
  (cond
    [(string? node) node]
    [(and (pair? node) (symbol? (car node)) (list? (cadr node)))
     (element (car node) (cadr node) (map mml->content (cddr node)) #f)]
    [else (error 'asciimath "malformed MathML node: ~e" node)]))

(define (m tree) (mml->content tree))
(define (math tree) (mml->content tree))

;; ---------------------------------------------------------------------------
;; Symbol table. Each entry: (list match kind payload).
;;   kind 'mo / 'mi  -> a constant, payload is the rendered text
;;   kind 'left / 'right -> a bracket, payload is rendered text or #f (invisible)
;;   kind 'sqrt / 'frac / 'root -> unary/binary operators
;;   kind 'sub / 'sup / 'div -> infix `_` / `^` / `/`
;; Longest match wins (the tokenizer sorts by descending match length).

(define raw-symbols
  '(;; infix
    ("_" sub #f) ("^" sup #f) ("/" div #f)
    ;; brackets
    ("(" left "(") (")" right ")")
    ("[" left "[") ("]" right "]")
    ("{" left "{") ("}" right "}")
    ("{:" left #f) (":}" right #f)
    ("(:" left "〈") (":)" right "〉")   ; ⟨ ⟩
    ;; unary / binary operators
    ("sqrt" sqrt #f) ("root" root #f) ("frac" frac #f)
    ;; big operators
    ("sum" mo "∑") ("prod" mo "∏")
    ("int" mo "∫") ("oint" mo "∮")
    ("del" mo "∂") ("partial" mo "∂")
    ("grad" mo "∇") ("nabla" mo "∇")
    ;; binary/relational operators
    ("+-" mo "±") ("-+" mo "∓")
    ("xx" mo "×") ("-:" mo "÷")
    ("**" mo "∗") ("*" mo "⋅") ("cdot" mo "⋅")
    ("//" mo "/") ("@" mo "∘")
    ("o+" mo "⊕") ("ox" mo "⊗")
    ("-" mo "−") ("+" mo "+")
    ("!=" mo "≠") ("<=>" mo "⇔")
    ("<=" mo "≤") (">=" mo "≥")
    ("=" mo "=") ("<" mo "<") (">" mo ">")
    ("~=" mo "≅") ("~~" mo "≈") ("-=" mo "≡")
    ("prop" mo "∝")
    ;; set / logic
    ("in" mo "∈") ("notin" mo "∉")
    ("sube" mo "⊆") ("supe" mo "⊇")
    ("sub" mo "⊂") ("sup" mo "⊃")
    ("cup" mo "∪") ("cap" mo "∩")
    ("forall" mo "∀") ("exists" mo "∃")
    ("AA" mo "∀") ("EE" mo "∃") ("not" mo "¬")
    ;; arrows
    ("|->" mo "↦") ("->" mo "→") ("<-" mo "←")
    ("=>" mo "⇒")
    ;; misc
    ("oo" mo "∞") ("..." mo "…") ("ldots" mo "…")
    ("cdots" mo "⋯") ("!" mo "!")
    ;; greek (lowercase)
    ("alpha" mi "α") ("beta" mi "β") ("gamma" mi "γ")
    ("delta" mi "δ") ("epsilon" mi "ε") ("varepsilon" mi "ε")
    ("zeta" mi "ζ") ("eta" mi "η") ("theta" mi "θ")
    ("vartheta" mi "ϑ") ("iota" mi "ι") ("kappa" mi "κ")
    ("lambda" mi "λ") ("mu" mi "μ") ("nu" mi "ν")
    ("xi" mi "ξ") ("pi" mi "π") ("rho" mi "ρ")
    ("sigma" mi "σ") ("tau" mi "τ") ("upsilon" mi "υ")
    ("phi" mi "φ") ("varphi" mi "ϕ") ("chi" mi "χ")
    ("psi" mi "ψ") ("omega" mi "ω")
    ;; greek (uppercase)
    ("Gamma" mi "Γ") ("Delta" mi "Δ") ("Theta" mi "Θ")
    ("Lambda" mi "Λ") ("Xi" mi "Ξ") ("Pi" mi "Π")
    ("Sigma" mi "Σ") ("Phi" mi "Φ") ("Psi" mi "Ψ")
    ("Omega" mi "Ω")
    ;; function names (kept together as a single mi)
    ("sin" mi "sin") ("cos" mi "cos") ("tan" mi "tan")
    ("sec" mi "sec") ("csc" mi "csc") ("cot" mi "cot")
    ("sinh" mi "sinh") ("cosh" mi "cosh") ("tanh" mi "tanh") ("coth" mi "coth")
    ("log" mi "log") ("ln" mi "ln") ("exp" mi "exp")
    ("det" mi "det") ("dim" mi "dim") ("lim" mi "lim")
    ("gcd" mi "gcd") ("mod" mi "mod") ("min" mi "min") ("max" mi "max")))

(define sorted-symbols
  (sort raw-symbols > #:key (lambda (e) (string-length (car e)))))

;; ---------------------------------------------------------------------------
;; Tokenizer. Tokens are vectors #(kind data offset):
;;   #(const (tag . text) off)  -- tag in {mi mn mo}
;;   #(mtext text off)
;;   #(left rendered-or-#f off) / #(right rendered-or-#f off)
;;   #(sqrt #f off) / #(frac #f off) / #(root #f off)
;;   #(sub #f off) / #(sup #f off) / #(div #f off)
;;   #(eof #f off)

(define (ascii-digit? c) (and (char>=? c #\0) (char<=? c #\9)))

(define (match-symbol text i n)
  (for/or ([e (in-list sorted-symbols)])
    (define s (car e))
    (define len (string-length s))
    (and (<= (+ i len) n)
         (string=? s (substring text i (+ i len)))
         e)))

(define (tokenize text loc)
  (define n (string-length text))
  (let loop ([i 0] [acc '()])
    (cond
      [(>= i n) (reverse (cons (vector 'eof #f i) acc))]
      [else
       (define c (string-ref text i))
       (cond
         [(char-whitespace? c) (loop (add1 i) acc)]
         [(char=? c #\")
          (let scan ([j (add1 i)])
            (cond
              [(>= j n) (asciimath-error loc i "unterminated string literal")]
              [(char=? (string-ref text j) #\")
               (loop (add1 j)
                     (cons (vector 'mtext (substring text (add1 i) j) i) acc))]
              [else (scan (add1 j))]))]
         [(ascii-digit? c)
          (let scan ([j (add1 i)] [seen-dot #f])
            (cond
              [(and (< j n) (ascii-digit? (string-ref text j))) (scan (add1 j) seen-dot)]
              [(and (< j n) (not seen-dot) (char=? (string-ref text j) #\.)
                    (< (add1 j) n) (ascii-digit? (string-ref text (add1 j))))
               (scan (add1 j) #t)]
              [else
               (loop j (cons (vector 'const (cons 'mn (substring text i j)) i) acc))]))]
         [(match-symbol text i n)
          => (lambda (e)
               (define kind (cadr e))
               (define payload (caddr e))
               (define len (string-length (car e)))
               (define tok
                 (case kind
                   [(mo mi) (vector 'const (cons kind payload) i)]
                   [(left)  (vector 'left payload i)]
                   [(right) (vector 'right payload i)]
                   [(sqrt frac root sub sup div) (vector kind #f i)]))
               (loop (+ i len) (cons tok acc)))]
         [(char-alphabetic? c)
          (loop (add1 i) (cons (vector 'const (cons 'mi (string c)) i) acc))]
         [else
          (loop (add1 i) (cons (vector 'const (cons 'mo (string c)) i) acc))])])))

;; ---------------------------------------------------------------------------
;; Recursive-descent parser. Grammar (following AsciiMath precedence):
;;   simple       := const | mtext | l Expr r | sqrt simple
;;                 | frac simple simple | root simple simple
;;   intermediate := simple (`_` simple (`^` simple)? | `^` simple)?
;;   Expr         := (intermediate (`/` intermediate)?)*
;; Sub/sup/frac/sqrt/root arguments have their outer brackets stripped
;; ("removeBrackets" in AsciiMath); bases keep theirs.

(define (asciimath-error loc offset msg)
  (error 'asciimath "~a:~a:~a: ~a (body offset ~a)"
         (srcloc-source loc) (srcloc-line loc) (srcloc-column loc) msg offset))

;; Collapse a list of nodes into one node (a bare node, or an mrow).
(define (wrap-mrow nodes)
  (cond
    [(null? nodes) (list 'mrow '())]
    [(null? (cdr nodes)) (car nodes)]
    [else (list* 'mrow '() nodes)]))

;; text -> MathML tree; `display` is "inline" or "block".
(define (parse-asciimath text loc display)
  (define state (box (tokenize text loc)))
  (define (peek) (car (unbox state)))
  (define (advance!) (set-box! state (cdr (unbox state))))
  (define (kind t) (vector-ref t 0))
  (define (data t) (vector-ref t 1))

  ;; -> (values node stripped-node)
  (define (parse-simple)
    (define t (peek))
    (case (kind t)
      [(const)
       (advance!)
       (define node (list (car (data t)) '() (cdr (data t))))
       (values node node)]
      [(mtext)
       (advance!)
       (define node (list 'mtext '() (data t)))
       (values node node)]
      [(left)
       (advance!)
       (define inner (parse-expr #t))
       (define rt (peek))
       (define rstr
         (cond [(eq? (kind rt) 'right) (advance!) (data rt)]
               [else #f]))          ; unbalanced: no closing bracket, degrade
       (define lstr (data t))
       (define stripped (wrap-mrow inner))
       (define visible
         (wrap-mrow (append (if lstr (list (list 'mo '() lstr)) '())
                            inner
                            (if rstr (list (list 'mo '() rstr)) '()))))
       (values visible stripped)]
      [(sqrt)
       (advance!)
       (define-values (arg arg-s) (parse-simple))
       (define node (list 'msqrt '() arg-s))
       (values node node)]
      [(frac)
       (advance!)
       (define-values (a a-s) (parse-simple))
       (define-values (b b-s) (parse-simple))
       (define node (list 'mfrac '() a-s b-s))
       (values node node)]
      [(root)
       (advance!)
       (define-values (idx idx-s) (parse-simple))
       (define-values (base base-s) (parse-simple))
       (define node (list 'mroot '() base-s idx-s))
       (values node node)]
      [(sub sup div)
       ;; infix with no left operand: degrade to a literal operator glyph
       (advance!)
       (define ch (case (kind t) [(sub) "_"] [(sup) "^"] [(div) "/"]))
       (define node (list 'mo '() ch))
       (values node node)]
      [(right)
       ;; A right bracket only reaches here when it is unmatched (the matched
       ;; one is consumed by the enclosing `left` branch, and parse-expr stops
       ;; before it inside brackets). Consume it and degrade to a literal glyph
       ;; so the top-level loop always makes progress.
       (advance!)
       (define r (data t))
       (define node (if r (list 'mo '() r) (list 'mrow '())))
       (values node node)]
      [(eof)
       ;; Terminal: yield an empty group WITHOUT advancing. Safe because the
       ;; only re-entry point (parse-expr) stops on eof, so no loop.
       (define node (list 'mrow '()))
       (values node node)]))

  ;; -> (values node stripped-node)
  (define (parse-intermediate)
    (define-values (base base-s) (parse-simple))
    (define t (peek))
    (case (kind t)
      [(sub)
       (advance!)
       (define-values (sub sub-s) (parse-simple))
       (cond
         [(eq? (kind (peek)) 'sup)
          (advance!)
          (define-values (sup sup-s) (parse-simple))
          (define node (list 'msubsup '() base sub-s sup-s))
          (values node node)]
         [else
          (define node (list 'msub '() base sub-s))
          (values node node)])]
      [(sup)
       (advance!)
       (define-values (sup sup-s) (parse-simple))
       (define node (list 'msup '() base sup-s))
       (values node node)]
      [else (values base base-s)]))

  ;; in-bracket? -> stop at a `right` token instead of only eof.
  ;; Returns a list of nodes.
  (define (parse-expr in-bracket?)
    (let loop ([acc '()])
      (define t (peek))
      (cond
        [(eq? (kind t) 'eof) (reverse acc)]
        [(and in-bracket? (eq? (kind t) 'right)) (reverse acc)]
        [else
         (define-values (inode istr) (parse-intermediate))
         (cond
           [(eq? (kind (peek)) 'div)
            (advance!)
            (define-values (rnode rstr) (parse-intermediate))
            (loop (cons (list 'mfrac '() istr rstr) acc))]
           [else (loop (cons inode acc))])])))

  (list 'math (list (cons 'display display)) (wrap-mrow (parse-expr #f))))

;; ---------------------------------------------------------------------------
;; Read-time body reader: turn AsciiMath body text into the syntax of the
;; single body argument passed to the `m`/`math` call, i.e. a quoted tree.

(define ((asciimath-body display) text loc)
  (datum->syntax #f (list 'quote (parse-asciimath text loc display)) loc))

(module* yamd-reader #f
  (require yamd/reader/registry)
  (provide body-readers)
  (define body-readers
    (hasheq 'm    (body-reader (asciimath-body "inline") #f)
            'math (body-reader (asciimath-body "block") #f))))

(module+ test
  (require rackunit)
  (define loc (srcloc "t" 1 0 1 0))
  (define (parse s) (parse-asciimath s loc "inline"))

  ;; numbers, identifiers, operators
  (check-equal? (parse "1 + x")
                '(math ((display . "inline"))
                       (mrow () (mn () "1") (mo () "+") (mi () "x"))))

  ;; fraction uses stripped brackets on numerator
  (check-equal? (parse "(a+b)/2")
                '(math ((display . "inline"))
                       (mfrac ()
                              (mrow () (mi () "a") (mo () "+") (mi () "b"))
                              (mn () "2"))))

  ;; sub + super collapses to msubsup; greek + big operator lookups
  (check-equal? (parse "sum_(i=0)^n")
                '(math ((display . "inline"))
                       (msubsup ()
                                (mo () "∑")
                                (mrow () (mi () "i") (mo () "=") (mn () "0"))
                                (mi () "n"))))

  (check-equal? (parse "sqrt(x^2+1)")
                '(math ((display . "inline"))
                       (msqrt ()
                              (mrow () (msup () (mi () "x") (mn () "2"))
                                    (mo () "+") (mn () "1")))))

  (check-equal? (parse "alpha")
                '(math ((display . "inline")) (mi () "α")))

  ;; unknown token degrades to mo, never errors
  (check-equal? (parse "?")
                '(math ((display . "inline")) (mo () "?")))

  ;; stray/unmatched brackets must terminate (regression: infinite loop)
  (check-equal? (parse ")")
                '(math ((display . "inline")) (mo () ")")))
  (check-equal? (parse "a)b")
                '(math ((display . "inline"))
                       (mrow () (mi () "a") (mo () ")") (mi () "b"))))
  ;; unbalanced open bracket degrades to a bare left glyph, no closing mo
  (check-equal? (parse "(a+b")
                '(math ((display . "inline"))
                       (mrow () (mo () "(") (mi () "a") (mo () "+") (mi () "b"))))
  ;; operators/units with nothing to bind must not hang either
  (check-equal? (parse "sqrt")
                '(math ((display . "inline")) (msqrt () (mrow ()))))

  ;; unterminated string literal is the one hard error, with a located message
  (check-exn #rx"t:1:0: unterminated string literal \\(body offset 0\\)"
             (lambda () (parse "\"oops"))))
