#lang racket/base
;; Runtime support required by reader-generated code. The evaluation
;; namespace (see eval.rkt) always has this module attached.

(require (for-syntax racket/base)
         "ir.rkt"
         "context.rkt"
         "info-table.rkt"
         "passes.rkt"
         (only-in "decode.rkt" para-break))

(provide yamd-body
         yamd-use
         ->content
         para-break
         current-yamd-source
         current-docref-root
         (all-from-out "ir.rkt")
         (all-from-out "context.rkt")
         ;; The info-table accessors are UNTYPED here (info-table.rkt), never
         ;; the typed passes.rkt re-exports: plugins storing Content (section
         ;; titles with a #ref) into the table must not cross a typed `Any`
         ;; boundary (see info-table.rkt).
         (all-from-out "info-table.rkt")
         (all-from-out "passes.rkt"))

;; The source file of the document currently being evaluated (a complete,
;; simplified path), or #f for string-fed documents. Set by eval.rkt entry
;; points and rebound by `yamd-eval-embedded` for each embedded child, so
;; plugins (#include, #toctree) resolve relative paths against the file that
;; names them. Lives here (not in eval.rkt) so plugin instantiations in
;; document namespaces — where yamd/runtime is always attached — share it.
(define current-yamd-source (make-parameter #f))

;; Base directory for #doc-ref document ids: a #doc-ref path is written relative
;; to this dir (the CLI's --root, e.g. a monorepo root). #f = the root
;; document's own directory. Pinning it keeps ids stable no matter which
;; sub-document of the tree you build. Also the site root that root-relative
;; `/x.png` image srcs resolve under (yamd/image; #f = left unresolved).
;; Lives here, like current-yamd-source, so plugins read the builder's value.
(define current-docref-root (make-parameter #f))

;; Coerce a host value to Content. Void disappears (statement results);
;; numbers/symbols display; anything else is an error — no silent ~a dumping.
(define (->content v)
  (cond
    [(void? v) '()]
    [(string? v) v]
    [(element? v) v]
    [(raw? v) v]
    [(delayed? v) v]
    [(list? v) (map ->content v)]
    [(number? v) (number->string v)]
    [(symbol? v) (symbol->string v)]
    [(char? v) (string v)]
    [else (error '->content "cannot convert host value to content: ~e" v)]))

;; Imports plugin modules into the document namespace at collect time. The
;; reader emits `(yamd-use 'mod ...)` for each `#use(...)`; the corresponding
;; read-time body-reader merge happens in the reader itself.
;;
;; Plugins are instantiated ONCE — in this module's home registry (the main
;; instantiation: yamd/runtime is attached into every document namespace) —
;; and attached into the requiring namespace, never re-instantiated per
;; namespace. Without this, each document of a multi-source set would get its
;; own instantiation of a #use'd plugin, hence distinct struct types: a value
;; collected into the shared info by one file (e.g. std/xref's xdef) would
;; fail another file's predicate. Consequence, by design: plugin module-level
;; state is process-global — per-document state belongs in info/context
;; (which plugins already must use, since documents share one process).
(define-namespace-anchor runtime-anchor)

(define (yamd-use . mods)
  (define home (namespace-anchor->namespace runtime-anchor))
  (for ([m (in-list mods)])
    (parameterize ([current-namespace home])
      (dynamic-require m #f))
    (with-handlers
        ([exn:fail?
          (lambda (e)
            (raise
             (make-exn:fail
              (format
               "yamd-use: module ~a is already separately instantiated in this document — was it loaded with a top-level #(require ...) before #use? Always #use plugins, never #(require ...) them: a per-namespace instantiation breaks struct identity across a document set\n  underlying error: ~a"
               m (exn-message e))
              (exn-continuation-marks e))))])
      (namespace-attach-module home m))
    (namespace-require m)))

;; A parsed markup body: a sequence of forms -> one Content list. Forms may
;; freely interleave content expressions with block-local definitions
;; (`#(define ...)` inside a `{...}` or indented body); per DESIGN, defines
;; are local to the block. Implemented as a splicing internal-definition
;; context: each form is partial-expanded; definitions are hoisted into scope
;; and expression values are collected, in order, into the result list.
(define-syntax (yamd-body stx)
  (syntax-case stx ()
    [(_ form ...)
     (let ([ctx (syntax-local-make-definition-context)]
           [ctxv (list (gensym 'yamd-body))]
           [stops (list #'begin #'define-values #'define-syntaxes
                        #'#%require #'#%provide #'#%declare)])
       (define body-out '())    ; reversed: defs + (define-values (tmp) val)
       (define result-tmps '()) ; reversed: temps holding expression values
       (let loop ([forms (syntax->list #'(form ...))])
         (cond
           [(null? forms) (void)]
           [else
            ;; `require`/`provide` raise their own cryptic "not at module
            ;; level" inside local-expand; pre-check the head for a clear,
            ;; located diagnostic. The post-expansion #%require check below
            ;; still catches macros expanding into them.
            (syntax-case (car forms) ()
              [(head . _)
               (and (identifier? #'head)
                    (memq (syntax-e #'head) '(require provide)))
               (raise-syntax-error
                'yamd-body
                "require/provide is not allowed inside a markup body; use #use(...) (a top-level #(require ...) is for host-only libraries, never for yamd plugins)"
                (car forms))]
              [_ (void)])
            (define e (local-expand (car forms) ctxv stops ctx))
            (syntax-case e (begin define-values define-syntaxes)
              [(begin f ...)
               (loop (append (syntax->list #'(f ...)) (cdr forms)))]
              [(define-syntaxes (id ...) rhs)
               (begin
                 (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #'rhs ctx)
                 (set! body-out (cons e body-out))
                 (loop (cdr forms)))]
              [(define-values (id ...) rhs)
               (begin
                 (syntax-local-bind-syntaxes (syntax->list #'(id ...)) #f ctx)
                 (set! body-out (cons e body-out))
                 (loop (cdr forms)))]
              [(head . _)
               (and (identifier? #'head)
                    (memq (syntax-e #'head) '(#%require #%provide #%declare)))
               (raise-syntax-error
                'yamd-body
                "require/provide is not allowed inside a markup body; use #use(...) (a top-level #(require ...) is for host-only libraries, never for yamd plugins)"
                e)]
              [_
               (let ([tmp (car (generate-temporaries '(yamd-body-value)))])
                 (syntax-local-bind-syntaxes (list tmp) #f ctx)
                 (set! body-out (cons #`(define-values (#,tmp) (->content #,e)) body-out))
                 (set! result-tmps (cons tmp result-tmps))
                 (loop (cdr forms)))])]))
       (with-syntax ([(b ...) (reverse body-out)]
                     [(t ...) (reverse result-tmps)])
         (internal-definition-context-introduce
          ctx
          #'(let () b ... (list t ...)))))]))
