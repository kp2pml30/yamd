#lang typed/racket/base
;; YAMD element IR — the single data model every reader/function/backend agrees on.
;; Typed on purpose: backends and passes pattern-match on this exhaustively.

;; The `info` two-pass table is defined UNTYPED (info-table.rkt) so the Content
;; it stores never crosses TR's `Any` boundary; here it is only an OPAQUE type,
;; enough to name `delayed`'s proc domain. See info-table.rkt's header.
(require/typed "info-table.rkt"
  [#:opaque info info?])

(provide (struct-out element)
         ;; The struct is `raw-node`; its predicate/accessors are re-exported
         ;; under the classic `raw?`/`raw-backend`/`raw-text`/`raw-block?`
         ;; names, and `raw` is the block?-optional smart constructor. The
         ;; struct type stays internal (`Content` is the public type).
         (struct-out delayed)
         info                             ; the opaque type (struct lives in info-table.rkt)
         (rename-out [raw-node? raw?]
                     [raw-node-backend raw-backend]
                     [raw-node-text raw-text]
                     [raw-node-block? raw-block?])
         raw                              ; smart constructor (block? optional)
         Content Attrs
         elem
         content?
         content->sexp)

(define-type Attrs (Listof (Pairof Symbol String)))

;; Content is a splice-friendly tree: lists are transparently flattened by
;; consumers (decode, backends). `raw` is a backend-specific escape hatch;
;; a backend MUST error on raw nodes addressed to a different backend.
;; `delayed` nodes are resolved against the collected `info` in pass 2.
(define-type Content (U String element raw-node delayed (Listof Content)))

(struct element
  ([tag : Symbol]
   [attrs : Attrs]
   [children : (Listof Content)]
   [loc : (U srcloc False)])
  #:transparent)

;; `block?` marks raw content that is block-level (e.g. a `<div>`): decode
;; treats it like a block element — it terminates the surrounding paragraph
;; and is NOT wrapped in <p>. Inline raw (block? = #f) still joins the flow.
;; The struct is named `raw-node` so the exported smart constructor can take
;; the friendly name `raw`; predicate/accessors are re-exported (rename-out)
;; under their classic `raw?`/`raw-backend`/`raw-text`/`raw-block?` names.
(struct raw-node
  ([backend : Symbol]
   [text : String]
   [block? : Boolean])
  #:transparent)

;; `raw` wrapper makes block? optional (default #f) so 2-arg call sites stay
;; valid; new block-level call sites pass #t.
(: raw (->* (Symbol String) (Boolean) raw-node))
(define (raw backend text [block? #f])
  (raw-node backend text block?))

(struct delayed
  ([proc : (-> info Content)])
  #:transparent)

(: elem (->* (Symbol) (#:attrs Attrs #:loc (U srcloc False)) #:rest Content element))
(define (elem tag #:attrs [attrs '()] #:loc [loc #f] . children)
  (element tag attrs children loc))

;; Note: TR cannot prove the recursive proposition, so this is a plain
;; Boolean predicate; use `(cast v Content)` after checking if needed.
(: content? (-> Any Boolean))
(define (content? v)
  (or (string? v)
      (element? v)
      (raw-node? v)
      (delayed? v)
      (and (list? v) (andmap content? v))))

;; Stable s-expression view of the IR, used by .sexp golden tests.
(: content->sexp (-> Content Any))
(define (content->sexp c)
  (cond
    [(string? c) c]
    [(element? c)
     (list 'element (element-tag c) (element-attrs c)
           (map content->sexp (element-children c)))]
    ;; Inline raw keeps its historical 3-element shape; a block raw appends a
    ;; trailing 'block marker so existing inline goldens do not churn.
    [(raw-node? c) (if (raw-node-block? c)
                       (list 'raw (raw-node-backend c) (raw-node-text c) 'block)
                       (list 'raw (raw-node-backend c) (raw-node-text c)))]
    [(delayed? c) '(delayed)]
    [else (cons 'splice (map content->sexp c))]))
