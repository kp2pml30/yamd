#lang racket/base
;; Auto-scoped context stack. A context is a chain of mutable frames held in
;; a parameter: `context-ref` searches outward, `context-set!` writes the
;; TOPMOST frame (shadowing, never clobbering, outer frames). Forking pushes
;; a fresh frame for a dynamic extent; functions that want their body to see
;; a forked context take a lazy body and force it inside
;; `with-forked-context` (see docs/architecture.md "Contexts").

(provide current-context
         context-ref
         context-set!
         context-update!
         call-with-forked-context
         with-forked-context
         call-with-fresh-context)

(struct ctx-frame (table parent) #:transparent)

(define current-context
  (make-parameter (ctx-frame (make-hasheq) #f)))

(define nothing (gensym 'nothing))

(define (context-ref key [default #f])
  (let loop ([frame (current-context)])
    (cond
      [(not frame) default]
      [else
       (define v (hash-ref (ctx-frame-table frame) key nothing))
       (if (eq? v nothing)
           (loop (ctx-frame-parent frame))
           v)])))

(define (context-set! key val)
  (hash-set! (ctx-frame-table (current-context)) key val))

;; Read (through the chain), transform, write to the topmost frame.
(define (context-update! key f [default #f])
  (define v (f (context-ref key default)))
  (context-set! key v)
  v)

(define (call-with-forked-context thunk)
  (parameterize ([current-context (ctx-frame (make-hasheq) (current-context))])
    (thunk)))

(define-syntax-rule (with-forked-context body ...)
  (call-with-forked-context (lambda () body ...)))

;; A brand-new root context sharing nothing with the caller. Each document
;; evaluation runs under one so context state never leaks across documents.
(define (call-with-fresh-context thunk)
  (parameterize ([current-context (ctx-frame (make-hasheq) #f)])
    (thunk)))
