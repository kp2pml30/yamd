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
         call-with-fresh-context
         ;; Frame-level API (opaque frame handle). Unlike context-ref/set!,
         ;; these read/write ONE named frame with no chain search, and let a
         ;; caller capture the current frame and walk its parents. A plugin
         ;; that needs its own scope chain (std/xref) builds it on these; core
         ;; stays free of plugin concepts.
         current-frame
         frame-parent
         frame-ref
         frame-set!)

(struct ctx-frame (table parent) #:transparent)

;; The current frame as an opaque handle, for capture (e.g. a delayed proc
;; that must walk the scope chain in pass 2 — contexts are gone by then, but a
;; captured frame and its parents stay reachable through the closure).
(define (current-frame) (current-context))
(define (frame-parent fr) (ctx-frame-parent fr))
;; Read/write a SINGLE frame (no chain search — that is context-ref's job).
(define (frame-ref fr key [default #f]) (hash-ref (ctx-frame-table fr) key default))
(define (frame-set! fr key val) (hash-set! (ctx-frame-table fr) key val))

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
