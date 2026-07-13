#lang racket/base
;; Control-flow calls — a plugin: `#use(std/control)`. Both take a LAZY body
;; (forced by the callee, like `#section` / `#fork-context`), so `#showif`
;; never evaluates the branch it drops, and `#foreach` evaluates its body once
;; per element under a freshly forked context.
;;
;;   #foreach(seq 'x)         iterate `seq` (any sequence — a list, #(in-range …),
;;       #item{#(context-ref 'x)}   a stream, a generator …); each pass forks the
;;                            context, binds `x` to the element (read it with
;;                            #(context-ref 'x)), forces the body, and the
;;                            per-pass contents are spliced in document order.
;;   #showif(test)            force and emit the body when `test` is truthy,
;;       #para{…}             otherwise nothing — an `if` with no `else`.
;;
;; Named foreach/showif, NOT for/when/if: a plugin's bindings share the document
;; namespace with host code, so providing `for`/`when`/`if` would collide with
;; Racket's and break `#(for …)` / `#(when …)` / `#(if …)` in host escapes.

(require "../runtime.rkt"          ; context-set!, with-forked-context (via context.rkt)
         (only-in racket/list append-map))

(provide foreach showif)

;; #foreach(seq 'var):: body — `var` is a quoted symbol (a context key).
;; append-map splices each pass's body content (a list) into one flat sequence,
;; matching the shape `#(for/list …)` produces, so `#foreach` drops straight
;; into a `#list` / `#table` / flow position.
(define (foreach seq var body)
  (unless (symbol? var)
    (error 'foreach
           "the loop variable must be a quoted symbol, e.g. #foreach(items 'x); got ~e"
           var))
  (append-map
   (lambda (v)
     (with-forked-context
       (context-set! var v)
       (body)))
   (sequence->list seq)))

;; Accept any sequence, not just lists, without pulling in racket/sequence's
;; for-clause optimizations: force it to a list once so append-map can consume
;; it. (`for/list ([v seq])` would also work; this keeps the append-map shape.)
(define (sequence->list seq)
  (if (list? seq)
      seq
      (for/list ([v seq]) v)))

;; #showif(test):: body — emit the (forced) body when `test` is truthy, else
;; nothing. Lazy, so a dropped body is never evaluated (no stray defines, no
;; #section/#def side effects from a branch that is not taken).
(define (showif test body)
  (if test (body) '()))

(module* yamd-reader #f
  (require "../reader/registry.rkt")
  (provide body-readers)
  (define body-readers
    (hasheq 'foreach (body-reader 'markup #t)
            'showif  (body-reader 'markup #t))))

(module+ test
  (require rackunit
           "../ir.rkt"
           "../context.rkt")

  ;; foreach forces the body once per element, under a forked context that
  ;; binds the loop var; contents splice in order.
  (check-equal?
   (foreach (list 10 20 30) 'x
            (lambda () (list (elem 'li (list (number->string (context-ref 'x)))))))
   (list (elem 'li (list "10")) (elem 'li (list "20")) (elem 'li (list "30"))))

  ;; any sequence works, not just lists
  (check-equal?
   (foreach (in-range 3) 'i (lambda () (list (context-ref 'i))))
   (list 0 1 2))

  ;; the forked context does not leak: the binding is gone after the loop
  (void (foreach (list 1) 'leaked (lambda () '())))
  (check-false (context-ref 'leaked))

  ;; a non-symbol loop variable is a clear error
  (check-exn #rx"must be a quoted symbol"
             (lambda () (foreach (list 1) "x" (lambda () '()))))

  ;; showif: truthy forces + emits the body; falsy drops it WITHOUT forcing
  (check-equal? (showif #t (lambda () (list "shown"))) (list "shown"))
  (check-equal? (showif #f (lambda () (error "must not be forced"))) '()))
