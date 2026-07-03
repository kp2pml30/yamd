#lang typed/racket/base
;; Two-pass machinery, pass 2. Pass 1 (collect) is document evaluation itself —
;; functions call `collect!`/`info-add!` (see info-table.rkt) while producing IR
;; that may contain `delayed` nodes; pass 2 (`resolve-content`, here) walks the
;; IR and replaces each delayed node with the result of its proc applied to the
;; frozen info table.
;;
;; The info table itself lives UNTYPED in info-table.rkt: its stored Content
;; must not cross this typed module's `Any` boundary, which would chaperone-wrap
;; the `delayed` procs inside it into un-applicable values. This module only
;; RESOLVES IR — it never reads the table — so it stays typed with `info` as an
;; opaque type and does not touch (or re-export) the untyped accessors. Untyped
;; callers require info-table.rkt directly.

(require "ir.rkt")

(provide resolve-content)

;; Depth-limited so a delayed proc that keeps returning delayed nodes is an
;; error instead of an infinite loop.
(: resolve-content (->* (Content info) (#:fuel Natural) Content))
(define (resolve-content c i #:fuel [fuel 32])
  (cond
    [(string? c) c]
    [(raw? c) c]
    [(delayed? c)
     (when (zero? fuel)
       (error 'resolve-content "delayed node did not converge (fuel exhausted)"))
     (resolve-content ((delayed-proc c) i) i #:fuel (sub1 fuel))]
    [(element? c)
     (element (element-tag c)
              (element-attrs c)
              (for/list : (Listof Content) ([ch (in-list (element-children c))])
                (resolve-content ch i #:fuel fuel))
              (element-loc c))]
    [else
     (for/list : (Listof Content) ([ch (in-list c)])
       (resolve-content ch i #:fuel fuel))]))
