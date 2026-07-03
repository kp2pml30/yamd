#lang racket/base
;; The two-pass `info` table — pass-1 collected state. UNTYPED on purpose.
;;
;; This table stores arbitrary host data AND Content (notably `section`'s
;; 'sections entries, whose title may contain a `delayed` node from a #ref in
;; the heading). Content stored here therefore round-trips: pass 1 writes it,
;; and consumers (#toc, #glossary, #notes) read it back and embed it into their
;; own delayed output, which pass 2 (`resolve-content`, in the typed passes.rkt)
;; then resolves.
;;
;; The table MUST NOT live behind Typed Racket's `Any` boundary. A `delayed`
;; proc stored as `Any` gets chaperone-wrapped into an un-applicable value, so
;; resolving it later crashes with the opaque "Attempted to use a higher-order
;; value passed as `Any`" contract error (a #ref in a #section title + #toc hit
;; exactly this). Keeping the struct and every accessor untyped means stored
;; Content never crosses a typed boundary; typed code (ir.rkt, passes.rkt)
;; treats `info` as an OPAQUE type (require/typed #:opaque). Same rationale as
;; docset.rkt's side registry — see the "Registry" note there.

;; NOTE: only the predicate `info?` is exported, not the struct constructor or
;; the `info-table` accessor — callers create tables with `make-info` and never
;; touch the field. This also keeps the untyped value name space clear of a
;; binding named `info`, which the typed side (ir.rkt) exports as the OPAQUE
;; TYPE `info`; re-exporting both through runtime/main would otherwise clash.
(provide info?
         current-info
         make-info
         info-set! info-add! info-ref
         collect!)

;; Collected cross-document state (pass 1 writes, pass 2 reads). The table is a
;; plain mutable hash — never a chaperoned one — so the Content values inside
;; (delayed nodes included) come back exactly as stored.
(struct info (table) #:transparent)

(define current-info (make-parameter #f))

(define (make-info)
  (info (make-hash)))

(define (info-set! i key val)
  (hash-set! (info-table i) key val))

;; Multi-valued keys (backlinks, glossary entries, TOC): conses onto a list.
;; Mixing info-set! and info-add! on one key is a bug — error, don't guess.
(define (info-add! i key val)
  (hash-update! (info-table i) key
                (lambda (old)
                  (unless (list? old)
                    (error 'info-add! "key ~e holds non-list value ~e (info-set!/info-add! mix?)"
                           key old))
                  (cons val old))
                (lambda () '())))

(define (info-ref i key default)
  (hash-ref (info-table i) key (lambda () default)))

(define (need-info)
  (define i (current-info))
  (unless i
    (error 'collect! "no current info table; not inside a YAMD collect pass"))
  i)

(define (collect! key val)
  (info-set! (need-info) key val))
