#lang racket/base
;; Body-reader registry: consulted by the reader BEFORE parsing a body.
;; Plugins contribute entries via a `yamd-reader` submodule exporting
;; `body-readers` (hash: symbol -> body-reader); `#use` merges them into
;; `current-body-readers` for the rest of the file being read.

(require racket/contract)

(provide (struct-out body-reader)
         (contract-out
          [current-body-readers parameter?]
          [lookup-body-reader (-> symbol? body-reader?)]
          [merge-body-readers! (-> (hash/c symbol? body-reader?) void?)]
          [default-body-reader body-reader?]))

;; parse : 'markup                     — full YAMD parsing (default)
;;       | 'raw                        — verbatim text
;;       | (-> string? srcloc? syntax?) — custom: gets the body text (block:
;;         dedented; inline: brace-balanced scan) and must return the syntax
;;         of the expression passed as that body argument.
;; lazy? : reader wraps each body as (lambda () ...) so the callee controls
;;         evaluation (needed for context forking, e.g. #section).
(struct body-reader (parse lazy?) #:transparent)

(define default-body-reader (body-reader 'markup #f))

;; symbol -> body-reader; reader parameterizes this per input file.
(define current-body-readers (make-parameter (hasheq)))

(define (lookup-body-reader name)
  (hash-ref (current-body-readers) name default-body-reader))

(define (merge-body-readers! table)
  (current-body-readers
   (for/fold ([acc (current-body-readers)])
             ([(k v) (in-hash table)])
     (hash-set acc k v))))
