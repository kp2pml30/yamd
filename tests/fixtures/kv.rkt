#lang racket/base
;; Tiny test plugin exercising the custom body-reader path of the reader.
;; `#kv` parses `key: value` lines (at READ time) into a definition-list IR.
;; Used by tests/golden/reader/custom.yamd via #use("../../fixtures/kv.rkt").

(require racket/list
         yamd/runtime)

(provide kv)

;; Runtime: `#kv` calls this with the parsed pairs; render a <dl> of <dt>/<dd>.
(define (kv pairs)
  (element 'dl '()
           (append*
            (for/list ([p (in-list pairs)])
              (list (elem 'dt (car p))
                    (elem 'dd (cdr p)))))
           #f))

;; Read-time: the custom parser. Signature is (-> string? srcloc? syntax?):
;; it receives the body text (block: dedented; inline: brace-balanced) plus
;; the srcloc the reader assigned, and returns the syntax of the expression
;; passed as the body argument.
(module* yamd-reader #f
  (require yamd/reader/registry
           racket/string
           (for-syntax racket/base))

  (provide body-readers)

  (define (parse-kv text loc)
    (define lines
      (for/list ([ln (in-list (string-split text "\n"))]
                 #:unless (regexp-match? #px"^\\s*$" ln))
        ln))
    (define pairs
      (for/list ([ln (in-list lines)])
        (define m (regexp-match #px"^([^:]+):[ \t]*(.*)$" ln))
        (unless m
          (error 'kv "~a:~a:~a: kv line without `key: value`: ~s"
                 (srcloc-source loc) (srcloc-line loc) (srcloc-column loc) ln))
        (cons (string-trim (cadr m)) (string-trim (caddr m)))))
    ;; The returned syntax is the single body ARGUMENT of the `#kv` call, i.e.
    ;; `(kv <this>)`. Hand `kv` the parsed pairs as quoted data.
    (datum->syntax #f (list 'quote pairs)))

  (define body-readers
    (hasheq 'kv (body-reader parse-kv #f))))
