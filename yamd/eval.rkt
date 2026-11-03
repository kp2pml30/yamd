#lang racket/base
;; Evaluation pipeline: read -> eval (= collect pass) -> resolve -> content.
;; Documents evaluate in a fresh namespace with yamd/runtime (and, by
;; default, yamd/std/base) attached so IR struct identities are shared with
;; the caller.

(require racket/path
         "ir.rkt"
         "info-table.rkt"
         "passes.rkt"
         "decode.rkt"
         "runtime.rkt"
         (only-in "image.rkt" current-image-resolver)
         "docset.rkt"
         "backend/html.rkt"
         "reader/reader.rkt"
         "reader/registry.rkt")

(provide make-yamd-namespace
         yamd-eval
         yamd-eval-embedded
         call-with-document-source
         yamd-read-file
         yamd-file->content
         yamd-file->html
         yamd-string->content
         yamd-string->html)

(define-namespace-anchor anchor)

(define default-requires '(yamd/std/base))

;; Seed body-readers from the default modules' `yamd-reader` submodules
;; (docs/architecture.md "Body-reader dispatch"), so the reader dispatches plugin
;; bodies of the always-attached modules without an explicit #use. Extra
;; plugins are merged at read time by the reader when it sees #use.
(define (seed-body-readers mods)
  (for/fold ([acc (hasheq)]) ([m (in-list mods)])
    (define subm `(submod ,m yamd-reader))
    (if (module-declared? subm #t)
        (for/fold ([a acc]) ([(k v) (in-hash (dynamic-require subm 'body-readers))])
          (hash-set a k v))
        acc)))

(define (call-with-seeded-readers thunk)
  (parameterize ([current-body-readers (seed-body-readers default-requires)])
    (thunk)))

;; Attached (struct/parameter identity shared) but NOT namespace-required —
;; documents must not see these modules' bindings; plugins that require them
;; (std/include, std/toctree need yamd/docset + yamd/eval) resolve to the
;; same instantiation as the builder instead of a fresh, state-less copy.
(define attach-only '(yamd/docset yamd/eval))

(define (make-yamd-namespace #:requires [mods default-requires])
  (define here (namespace-anchor->namespace anchor))
  (define ns (make-base-namespace))
  (define required (cons 'yamd/runtime mods))
  (define attached (append attach-only required))
  (parameterize ([current-namespace here])
    (for ([m (in-list attached)])
      (dynamic-require m #f)))
  (for ([m (in-list attached)])
    (namespace-attach-module here m ns))
  (parameterize ([current-namespace ns])
    (for ([m (in-list required)])
      (namespace-require m)))
  ns)

;; Best-effort source locations for runtime errors: prefix the exception
;; message with the srcloc of the top-level form being evaluated, keeping
;; the exception type (handlers upstream must still see e.g.
;; exn:fail:contract?). Syntax/read errors already carry srclocs — pass
;; them through, likewise messages that already start with a srcloc.
(define has-srcloc-prefix-rx #px"^[^\n]*:[0-9]+:[0-9]+: ")

(define (add-srcloc-prefix e stx)
  (cond
    [(not (and (syntax? stx) (syntax-source stx) (syntax-line stx))) e]
    [(exn:fail:syntax? e) e]
    [(exn:fail:read? e) e]
    [(regexp-match? has-srcloc-prefix-rx (exn-message e)) e]
    [else
     (define msg (format "~a:~a:~a: ~a"
                         (syntax-source stx) (syntax-line stx) (syntax-column stx)
                         (exn-message e)))
     (define marks (exn-continuation-marks e))
     (cond
       [(exn:fail:contract:variable? e)
        (make-exn:fail:contract:variable msg marks (exn:fail:contract:variable-id e))]
       [(exn:fail:contract:arity? e) (make-exn:fail:contract:arity msg marks)]
       [(exn:fail:contract:divide-by-zero? e)
        (make-exn:fail:contract:divide-by-zero msg marks)]
       [(exn:fail:contract? e) (make-exn:fail:contract msg marks)]
       [(exn:fail? e) (make-exn:fail msg marks)]
       [else e])]))

(define (eval-form stx ns)
  (with-handlers ([exn:fail? (lambda (e) (raise (add-srcloc-prefix e stx)))])
    (eval stx ns)))

;; Evaluates reader output; returns (values content info) — NOT yet resolved
;; or paragraph-decoded. Evaluation IS the collect pass: functions may
;; collect!/info-add! and return delayed nodes. Callers must
;; resolve-content, THEN decode-flow — in that order, so delayed nodes that
;; resolve to block-level content (toc, glossary) participate in paragraph
;; grouping instead of being trapped inside a <p>.
(define (yamd-eval stxs #:namespace [ns (make-yamd-namespace)])
  (define i (make-info))
  (define content
    ;; per-document: a resolver set inside the document dies with it
    (parameterize ([current-info i]
                   [current-image-resolver (current-image-resolver)])
      (call-with-fresh-context
       (lambda ()
         (for/list ([stx (in-list stxs)])
           (->content (eval-form stx ns)))))))
  (values content i))

(define (yamd-read-file path)
  (call-with-seeded-readers
   (lambda ()
     (call-with-input-file path
       (lambda (in) (yamd-read-syntax path in))))))

;; Evaluates another .yamd file as part of the CURRENT document evaluation
;; (docs/multi-doc.md "Include semantics"): same info table (its collect!
;; calls land in the enclosing pass), fresh namespace (host definitions do
;; not leak either way), forked context (it reads the enclosing values —
;; header-depth auto-nesting — but its writes die with the fork; a toctree
;; child additionally resets 'header-depth to 0: standalone page, h1 top
;; sections). `spec` is resolved relative to the file currently being
;; evaluated; cycles error with the whole chain. The child is read with its
;; own path as src, so its errors name the right file:line. Returns the
;; child's Content list, unresolved.
(define (yamd-eval-embedded spec
                            #:who [who 'include]
                            #:reset-header-depth? [reset? #f])
  (define abs (resolve-doc-path who spec (current-yamd-source)))
  (check-doc-cycle who abs)
  (define ns (make-yamd-namespace))
  (define stxs (yamd-read-file abs))
  (define (run)
    (parameterize ([current-yamd-source abs]
                   [current-docset-stack (cons abs (current-docset-stack))]
                   [current-image-resolver (current-image-resolver)])
      (call-with-forked-context
       (lambda ()
         (when reset? (context-set! 'header-depth 0))
         (for/list ([stx (in-list stxs)])
           (->content (eval-form stx ns)))))))
  ;; A reset (toctree) child is a standalone PAGE: it becomes the enclosing
  ;; page for any toctree evaluated inside it. An include (#f reset) keeps
  ;; the current page, so nested toctrees attach to the right doc-graph node.
  (if reset?
      (parameterize ([current-docset-parent abs]) (run))
      (run)))

;; Entry-point plumbing for multi-source documents: the file entry points
;; below bind the source path (relative include/toctree resolution) and seed
;; the cycle stack with the root, so a root that reaches itself errors.
(define (call-with-document-source path thunk)
  (define abs (doc-identity path)) ; same identity rules as include/toctree
  (parameterize ([current-yamd-source abs]
                 [current-docset-stack (list abs)]
                 [current-docset-parent abs])
    (thunk)))

(define (finish-content content i)
  (decode-flow (resolve-content content i)))

;; Full front half: read + collect + resolve + decode. Returns Content.
(define (yamd-file->content path #:namespace [ns (make-yamd-namespace)])
  (call-with-document-source
   path
   (lambda ()
     (define-values (content i) (yamd-eval (yamd-read-file path) #:namespace ns))
     (finish-content content i))))

(define (yamd-file->html path #:namespace [ns (make-yamd-namespace)])
  (content->html (yamd-file->content path #:namespace ns)))

;; Full front half from a string. Symmetric companion to yamd-file->content.
(define (yamd-string->content str #:src [src 'string] #:namespace [ns (make-yamd-namespace)])
  (define stxs
    (call-with-seeded-readers
     (lambda () (yamd-read-syntax src (open-input-string str)))))
  (define-values (content i) (yamd-eval stxs #:namespace ns))
  (finish-content content i))

(define (yamd-string->html str #:src [src 'string] #:namespace [ns (make-yamd-namespace)])
  (content->html (yamd-string->content str #:src src #:namespace ns)))
