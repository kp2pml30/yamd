#lang racket/base
;; std/html — raw-HTML escape hatch for the HTML backend.
;; WARNING: Non-portable. Content produced here is opaque to other backends
;; and to the two-pass machinery (cross-references, TOC, etc. cannot see
;; inside raw HTML). Use only when the IR has no suitable representation.
;;
;; Usage:
;;   #use(std/html)
;;   #html{<strong>verbatim HTML</strong>}          ; inline: joins the flow
;;   #(html "<em>via host expression</em>")
;;   #html-block{<div>a block</div>}                ; block: not wrapped in <p>
;;
;; `html` vs `html-block`: both pass their raw body through verbatim, but
;; `html` produces an INLINE raw node (decode wraps it in the surrounding
;; <p>, correct for phrasing content like <strong>/<em>), whereas
;; `html-block` produces a BLOCK raw node — decode treats it like a block
;; element, so it breaks the paragraph and is emitted standalone. Use
;; `html-block` for flow content (<div>, <table>, <ul>, …) to avoid emitting
;; invalid `<p><div>…</div></p>`.

(require "../ir.rkt")

(provide html html-block)

;; Wraps `text` in a (raw 'html ...) IR node, which the HTML backend passes
;; through verbatim. Any other backend will raise an error on this node.
;; When loaded as a plugin (via #use), the yamd-reader submodule registers
;; this function to use a `raw` body reader so that #html{...} passes the
;; brace-balanced text verbatim (no # recognition, no escaping).
(define (html text)
  (unless (string? text)
    (error 'html "expected a string, got ~e" text))
  (raw 'html text))

;; Block-level counterpart: emits a block-flagged raw node so decode does not
;; wrap it in a paragraph. Same verbatim/raw-body semantics as `html`.
(define (html-block text)
  (unless (string? text)
    (error 'html-block "expected a string, got ~e" text))
  (raw 'html text #t))

;; Body-reader registration: both forms get a raw reader so that
;; #html{<em>x</em>} / #html-block{<div>x</div>} do not parse `<`, `>`, or
;; `#` as YAMD.
(module* yamd-reader #f
  (require "../reader/registry.rkt")
  (provide body-readers)
  (define body-readers
    (hasheq 'html (body-reader 'raw #f)
            'html-block (body-reader 'raw #f))))

(module+ test
  (require rackunit
           "../ir.rkt"
           "../backend/html.rkt")

  ;; Basic string passthrough
  (check-equal? (html "<b>bold</b>") (raw 'html "<b>bold</b>"))
  (check-equal? (html "") (raw 'html ""))

  ;; `html` is inline (block? = #f); `html-block` is block (block? = #t).
  (check-equal? (raw-block? (html "<b>x</b>")) #f)
  (check-equal? (html-block "<div>x</div>") (raw 'html "<div>x</div>" #t))
  (check-equal? (raw-block? (html-block "<div>x</div>")) #t)
  ;; Same verbatim passthrough as `html`.
  (check-equal? (content->html (html-block "<div>a & b</div>"))
                "<div>a & b</div>")
  (check-exn #rx"expected a string"
             (lambda () (html-block 42)))

  ;; Characters that would be escaped in normal content are left alone
  (check-equal? (content->html (html "<script>alert('xss')</script>"))
                "<script>alert('xss')</script>")
  (check-equal? (content->html (html "a & b < c > d"))
                "a & b < c > d")

  ;; Wrong type is an error
  (check-exn #rx"expected a string"
             (lambda () (html 42))))
