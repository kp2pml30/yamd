#lang racket/base
;; A project-local YAMD plugin: admonition boxes and keyboard shortcuts.
;; Loaded by the book with a document-relative path:
;;
;;   #use("ext/admonition.rkt")
;;
;; It demonstrates the two halves of the plugin ABI:
;;
;;   1. an ORDINARY FUNCTION (`admonition`) — Content in, IR out, with
;;      keyword arguments and a context-supplied default; no reader
;;      registration needed because its body is plain markup;
;;   2. a CUSTOM BODY READER (`keys`) — a tiny DSL parsed at READ time by
;;      the `yamd-reader` submodule, with located parse errors.
;;
;; Both deliberately emit only portable IR tags (blockquote/p/strong/span/
;; code), so the book renders through the html, markdown, AND typst
;; backends. A plugin emitting e.g. a bare `div` would render to HTML but
;; be rejected by the typst backend.

(require racket/string
         (only-in racket/list add-between)
         yamd/runtime)

(provide admonition keys)

;; ---------------------------------------------------------------------------
;; #admonition(kind: "warning"){body} — a call-out box.
;;
;; `kind:` picks the flavor; when omitted, the default is read from the
;; CONTEXT key 'admonition-kind (falling back to "note"), so a document can
;; set a scoped default:
;;
;;   #fork-context::
;;   #(context-set! 'admonition-kind "warning")
;;   #admonition{Defaults to a warning inside this block.}
;;
;; `title:` overrides the heading text (default: the capitalized kind).
;; Output: <blockquote class="admonition admonition-KIND"> with a leading
;; <p class="admonition-title"><strong>Title</strong></p>. The classes are
;; presentation hints the markdown/typst backends drop; the content survives
;; everywhere.

(define known-kinds '("note" "tip" "warning" "danger"))

(define (admonition #:kind [kind #f] #:title [title #f] . bodies)
  (define k (or kind (context-ref 'admonition-kind "note")))
  (unless (and (string? k) (member k known-kinds))
    (error 'admonition "unknown kind ~e; use one of ~a"
           k (string-join known-kinds ", ")))
  (define heading (or title (string-titlecase k)))
  (apply elem 'blockquote
         #:attrs (list (cons 'class (string-append "admonition admonition-" k)))
         (elem 'p #:attrs '((class . "admonition-title"))
               (elem 'strong heading))
         bodies))

;; ---------------------------------------------------------------------------
;; #keys{Ctrl+Shift+P} — a keyboard-shortcut DSL.
;;
;; The body is NOT markup: the yamd-reader submodule below registers a custom
;; parser that splits the text on `+` at read time, so the runtime function
;; receives a plain list of key names. Rendered as <code> chunks joined by
;; `+` inside a <span class="keys">.

(define (keys parts)
  (elem 'span #:attrs '((class . "keys"))
        (add-between (map (lambda (p) (elem 'code p)) parts) "+")))

;; ---------------------------------------------------------------------------
;; Read-time registration. The reader consults this submodule when the plugin
;; is loaded via #use; `keys` gets a custom parser (signature
;; (-> string? srcloc? syntax?)), `admonition` needs no entry — its bodies
;; are default markup.

(module* yamd-reader #f
  (require yamd/reader/registry
           racket/string)

  (provide body-readers)

  (define (parse-keys text loc)
    (define parts (map string-trim (string-split (string-trim text) "+" #:trim? #f)))
    (when (or (null? parts) (ormap (lambda (p) (string=? p "")) parts))
      (error 'keys "~a:~a:~a: malformed shortcut ~s; expected KEY(+KEY)*, e.g. Ctrl+Shift+P"
             (srcloc-source loc) (srcloc-line loc) (srcloc-column loc)
             (string-trim text)))
    ;; The returned syntax is the single body argument of the `#keys` call,
    ;; i.e. `(keys <this>)` — the key names as quoted data.
    (datum->syntax #f (list 'quote parts) loc))

  (define body-readers
    (hasheq 'keys (body-reader parse-keys #f))))

(module+ test
  (require rackunit)

  ;; admonition: explicit kind and title
  (check-equal?
   (admonition #:kind "warning" "careful")
   (elem 'blockquote #:attrs '((class . "admonition admonition-warning"))
         (elem 'p #:attrs '((class . "admonition-title")) (elem 'strong "Warning"))
         "careful"))
  (check-equal?
   (admonition #:kind "tip" #:title "Pro tip" "x")
   (elem 'blockquote #:attrs '((class . "admonition admonition-tip"))
         (elem 'p #:attrs '((class . "admonition-title")) (elem 'strong "Pro tip"))
         "x"))
  (check-exn #rx"unknown kind" (lambda () (admonition #:kind "shrug" "x")))

  ;; kind defaults from the context, with "note" as the final fallback
  (check-regexp-match
   "admonition-note"
   (cdr (assq 'class (element-attrs (call-with-fresh-context
                                     (lambda () (admonition "x")))))))
  (check-regexp-match
   "admonition-danger"
   (cdr (assq 'class (element-attrs
                      (call-with-fresh-context
                       (lambda ()
                         (context-set! 'admonition-kind "danger")
                         (admonition "x")))))))

  ;; keys: runtime IR shape
  (check-equal?
   (keys '("Ctrl" "P"))
   (elem 'span #:attrs '((class . "keys"))
         (list (elem 'code "Ctrl") "+" (elem 'code "P"))))
  (check-equal?
   (keys '("F5"))
   (elem 'span #:attrs '((class . "keys")) (list (elem 'code "F5"))))

  ;; keys: the read-time parser accepts/rejects
  (require (submod ".." yamd-reader)
           (only-in yamd/reader/registry body-reader-parse))
  (define parse (body-reader-parse (hash-ref body-readers 'keys)))
  (define loc (srcloc "doc.yamd" 1 0 #f #f))
  (check-equal? (syntax->datum (parse "Ctrl + Shift+P" loc))
                '(quote ("Ctrl" "Shift" "P")))
  (check-exn #rx"malformed shortcut" (lambda () (parse "Ctrl++P" loc)))
  (check-exn #rx"malformed shortcut" (lambda () (parse "  " loc))))
