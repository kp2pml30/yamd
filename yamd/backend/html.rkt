#lang typed/racket/base
;; HTML backend: serializes fully-resolved IR. Refuses delayed nodes and
;; raw nodes addressed to other backends — no silent output corruption.
;;
;; Output format: compact single-line — no whitespace is ever added between
;; elements (HTML whitespace sensitivity makes deterministic pretty-printing
;; intractable in general). Callers that need indented output should post-
;; process with an external formatter.

(require racket/string
         "../ir.rkt")

(provide content->html
         html-escape
         attr-escape)

(define void-tags : (Listof Symbol)
  '(area base br col embed hr img input link meta source track wbr))

(: html-escape (-> String String))
(define (html-escape s)
  (string-replace
   (string-replace
    (string-replace s "&" "&amp;")
    "<" "&lt;")
   ">" "&gt;"))

(: attr-escape (-> String String))
(define (attr-escape s)
  (string-replace (html-escape s) "\"" "&quot;"))

(: content->html (-> Content String))
(define (content->html c)
  (define out (open-output-string))
  (write-content c out)
  (get-output-string out))

(: write-content (-> Content Output-Port Void))
(define (write-content c out)
  (cond
    [(string? c) (write-string (html-escape c) out) (void)]
    [(element? c) (write-element c out)]
    [(raw? c)
     (unless (eq? (raw-backend c) 'html)
       (error 'content->html "raw node for backend ~a cannot be rendered to html" (raw-backend c)))
     (write-string (raw-text c) out)
     (void)]
    [(delayed? c)
     (error 'content->html "unresolved delayed node; run resolve-content first")]
    [else
     (for ([ch (in-list c)]) (write-content ch out))]))

;; Tag/attr *names* are not escapable in HTML — reject anything unsafe
;; instead of emitting injectable markup.
(define name-rx #rx"^[a-zA-Z][a-zA-Z0-9-]*$")

(: check-name (-> Symbol String (U srcloc False) Void))
(define (check-name sym what loc)
  (unless (regexp-match? name-rx (symbol->string sym))
    (error 'content->html "invalid ~a name ~e (at ~a)" what sym loc)))

(: write-element (-> element Output-Port Void))
(define (write-element e out)
  (define tag (element-tag e))
  (when (regexp-match? #rx"^%" (symbol->string tag))
    (error 'content->html "internal element ~a leaked into backend (at ~a)"
           tag (element-loc e)))
  (check-name tag "tag" (element-loc e))
  (write-string "<" out)
  (write-string (symbol->string tag) out)
  (for ([attr (in-list (element-attrs e))])
    (check-name (car attr) "attribute" (element-loc e))
    (write-string " " out)
    (write-string (symbol->string (car attr)) out)
    (write-string "=\"" out)
    (write-string (attr-escape (cdr attr)) out)
    (write-string "\"" out))
  (cond
    [(memq tag void-tags)
     (unless (null? (element-children e))
       (error 'content->html "void element ~a has children" tag))
     (write-string " />" out)]
    [else
     (write-string ">" out)
     (for ([ch (in-list (element-children e))])
       (write-content ch out))
     (write-string "</" out)
     (write-string (symbol->string tag) out)
     (write-string ">" out)])
  (void))

;; ---------------------------------------------------------------------------
;; Tests

(module+ test
  (require typed/rackunit)

  ;; --- html-escape ---
  (check-equal? (html-escape "Hello") "Hello")
  (check-equal? (html-escape "a < b & c > d") "a &lt; b &amp; c &gt; d")
  (check-equal? (html-escape "<script>alert(1)</script>") "&lt;script&gt;alert(1)&lt;/script&gt;")
  ;; Double quotes are NOT escaped in text content — only needed in attrs.
  (check-equal? (html-escape "say \"hi\"") "say \"hi\"")

  ;; --- attr-escape ---
  (check-equal? (attr-escape "foo?a=1&b=<2>") "foo?a=1&amp;b=&lt;2&gt;")
  (check-equal? (attr-escape "say \"hi\"") "say &quot;hi&quot;")

  ;; --- plain text ---
  (check-equal? (content->html "plain text") "plain text")
  (check-equal? (content->html "<>&\"") "&lt;&gt;&amp;\"")

  ;; --- simple element ---
  (check-equal? (content->html (element 'em '() (list "emphasis") #f))
                "<em>emphasis</em>")

  ;; --- attrs: escaping ---
  (check-equal? (content->html (element 'a '((href . "u?a=1&b=<x>\""))
                                        (list "link") #f))
                "<a href=\"u?a=1&amp;b=&lt;x&gt;&quot;\">link</a>")

  ;; --- void element: no children ---
  (check-equal? (content->html (element 'br '() '() #f)) "<br />")
  (check-equal? (content->html (element 'hr '((class . "sep")) '() #f))
                "<hr class=\"sep\" />")

  ;; --- void element with children is an error ---
  (check-exn #rx"void element br has children"
             (lambda () (content->html (element 'br '() (list "x") #f))))

  ;; --- raw html passthrough ---
  (check-equal? (content->html (raw 'html "<b>bold</b>")) "<b>bold</b>")

  ;; --- raw node for wrong backend is an error ---
  (check-exn #rx"raw node for backend latex"
             (lambda () (content->html (raw 'latex "\\textbf{x}"))))

  ;; --- unresolved delayed node is an error ---
  (check-exn #rx"unresolved delayed node"
             (lambda () (content->html (delayed (lambda (i) "")))))

  ;; --- internal %-tag leak is an error ---
  (check-exn #rx"internal element %para-break leaked"
             (lambda () (content->html (element '%para-break '() '() #f))))

  ;; --- invalid tag name is an error ---
  (check-exn #rx"invalid tag name"
             (lambda () (content->html (element '|bad tag| '() '() #f))))

  ;; --- invalid attr name is an error ---
  (check-exn #rx"invalid attribute name"
             (lambda () (content->html (element 'div (list (cons '|bad attr| "v")) '() #f))))

  ;; --- list (splice) ---
  (check-equal? (content->html (list "a" (element 'em '() (list "b") #f) "c"))
                "a<em>b</em>c"))
