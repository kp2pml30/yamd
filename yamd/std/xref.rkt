#lang racket/base
;; Cross-references, glossary and table of contents — the
;; two-pass showcase (see docs/architecture.md "Two-pass evaluation"). A plugin:
;; nothing in core requires it; documents get it via `#use(std/xref)`.
;;
;;   #def(id){term}{definition}   record a definition, emit an anchor
;;   #ref(id)                     delayed link to the def site (+ backlink)
;;   #glossary                    delayed: all defs, sorted, with backlinks
;;   #toc                         delayed: nested list from `section` data
;;
;; NOTE on syntax vs docs/lang/design.md. DESIGN writes `#def(id){term}: definition`.
;; The implemented reader does not accept a `: rest-of-line`
;; body AFTER a `{...}` body — a call is either brace-bodied or `:`-bodied,
;; not both — so that exact form parses `{term}` as the definition body and
;; leaves `: definition` as trailing prose (uncollected). `def` therefore
;; takes the term as the FIRST body and the definition as the REMAINING
;; body/bodies; the two reader-supported spellings are:
;;   #def("vm"){virtual machine}{A sandboxed execution environment.}
;;   #def("vm"){virtual machine}::            (definition = the hanging block)
;; A term-only #def("vm"){virtual machine} is allowed (empty definition).
;;
;; The `id` is host code inside `(...)`, so write it as a string or a quoted
;; symbol: #def("vm"){…} or #def('vm){…}; a bare #def(vm) is a variable
;; reference, per the reader's "args are host expressions" rule.

(require "../runtime.rkt"
         (only-in racket/list append-map))

(provide def ref glossary toc
         ;; Additive: collect-time def registration WITHOUT emitting an anchor
         ;; element, for plugins that pre-declare a fixed vocabulary and render
         ;; the id targets in their own output (std/rfc2119). See register-def!.
         register-def!)

;; Collected per-definition record. `def` is the list of definition bodies
;; (each a Content list); empty for a term-only #def.
(struct xdef (term def anchor) #:transparent)

;; ---------------------------------------------------------------------------
;; ids

;; A cross-reference id is host data: a symbol or a string. Anything else is a
;; located error (raised at collect time, so eval.rkt prefixes the srcloc).
(define (xref-key who id)
  (cond
    [(symbol? id) (symbol->string id)]
    [(string? id) id]
    [else (error who "cross-reference id must be a symbol or string, got ~e" id)]))

;; Info keys. Per-def data lives under (list 'xref-def key); the insertion
;; order of def ids under 'xref-def-ids lets #glossary enumerate them;
;; backlinks per key accumulate under (list 'xref-backlink key).
(define (def-key key) (list 'xref-def key))
(define (backlink-key key) (list 'xref-backlink key))

;; ---------------------------------------------------------------------------
;; #def

(define (def id term . def-bodies)
  (define anchor (register-def! id term def-bodies))
  ;; Visible def site: a <dfn> anchor whose id is the #ref link target.
  (elem 'dfn #:attrs (list (cons 'id anchor)) term))

;; Collect-time bookkeeping shared by `#def` and by plugins that pre-declare a
;; fixed vocabulary (std/rfc2119): record `term` + `def-bodies` under the def
;; key, and register `id` in the enumeration order so #glossary lists it. Unlike
;; `def` this emits NO element — the id target is rendered by the caller (a
;; #ref points at `#def-<id>`, so SOMETHING must carry that id, e.g. rfc2119's
;; boilerplate paragraph). Returns the anchor string. Duplicate id is an error,
;; as with `#def`. `def-bodies` is the list of definition Content lists (empty
;; = term-only). `#:who` names the caller in the duplicate-id error.
(define (register-def! id term [def-bodies '()] #:who [who 'def])
  (define key (xref-key who id))
  (define anchor (string-append "def-" key))
  (define i (current-info))
  (when i
    (when (info-ref i (def-key key) #f)
      (error who "duplicate #def for id ~s" key))
    (info-set! i (def-key key) (xdef term def-bodies anchor))
    (info-add! i 'xref-def-ids key))
  anchor)

;; ---------------------------------------------------------------------------
;; #ref

;; Records a backlink at COLLECT time (the ref site gets a stable anchor now),
;; and returns a delayed link resolved in pass 2 — order-independent, so a
;; ref may precede its def. An unknown id is a dangling reference: an error at
;; resolve time (dangling refs are errors, never empty text). Optional body =
;; custom link text; default link text is the definition term.
(define (ref id . text-bodies)
  (define key (xref-key 'ref id))
  (define i (current-info))
  (define anchor
    (if i (allocate-ref-anchor i) (symbol->string (gensym 'ref-))))
  (when i
    (info-add! i (backlink-key key) anchor))
  (define custom (and (pair? text-bodies) text-bodies))
  (delayed
   (lambda (info)
     (define d (info-ref info (def-key key) #f))
     (unless d
       (error 'ref "dangling cross-reference #ref(~a): no matching #def" key))
     (elem 'a
           #:attrs (list (cons 'id anchor)
                         (cons 'href (string-append "#" (xdef-anchor d))))
           (or custom (xdef-term d))))))

(define (allocate-ref-anchor i)
  (define n (add1 (info-ref i 'yamd-xref-ref-counter 0)))
  (info-set! i 'yamd-xref-ref-counter n)
  (string-append "ref-" (number->string n)))

;; ---------------------------------------------------------------------------
;; #glossary — delayed: every def, sorted by id, each with its backlinks.

(define (glossary)
  (delayed
   (lambda (info)
     (define ids (sort (reverse (info-ref info 'xref-def-ids '())) string<?))
     (apply elem 'dl #:attrs '((class . "glossary"))
            (append-map (lambda (key) (glossary-entry info key)) ids)))))

(define (glossary-entry info key)
  (define d (info-ref info (def-key key) #f))
  (define backlinks (reverse (info-ref info (backlink-key key) '())))
  (list
   (elem 'dt
         (elem 'a #:attrs (list (cons 'href (string-append "#" (xdef-anchor d))))
               (xdef-term d)))
   (apply elem 'dd (append (xdef-def d) (backrefs backlinks)))))

;; A "[n]" link back to each ref site, in document order; empty if none.
(define (backrefs backlinks)
  (if (null? backlinks)
      '()
      (list " "
            (apply elem 'span #:attrs '((class . "backrefs"))
                   (for/list ([a (in-list backlinks)]
                              [n (in-naturals 1)])
                     (elem 'a #:attrs (list (cons 'href (string-append "#" a)))
                           (string-append "[" (number->string n) "]")))))))

;; ---------------------------------------------------------------------------
;; #toc — delayed: a nested list built from the 'sections info key (documented
;; in std/base.rkt): entries (list depth anchor title), newest-first.

(define (toc)
  (delayed
   (lambda (info)
     ;; Sanitize each title for use as TOC link text. A section title may hold
     ;; two-pass content (a #ref in the heading); resolve it against `info` and
     ;; flatten it for link context (see toc-link-title) so the toc shows the
     ;; ref'd term without nesting an <a> in the toc's own <a> or duplicating
     ;; the ref's anchor id (which lives on the heading's copy).
     (define entries
       (for/list ([e (in-list (reverse (info-ref info 'sections '())))])
         (list (car e) (cadr e) (toc-link-title info (caddr e)))))
     (cond
       [(null? entries) (elem 'nav #:attrs '((class . "toc")))]
       [else
        (define-values (lis _rest) (toc-build entries (apply min (map car entries))))
        (elem 'nav #:attrs '((class . "toc")) (apply elem 'ul lis))]))))

;; Resolve a section title and reduce it to safe link-text: strip element `id`s
;; (the canonical anchor is on the heading; a copy here would duplicate it) and
;; unwrap any nested <a> to its children (an anchor inside the toc's own anchor
;; is invalid HTML). Plain-text titles pass through unchanged, so goldens
;; without refs do not churn.
(define (toc-link-title info title)
  (flatten-for-link (resolve-content title info)))

(define (flatten-for-link c)
  (cond
    [(pair? c) (map flatten-for-link c)]
    [(element? c)
     (define kids (map flatten-for-link (element-children c)))
     (if (eq? (element-tag c) 'a)
         kids ; drop the anchor wrapper, keep its content as plain link text
         (element (element-tag c)
                  (filter (lambda (a) (not (eq? (car a) 'id))) (element-attrs c))
                  kids
                  (element-loc c)))]
    [else c]))

;; Builds the <li>s for entries at nesting `level`, recursing for deeper
;; ones; returns (values lis remaining). Section depths are contiguous in
;; document order (a depth-d heading is always nested inside a depth-(d-1)
;; one), so the deeper-than-level branch is a defensive fallback, not the
;; common path.
(define (toc-build entries level)
  (let loop ([es entries] [lis '()])
    (cond
      [(null? es) (values (reverse lis) '())]
      [else
       (define e (car es))
       (define d (car e))
       (cond
         [(< d level) (values (reverse lis) es)]
         [(> d level)
          (define-values (sub rest) (toc-build es (add1 level)))
          (if (pair? lis)
              (loop rest (cons (li-add-sublist (car lis) sub) (cdr lis)))
              (loop rest (cons (elem 'li (apply elem 'ul sub)) lis)))]
         [else
          (define link
            (elem 'a #:attrs (list (cons 'href (string-append "#" (cadr e))))
                  (caddr e)))
          (define-values (sub rest) (toc-build (cdr es) (add1 level)))
          (define li (if (null? sub) (elem 'li link)
                         (elem 'li link (apply elem 'ul sub))))
          (loop rest (cons li lis))])])))

(define (li-add-sublist li sub)
  (element 'li (element-attrs li)
           (append (element-children li) (list (apply elem 'ul sub)))
           #f))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; def emits a <dfn> anchor around the term.
  (check-equal? (def "vm" (list "virtual machine"))
                (elem 'dfn #:attrs '((id . "def-vm")) (list "virtual machine")))

  ;; non-string/symbol id is rejected.
  (check-exn #rx"must be a symbol or string" (lambda () (def 42 (list "t"))))

  ;; ref returns a delayed node and records a backlink at collect time.
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (define d1 (def "vm" (list "virtual machine")))
      (define r (ref "vm"))
      (check-pred delayed? r)
      ;; one backlink recorded, ref anchor ref-1
      (check-equal? (info-ref i (backlink-key "vm") #f) (list "ref-1"))
      ;; resolves to a link to the def site with the term as text
      (check-equal? ((delayed-proc r) i)
                    (elem 'a #:attrs '((id . "ref-1") (href . "#def-vm"))
                          (list "virtual machine")))))

  ;; dangling ref: resolve-time error naming the id.
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (define r (ref "missing"))
      (check-exn #rx"dangling cross-reference"
                 (lambda () ((delayed-proc r) i)))))

  ;; duplicate def is an error.
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (def "dup" (list "a"))
      (check-exn #rx"duplicate #def" (lambda () (def "dup" (list "b"))))))

  ;; toc builds nested nav/ul from section entries (depths 1,2,1).
  (let ([i (make-info)])
    (info-add! i 'sections (list 1 "sec-1" "Alpha"))
    (info-add! i 'sections (list 2 "sec-2" "Alpha One"))
    (info-add! i 'sections (list 1 "sec-3" "Beta"))
    (define t ((delayed-proc (toc)) i))
    (check-equal?
     t
     (elem 'nav #:attrs '((class . "toc"))
           (elem 'ul
                 (elem 'li (elem 'a #:attrs '((href . "#sec-1")) "Alpha")
                       (elem 'ul (elem 'li (elem 'a #:attrs '((href . "#sec-2")) "Alpha One"))))
                 (elem 'li (elem 'a #:attrs '((href . "#sec-3")) "Beta")))))))
