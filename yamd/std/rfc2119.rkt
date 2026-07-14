#lang racket/base
;; RFC 2119 / RFC 8174 requirement keywords as pre-declared cross-references.
;; A plugin built ON std/xref's collect/resolve machinery; nothing in core
;; requires it. A document gets it with:
;;
;;   #use(std/rfc2119)
;;
;; and may then #ref the requirement keywords by lowercase-kebab id and list
;; them (with backlinks) via #glossary:
;;
;;   The server #ref('must) validate the token; it #ref('should-not) log it.
;;   #glossary
;;
;; Ids are host code inside (...), so quote them: #ref('must), #ref("shall-not").
;; A bare #ref(must) is a variable reference, per the reader's "args are host
;; expressions" rule (same caveat as std/xref).
;;
;; ---------------------------------------------------------------------------
;; Keywords, ids, and the alias-vs-separate-def decision
;;
;; RFC 2119 (with RFC 8174's NOT RECOMMENDED addition) groups eleven spellings
;; into FIVE distinct requirement levels. We keep ONE canonical definition per
;; level and treat the synonyms as ALIASES that resolve to it, so #glossary
;; lists five entries, not eleven near-identical ones:
;;
;;   canonical id  keyword     aliases (id -> keyword)
;;   ------------  ---------   ----------------------------------------
;;   must          MUST        required (REQUIRED), shall (SHALL)
;;   must-not      MUST NOT    shall-not (SHALL NOT)
;;   should        SHOULD      recommended (RECOMMENDED)
;;   should-not    SHOULD NOT  not-recommended (NOT RECOMMENDED)  [RFC 8174]
;;   may           MAY         optional (OPTIONAL)
;;
;; #ref on an alias id renders the alias's own uppercase keyword (so
;; #ref('required) reads "REQUIRED"), but LINKS to — and records its backlink
;; under — the canonical level, so the glossary aggregates every synonym's
;; backlinks under the one MUST / SHOULD / ... entry. This is done by wrapping
;; std/xref's `ref`: an alias id is redirected to the canonical id with the
;; alias keyword supplied as the link text.
;;
;; ---------------------------------------------------------------------------
;; Anchor / boilerplate semantics (sphinx-style)
;;
;; The five canonical defs are registered into the info table at COLLECT time
;; (so #ref / #glossary resolve), but registration emits NO elements — a bare
;; #use(std/rfc2119) that refs nothing must not litter the output with stray
;; anchors. Each def's id target (`#def-must`, ...) is therefore rendered by
;; ONE place, on demand:
;;
;;   #rfc2119-boilerplate  emits the standard "The key words MUST ... are to be
;;                         interpreted as described in RFC 2119." paragraph, and
;;                         that paragraph CARRIES the five canonical <dfn> id
;;                         anchors (the alias words in it are plain text — their
;;                         refs point at the canonical anchor).
;;
;; Without the boilerplate the keywords are "glossary-only": #glossary still
;; lists all five with backlinks, and #ref links still render with the right
;; text and href, but the `#def-<id>` fragment has no in-page target element
;; (exactly like a sphinx doc that forgets the RFC-2119 admonition). Emit
;; #rfc2119-boilerplate ONCE, conventionally near the top, to give the links
;; a target; a second emission in the same document would duplicate the id
;; anchors and is an explicit collect-time error. This keeps the whole
;; mechanism inside std/xref's existing machinery with NO core changes; the
;; only std/xref addition is the element-less `register-def!` (see xref.rkt).
;;
;; Caveat: the wrapped #glossary pre-registers the keywords, so a document
;; that #use's this plugin gets the five RFC entries in its glossary even if
;; it only refs its own #def'd terms. If that is unwanted, #use(std/xref)
;; instead and reserve std/rfc2119 for documents that speak RFC 2119.
;;
;; ---------------------------------------------------------------------------
;; Registration timing
;;
;; There is no per-#use collect hook, so registration is idempotent (guarded by
;; a per-info flag) and triggered by whichever public entry point the document
;; actually reaches at collect time: the wrapped `ref`, the wrapped `glossary`,
;; or `rfc2119-boilerplate`. All three run in pass 1 with `current-info` bound,
;; so the five defs are in place before pass 2 resolves any ref or the glossary.

(require "../runtime.rkt"
         (prefix-in xref: "xref.rkt"))

;; Re-provide the xref surface a document wants under a single #use, plus our
;; wrappers and the boilerplate. `ref` and `glossary` are OUR wrappers (they
;; ensure keyword registration and handle alias ids); `def` and `toc` pass
;; through unchanged, so a document may still define its own terms / build a toc.
(provide (rename-out [rfc-ref ref]
                     [rfc-glossary glossary])
         (rename-out [xref:def def]
                     [xref:toc toc]
                     [xref:xref-scope xref-scope]
                     [xref:defalias defalias])
         rfc2119-boilerplate)

;; ---------------------------------------------------------------------------
;; The keyword table.

;; Each canonical entry: id, uppercase keyword, definition Content (a faithful,
;; short paraphrase of RFC 2119's text — the normative meaning, in our words).
(define canonical-keywords
  (list
   (list "must" "MUST"
         "An absolute requirement of the specification.")
   (list "must-not" "MUST NOT"
         "An absolute prohibition of the specification.")
   (list "should" "SHOULD"
         (string-append
          "Recommended: there may exist valid reasons in particular"
          " circumstances to ignore it, but the full implications must be"
          " understood and carefully weighed first."))
   (list "should-not" "SHOULD NOT"
         (string-append
          "Not recommended: there may exist valid reasons in particular"
          " circumstances when the behavior is acceptable or even useful, but"
          " the full implications should be understood and the case weighed"
          " before choosing it."))
   (list "may" "MAY"
         (string-append
          "Truly optional: an implementation may choose to include the item"
          " because a particular market or situation calls for it, or to omit"
          " it; interoperating peers must handle both."))))

;; Alias id -> (list canonical-id keyword). Same requirement level, different
;; spelling; ref renders `keyword`, links to `canonical-id`.
(define alias-keywords
  (hash "required"        (list "must" "REQUIRED")
        "shall"           (list "must" "SHALL")
        "shall-not"       (list "must-not" "SHALL NOT")
        "recommended"     (list "should" "RECOMMENDED")
        "not-recommended" (list "should-not" "NOT RECOMMENDED")
        "optional"        (list "may" "OPTIONAL")))

;; ---------------------------------------------------------------------------
;; Collect-time registration (idempotent per document / per info table).

(define registered-flag 'rfc2119-registered)

(define (ensure-registered!)
  (define i (current-info))
  (when (and i (not (info-ref i registered-flag #f)))
    (info-set! i registered-flag #t)
    (for ([kw (in-list canonical-keywords)])
      ;; RFC keywords are a GLOBAL vocabulary: register at root even if the
      ;; first #ref that triggers this happens inside an #xref-scope, so the
      ;; keywords resolve everywhere in the document.
      (xref:register-def! (car kw) (list (cadr kw)) (list (list (caddr kw)))
                          #:who 'rfc2119 #:at-root? #t))))

;; ---------------------------------------------------------------------------
;; ids

(define (id->string id)
  (cond
    [(symbol? id) (symbol->string id)]
    [(string? id) id]
    [else (error 'rfc2119 "keyword id must be a symbol or string, got ~e" id)]))

;; ---------------------------------------------------------------------------
;; #ref — wraps std/xref's ref: ensures registration, and redirects an alias id
;; to its canonical level while keeping the alias's own keyword as link text.

;; Accepts all four #ref spellings — #ref('must), #ref{must}, #ref("must"){txt},
;; #ref(to: "must"){txt} — by delegating the id/custom-text split to xref's own
;; parser, then applying the RFC alias redirection on the resolved id.
(define (rfc-ref #:to [to #f] . args)
  (ensure-registered!)
  (define-values (id text-bodies) (xref:ref-id+custom to args))
  (define key (id->string id))
  (define alias (hash-ref alias-keywords key #f))
  (cond
    ;; Explicit link text: honor it, but still point an alias at its canonical.
    [(and alias (pair? text-bodies))
     (apply xref:ref (car alias) text-bodies)]
    [alias
     (xref:ref (car alias) (list (cadr alias)))]
    ;; Canonical keyword or a non-keyword id: plain xref (a truly unknown id
    ;; stays a dangling-ref error at resolve time, per xref). `id` is now a
    ;; plain string/symbol, so re-apply positionally.
    [else (apply xref:ref id text-bodies)]))

;; ---------------------------------------------------------------------------
;; #glossary — ensure the keywords exist, then defer to xref's glossary.

(define (rfc-glossary)
  (ensure-registered!)
  (xref:glossary))

;; ---------------------------------------------------------------------------
;; #rfc2119-boilerplate — the standard interpretation paragraph, carrying the
;; canonical id anchors. Registers the keywords too (so it works standalone).

;; A canonical keyword as its <dfn id="def-..."> anchor (the id target #ref
;; links to); an alias keyword as plain text.
(define (canonical-anchor id keyword)
  (elem 'dfn #:attrs (list (cons 'id (string-append "def-" id))) keyword))

(define boilerplate-flag 'rfc2119-boilerplate-emitted)

(define (rfc2119-boilerplate)
  (ensure-registered!)
  ;; Duplicate emission would duplicate the <dfn> id anchors (invalid HTML,
  ;; ambiguous fragment targets) — explicit error, not silent duplication.
  (let ([i (current-info)])
    (when i
      (when (info-ref i boilerplate-flag #f)
        (error 'rfc2119-boilerplate
               "#rfc2119-boilerplate emitted more than once in one document; its <dfn id=\"def-...\"> anchors must be unique"))
      (info-set! i boilerplate-flag #t)))
  ;; Interleave the eleven keywords (five as anchors, six as plain aliases)
  ;; with commas / "and" into the conventional sentence. The alias words and
  ;; anchor ids are deliberately spelled out here rather than derived from the
  ;; tables above: the sentence's RFC-conventional word ORDER interleaves
  ;; canonicals and aliases, so a derivation would need its own ordering table
  ;; anyway. The module test cross-checks the anchors against the canonical ids.
  (elem 'p
        "The key words "
        (canonical-anchor "must" "MUST") ", "
        (canonical-anchor "must-not" "MUST NOT") ", "
        "REQUIRED, SHALL, SHALL NOT, "
        (canonical-anchor "should" "SHOULD") ", "
        (canonical-anchor "should-not" "SHOULD NOT") ", "
        "RECOMMENDED, NOT RECOMMENDED, "
        (canonical-anchor "may" "MAY") ", and OPTIONAL"
        " in this document are to be interpreted as described in RFC 2119."))

;; ---------------------------------------------------------------------------
;; Body-reader forwarding: #use(std/rfc2119) re-exports std/xref's surface,
;; including #xref-scope, which needs its lazy (markup, forked) body-reader
;; registered too — the reader only merges the yamd-reader submodule of the
;; directly #use'd module, so mirror xref's registration here.

(module* yamd-reader #f
  (require "../reader/registry.rkt")
  (provide body-readers)
  (define body-readers
    (hasheq 'xref-scope (body-reader 'markup #t))))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit
           (only-in racket/list append-map)
           (only-in "../eval.rkt" yamd-string->html))

  ;; rfc-glossary pre-declares five entries (canonical only, not eleven).
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (rfc-glossary)
      (check-equal? (sort (info-ref i 'xref-def-ids '()) string<?)
                    '("may" "must" "must-not" "should" "should-not"))))

  ;; ref on a canonical id links to its own def and reads its keyword. Every
  ;; xref link now carries the xref-ref class.
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (define r (rfc-ref 'must))
      (check-equal? ((delayed-proc r) i)
                    (elem 'a #:attrs '((class . "xref-ref") (id . "ref-1") (href . "#def-must"))
                          (list "MUST")))))

  ;; ref on an ALIAS id reads the alias keyword but links to the canonical
  ;; def, and its ref is recorded (for backlinks) against the canonical level.
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (define r (rfc-ref 'required))
      ;; The alias keyword is supplied as one custom text body (a Content list,
      ;; as the reader passes a {body}), so it nests one level under the anchor;
      ;; splicing flattens it in output.
      (check-equal? ((delayed-proc r) i)
                    (elem 'a #:attrs '((class . "xref-ref") (id . "ref-1") (href . "#def-must"))
                          (list (list "REQUIRED"))))
      ;; one ref was collected, anchored ref-1
      (check-equal? (map (lambda (v) (vector-ref v 0))
                         (info-ref i 'xref-all-refs '()))
                    '("ref-1"))))

  ;; the new #ref spellings work through the wrapper: #ref{must} (brace id) and
  ;; #ref(to: 'must) resolve like #ref('must).
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (check-equal? ((delayed-proc (rfc-ref (list "must"))) i)
                    (elem 'a #:attrs '((class . "xref-ref") (id . "ref-1") (href . "#def-must"))
                          (list "MUST")))))
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (check-equal? ((delayed-proc (rfc-ref #:to 'must)) i)
                    (elem 'a #:attrs '((class . "xref-ref") (id . "ref-1") (href . "#def-must"))
                          (list "MUST")))))

  ;; boilerplate carries exactly the five canonical id anchors.
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (define p (rfc2119-boilerplate))
      (define ids
        (let walk ([c p])
          (cond
            [(pair? c) (append-map walk c)]
            [(and (element? c) (eq? (element-tag c) 'dfn))
             (list (cdr (assq 'id (element-attrs c))))]
            [(element? c) (append-map walk (element-children c))]
            [else '()])))
      (check-equal? (sort ids string<?)
                    '("def-may" "def-must" "def-must-not"
                      "def-should" "def-should-not"))))

  ;; A second boilerplate in the same document is an explicit error (its id
  ;; anchors must stay unique).
  (let ([i (make-info)])
    (parameterize ([current-info i])
      (rfc2119-boilerplate)
      (check-exn #rx"emitted more than once" rfc2119-boilerplate)))

  ;; Dangling protection: #ref of a keyword WITHOUT the plugin (std/xref only,
  ;; which does not pre-declare the keywords) is a resolve-time error, not
  ;; silent empty text.
  (check-exn
   #rx"dangling cross-reference"
   (lambda ()
     (yamd-string->html "#use(std/xref)\n#ref(\"must\")\n")))

  ;; With the plugin the same reference resolves.
  (check-not-exn
   (lambda ()
     (yamd-string->html "#use(std/rfc2119)\n#ref('must)\n#rfc2119-boilerplate\n"))))
