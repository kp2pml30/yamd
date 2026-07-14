#lang racket/base
;; Cross-references, glossary and table of contents — the
;; two-pass showcase (see docs/architecture.md "Two-pass evaluation"). A plugin:
;; nothing in core requires it; documents get it via `#use(std/xref)`.
;;
;;   #def(id){term}{definition}   record a definition, emit an anchor
;;   #ref(id)                     delayed link to the def site (+ backlink)
;;   #defalias(target, a, …)      extra ids that resolve to `target` (plurals)
;;   #xref-scope(...)::           open a nested definition scope for its body
;;   #glossary                    delayed: all defs, sorted, with backlinks
;;   #toc                         delayed: nested list from `section` data
;;
;; ---- Scoping (contexts) ----------------------------------------------------
;; `#def`/`#ref` live in the auto-scoped context chain (docs/architecture.md
;; "Contexts"). By DEFAULT every `#def` walks up to the document ROOT and
;; registers there — root-level, cross-file, exactly as a flat table would.
;; The root scope is backed by the shared `info` table, so a `#def` in one
;; file of a document set still resolves a `#ref` in another (unchanged), and
;; plugins that pre-declare a vocabulary (std/rfc2119) keep working.
;;
;; `#xref-scope::` opens an inner scope: `#def`s inside it register on that
;; scope's context frame instead of root, and a `#ref` resolves by walking
;; from its own frame outward — the nearest enclosing `#def` of that id wins
;; (an inner def SHADOWS an outer one; it is never a duplicate error across
;; scopes). A `#ref` in pass 2 walks the frame chain it CAPTURED at collect
;; time (contexts are gone by resolve — see the architecture "sharp edge"),
;; and mutable frames mean a def registered AFTER the ref is still seen, so a
;; ref may still precede its def.
;;
;; ---- Case-insensitivity ----------------------------------------------------
;; Id matching is case-INSENSITIVE: `#ref("VM")` resolves `#def("vm")`. Ids are
;; matched under a case-folded key, but the DEFINITION's own casing is
;; authoritative — the link text is the def's term and the anchor keeps the
;; def's original-cased id (`#def("VM")` → `def-VM`).
;;
;; NOTE on syntax vs docs/lang/index.yamd. DESIGN writes `#def(id){term}: definition`.
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
;;
;; ---- #ref spellings --------------------------------------------------------
;;   #ref("vm")               host-computed / quoted id; text = the def's term
;;   #ref{vm}                 brace-body id (terser; a single plain-text body)
;;   #ref("vm"){custom text}   positional id + custom link text
;;   #ref(to: "vm"){custom}    keyword id + custom link text
;; Rule: the brace body is the ID exactly when no explicit id (positional or
;; `to:`) is supplied; otherwise it is the custom link text.

(require "../runtime.rkt"
         (only-in racket/list append-map remove-duplicates)
         (only-in racket/string string-join))

(provide def ref glossary toc doc-ref
         ;; scope + alias surface
         xref-scope defalias
         ;; Additive: collect-time def registration WITHOUT emitting an anchor
         ;; element, for plugins that pre-declare a fixed vocabulary and render
         ;; the id targets in their own output (std/rfc2119). See register-def!.
         register-def!
         ;; The #ref argument parser (id/custom-text split across the four
         ;; spellings), so a plugin that WRAPS ref (std/rfc2119) understands the
         ;; same spellings instead of reimplementing arg handling.
         ref-id+custom)

;; Collected per-definition record. `def` is the list of definition bodies
;; (each a Content list); empty for a term-only #def. `ref-class` is the
;; def-scope's ref-class token (or #f), baked in at def time so a #ref can add
;; it to the link's class list at resolve without touching contexts.
(struct xdef (term def anchor ref-class) #:transparent)

;; An alias id: resolving it redirects to `target` (a folded id key), looked
;; up from the frame the alias itself lives on (so a scoped alias may target a
;; scoped or a root def).
(struct xalias (target) #:transparent)

;; ---------------------------------------------------------------------------
;; ids

;; A cross-reference id is host data: a symbol or a string. Anything else is a
;; located error (raised at collect time, so eval.rkt prefixes the srcloc).
;; Returns the ORIGINAL casing.
(define (xref-key who id)
  (cond
    [(symbol? id) (symbol->string id)]
    [(string? id) id]
    [else (error who "cross-reference id must be a symbol or string, got ~e" id)]))

;; The case-folded match key.
(define (fold s) (string-foldcase s))

;; Per-def data lives under (list 'xref-def foldedkey) in the ROOT scope
;; (info); the insertion order under 'xref-def-ids lets #glossary enumerate the
;; root defs. Inner scopes keep the same data on their context frame under
;; 'xref-defs / 'xref-def-order (see the frame keys below).
(define (def-key fkey) (list 'xref-def fkey))

;; ---------------------------------------------------------------------------
;; Scope frames
;;
;; A context frame becomes a def-scope when it carries 'xref-scope (#t or a
;; name string). The frame then holds:
;;   'xref-defs        mutable hash: folded-key -> (xdef | xalias)
;;   'xref-def-order   reversed list of folded keys defined here (for glossary)
;;   'xref-ref-class   the scope's ref-class token (or absent)
;;   'xref-scope-id    unique string, disambiguates scoped def anchors
;; Named scopes additionally register name->frame in the ROOT frame under
;; 'xref-named-scopes so #glossary(scope:) can find them at resolve.

;; The nearest enclosing def-scope frame, or #f meaning "root" (use info).
(define (nearest-def-scope-frame)
  (let loop ([fr (current-frame)])
    (cond
      [(not fr) #f]
      [(frame-ref fr 'xref-scope #f) fr]
      [else (loop (frame-parent fr))])))

;; The nearest ref-class token walking outward from `fr` (or #f).
(define (nearest-ref-class fr)
  (let loop ([fr fr])
    (cond
      [(not fr) #f]
      [(frame-ref fr 'xref-ref-class #f) => values]
      [else (loop (frame-parent fr))])))

;; The topmost (root) frame reachable from `fr`.
(define (root-frame [fr (current-frame)])
  (let loop ([fr fr]) (if (frame-parent fr) (loop (frame-parent fr)) fr)))

;; A per-document counter for unique scoped-def anchors.
(define (alloc-scope-id)
  (define i (current-info))
  (cond
    [i (define n (add1 (info-ref i 'yamd-xref-scope-counter 0)))
       (info-set! i 'yamd-xref-scope-counter n)
       (number->string n)]
    [else (symbol->string (gensym 'scope))]))

(define (register-named-scope! name fr)
  (define r (root-frame))
  (define h (or (frame-ref r 'xref-named-scopes #f)
                (let ([x (make-hash)]) (frame-set! r 'xref-named-scopes x) x)))
  (when (hash-has-key? h name)
    (error 'xref-scope "duplicate #xref-scope name ~s" name))
  (hash-set! h name fr))

(define (named-scope-frame captured name)
  (let ([h (frame-ref (root-frame captured) 'xref-named-scopes #f)])
    (and h (hash-ref h name #f))))

;; ---------------------------------------------------------------------------
;; #xref-scope — a lazy, context-forking block that marks its frame a def-scope.

(define (xref-scope #:name [name #f] #:ref-class [ref-class #f] . body-thunks)
  (unless (or (not name) (string? name))
    (error 'xref-scope "name: must be a string, got ~e" name))
  (unless (or (not ref-class) (string? ref-class))
    (error 'xref-scope "ref-class: must be a string, got ~e" ref-class))
  (with-forked-context
    (define fr (current-frame))
    (frame-set! fr 'xref-scope (or name #t))
    (frame-set! fr 'xref-scope-id (alloc-scope-id))
    (when ref-class (frame-set! fr 'xref-ref-class ref-class))
    (when name (register-named-scope! name fr))
    (map (lambda (t) (t)) body-thunks)))

;; ---------------------------------------------------------------------------
;; #def

;; `#:at-root?` registers in the ROOT scope even when the call sits inside an
;; #xref-scope. It is what a plugin whose vocabulary is GLOBAL — one shared by
;; the whole document set, where the first use is an accident of document order
;; (std/rfc2119's keywords, a house-style #req) — must pass, so a scoped
;; document cannot strand the vocabulary in its own scope and dangle every
;; reference elsewhere. Ordinary author-written #defs never want it: an id
;; defined inside a scope belongs to that scope.
(define (def id term #:at-root? [at-root? #f] . def-bodies)
  (define anchor (register-def! id term def-bodies #:at-root? at-root?))
  ;; Visible def site: a <dfn> anchor whose id is the #ref link target.
  (elem 'dfn #:attrs (list (cons 'id anchor)) term))

;; Collect-time bookkeeping shared by `#def` and by plugins that pre-declare a
;; fixed vocabulary (std/rfc2119). Registers `term` + `def-bodies` in the
;; nearest def-scope (root by default) and returns the anchor string. Unlike
;; `def` this emits NO element — the id target is rendered by the caller (a
;; #ref points at the anchor, so SOMETHING must carry that id, e.g. rfc2119's
;; boilerplate paragraph). Duplicate id WITHIN one scope is an error (an id in
;; an INNER scope legitimately shadows an outer one). `#:who` names the caller
;; in the duplicate-id error. `#:at-root?` forces registration into the ROOT
;; scope regardless of the enclosing #xref-scope — for a GLOBAL vocabulary
;; (rfc2119) whose keywords must resolve everywhere, even when the call that
;; triggers registration happens to run inside a scope.
(define (register-def! id term [def-bodies '()] #:who [who 'def] #:at-root? [at-root? #f])
  (define origkey (xref-key who id))
  (define fkey (fold origkey))
  (define scope (and (not at-root?) (nearest-def-scope-frame)))
  (cond
    [scope (register-def-in-frame! who scope fkey origkey term def-bodies)]
    [else  (register-def-in-info! who fkey origkey term def-bodies)]))

(define (register-def-in-info! who fkey origkey term def-bodies)
  (define i (current-info))
  ;; Duplicate check BEFORE claiming an anchor, so a rejected dup leaves no
  ;; anchor reserved.
  (when (and i (info-ref i (def-key fkey) #f))
    (error who "duplicate #def for id ~s" origkey))
  (define anchor (claim-anchor! (string-append "def-" origkey)))
  (when i
    (info-set! i (def-key fkey) (xdef term def-bodies anchor #f))
    (info-add! i 'xref-def-ids fkey))
  anchor)

(define (register-def-in-frame! who scope fkey origkey term def-bodies)
  (define defs (frame-defs! scope))
  (when (hash-has-key? defs fkey)
    (error who "duplicate #def for id ~s in this #xref-scope" origkey))
  (define anchor
    (claim-anchor!
     (string-append "def-" (frame-ref scope 'xref-scope-id "s") "-" origkey)))
  (hash-set! defs fkey (xdef term def-bodies anchor (frame-ref scope 'xref-ref-class #f)))
  (frame-set! scope 'xref-def-order (cons fkey (frame-ref scope 'xref-def-order '())))
  anchor)

;; Reserve a unique HTML-fragment anchor. Anchors are `def-<key>` (root) or
;; `def-<scope-id>-<key>` (scoped); since ids are arbitrary user data, a scoped
;; anchor could textually collide with a root anchor whose id happens to be
;; `<scope-id>-<key>`. A per-DOCUMENT used-set (on the info table — the document
;; identity, fresh per build) makes anchors unique by construction: colliders
;; get a `-2`, `-3`, … suffix. Refs always link via the stored xdef-anchor, so
;; the exact spelling never needs to be reconstructed. Root keys free of a
;; leading `<digit>-` (e.g. rfc2119's "must") never collide with a numeric
;; scope-id anchor, so those stay stable. Without an info table (bare unit
;; tests) there is no shared document, so the base is returned unchanged.
(define (claim-anchor! base)
  (define i (current-info))
  (cond
    [(not i) base]
    [else
     (define used (or (info-ref i 'xref-anchors #f)
                      (let ([h (make-hash)]) (info-set! i 'xref-anchors h) h)))
     (let loop ([n 1])
       (define cand (if (= n 1) base (string-append base "-" (number->string n))))
       (cond
         [(hash-ref used cand #f) (loop (add1 n))]
         [else (hash-set! used cand #t) cand]))]))

;; The scope frame's def hash, created on first use.
(define (frame-defs! scope)
  (or (frame-ref scope 'xref-defs #f)
      (let ([h (make-hash)]) (frame-set! scope 'xref-defs h) h)))

;; ---------------------------------------------------------------------------
;; #defalias — register extra ids that resolve to `target`. Variadic: the first
;; arg is the canonical (existing-or-future) id, every remaining arg is an
;; alias. Aliases live in the nearest def-scope (so they can be scope-local),
;; are not enumerated by #glossary, and resolve to `target` at lookup time
;; (order-independent, like #ref). Emits nothing.
(define (defalias target . aliases)
  (define tkey (fold (xref-key 'defalias target)))
  (when (null? aliases)
    (error 'defalias "#defalias needs at least one alias id besides the target"))
  (define scope (nearest-def-scope-frame))
  (for ([a (in-list aliases)])
    (register-alias! 'defalias scope (fold (xref-key 'defalias a)) a tkey))
  (void))

(define (register-alias! who scope akey aorig tkey)
  (cond
    [scope
     (define defs (frame-defs! scope))
     (when (hash-has-key? defs akey)
       (error who "duplicate #def/#defalias for id ~s in this #xref-scope" aorig))
     (hash-set! defs akey (xalias tkey))]
    [else
     (define i (current-info))
     (when i
       (when (info-ref i (def-key akey) #f)
         (error who "duplicate #def/#defalias for id ~s" aorig))
       (info-set! i (def-key akey) (xalias tkey)))]))

;; ---------------------------------------------------------------------------
;; Resolution: walk a captured frame chain, first hit wins; #f = fall through
;; to the root scope (info). Follows alias redirects (fuel-guarded).
(define (lookup-def start info fkey [fuel 16])
  (cond
    [(<= fuel 0) (error 'ref "alias cycle resolving cross-reference id ~s" fkey)]
    [else
     (let loop ([fr start])
       (cond
         [(not fr)
          (deref (info-ref info (def-key fkey) #f) start info fuel)]
         [(let ([h (frame-ref fr 'xref-defs #f)]) (and h (hash-ref h fkey #f)))
          => (lambda (v) (deref v fr info fuel))]
         [else (loop (frame-parent fr))]))]))

;; Resolve an xalias one hop (from the frame it was found on); pass an xdef
;; through unchanged; #f stays #f.
(define (deref v resume info fuel)
  (cond
    [(xalias? v) (lookup-def resume info (xalias-target v) (sub1 fuel))]
    [else v]))

;; ---------------------------------------------------------------------------
;; #ref

;; Records the ref globally at COLLECT time (anchor + captured frame + folded
;; key, for backlinks) and returns a delayed link resolved in pass 2 by
;; walking the captured frame chain — order-independent, so a ref may precede
;; its def. An unknown id is a dangling reference: an error at resolve time
;; (dangling refs are errors, never empty text). See "#ref spellings" above for
;; the id/custom-text forms.
(define (ref #:to [to #f] . args)
  (define-values (id custom-bodies) (ref-id+custom to args))
  (define origkey (xref-key 'ref id))
  (define fkey (fold origkey))
  (define fr (current-frame))
  (define i (current-info))
  (define anchor (if i (allocate-ref-anchor i) (symbol->string (gensym 'ref-))))
  (define ref-scope-class (nearest-ref-class fr))
  (when i (info-add! i 'xref-all-refs (vector anchor fr fkey)))
  (define custom (and (pair? custom-bodies) custom-bodies))
  (delayed
   (lambda (info)
     (define d (lookup-def fr info fkey))
     (unless d
       (error 'ref "dangling cross-reference #ref(~a): no matching #def" origkey))
     (elem 'a
           #:attrs (list (cons 'class (ref-class-string (xdef-ref-class d) ref-scope-class))
                         (cons 'id anchor)
                         (cons 'href (string-append "#" (xdef-anchor d))))
           (or custom (xdef-term d))))))

;; class = "xref-ref <def-scope> <ref-scope>", empties dropped, deduped (a ref
;; written in the same scope its def lives in would otherwise repeat a token).
(define (ref-class-string def-class ref-scope-class)
  (string-join (remove-duplicates (filter values (list "xref-ref" def-class ref-scope-class)))
               " "))

;; Splits #ref's arguments into (values id custom-text-bodies). The brace body
;; is the id ONLY when no explicit id (`to:` or a positional string/symbol) is
;; given; otherwise it is custom link text.
(define (ref-id+custom to args)
  (cond
    [to (values to args)]
    [(null? args)
     (error 'ref "#ref needs an id: #ref(\"x\"), #ref{x}, or #ref(to: \"x\"){text}")]
    [(or (string? (car args)) (symbol? (car args)))
     (values (car args) (cdr args))]
    [else
     (unless (null? (cdr args))
       (error 'ref "#ref{id} takes a single plain-text id body; for custom link text use #ref(to: \"id\"){text}"))
     (values (body->id (car args)) '())]))

;; A brace body (a Content list) used as an id must flatten to a single plain
;; string — nested elements (e.g. #ref{#emph{x}}) are an error, never silently
;; stringified.
(define (body->id body)
  (define s (content->plain body))
  (unless s
    (error 'ref "#ref{...} id must be plain text, got ~e" body))
  (when (string=? s "")
    (error 'ref "#ref{} needs a non-empty id"))
  s)

(define (content->plain c)
  (cond
    [(string? c) c]
    [(list? c)
     (let ([parts (map content->plain c)])
       (and (andmap values parts) (apply string-append parts)))]
    [else #f]))

(define (allocate-ref-anchor i)
  (define n (add1 (info-ref i 'yamd-xref-ref-counter 0)))
  (info-set! i 'yamd-xref-ref-counter n)
  (string-append "ref-" (number->string n)))

;; ---------------------------------------------------------------------------
;; #doc-ref — a link to another DOCUMENT of the set, by its path from the
;; doc-ref root (the builder's --root, or the root document's directory).
;;
;; Delayed: at resolve it reads the builder-installed index
;; ('yamd-docref-index: path -> (anchor . title)) and emits a `#anchor` link to
;; the target document's title heading. The multi-file strategy's cross-page
;; rewriter turns that into `page.ext#anchor`; single-file leaves it an in-page
;; anchor. An optional body overrides the link text (default: the target's
;; title). A path with no matching document — including ANY #doc-ref rendered
;; outside a document-set build, where no index exists — is a dangling
;; reference: an error at resolve, never silent (like a dangling #ref).
(define (doc-ref path . text-bodies)
  (unless (string? path)
    (error 'doc-ref "document path must be a string, got ~e" path))
  (define key (docref-normalize path))
  (define custom (and (pair? text-bodies) text-bodies))
  (delayed
   (lambda (info)
     (define idx (info-ref info 'yamd-docref-index #f))
     (define entry (and idx (hash-ref idx key #f)))
     (unless entry
       (error 'doc-ref
              "dangling document reference #doc-ref(~s): no such document~a"
              path
              (if (and idx (positive? (hash-count idx)))
                  (format " in the set; known: ~a"
                          (string-join (sort (hash-keys idx) string<?) ", "))
                  " (is this a #doc-ref outside a document-set build?)")))
     ;; default link text = the target's title, resolved and flattened for link
     ;; context (drop nested <a>/ids), exactly like #toc's section-title links.
     (elem 'a #:attrs (list (cons 'href (string-append "#" (car entry))))
           (or custom (flatten-for-link (resolve-content (cdr entry) info)))))))

;; Normalize a #doc-ref path to the builder's index-key form: drop a leading
;; "./" and a trailing ".yamd", backslashes to forward slashes. Kept in sync
;; with build.rkt's docref-normalize.
(define (docref-normalize s)
  (regexp-replace* #rx"\\\\"
                   (regexp-replace #rx"\\.yamd$"
                                   (regexp-replace #rx"^\\./" s "") "")
                   "/"))

;; ---------------------------------------------------------------------------
;; #glossary — delayed. Default: every ROOT def, sorted by id, each with its
;; backlinks. `#glossary(scope: "name")` lists a named #xref-scope's defs
;; instead. Backlinks come from the collected refs, matched to a def by
;; resolving each ref (so a scoped ref counts under its own def, not a
;; same-named def in another scope).

(define (glossary #:scope [scope-name #f])
  (define captured (current-frame))
  (delayed
   (lambda (info)
     (cond
       [scope-name (glossary-for-scope info captured scope-name)]
       [else (glossary-root info)]))))

(define (glossary-root info)
  (define ids (sort (reverse (info-ref info 'xref-def-ids '())) string<?))
  (apply elem 'dl #:attrs '((class . "glossary"))
         (append-map (lambda (fkey)
                       (define d (info-ref info (def-key fkey) #f))
                       (if (xdef? d) (glossary-entry info d) '()))
                     ids)))

(define (glossary-for-scope info captured scope-name)
  (define frame (named-scope-frame captured scope-name))
  (unless frame
    (error 'glossary "#glossary(scope: ~s): no #xref-scope with that name" scope-name))
  (define ids (sort (reverse (frame-ref frame 'xref-def-order '())) string<?))
  (define defs (frame-ref frame 'xref-defs #f))
  (apply elem 'dl #:attrs '((class . "glossary"))
         (append-map (lambda (fkey)
                       (define d (and defs (hash-ref defs fkey #f)))
                       (if (xdef? d) (glossary-entry info d) '()))
                     ids)))

(define (glossary-entry info d)
  (define backlinks (backlinks-for info d))
  (list
   (elem 'dt
         (elem 'a #:attrs (list (cons 'href (string-append "#" (xdef-anchor d))))
               (xdef-term d)))
   (apply elem 'dd (append (xdef-def d) (backrefs backlinks)))))

;; Every ref (document order) whose resolution lands on THIS def, by identity.
(define (backlinks-for info d)
  (for/list ([e (in-list (reverse (info-ref info 'xref-all-refs '())))]
             #:when (eq? (lookup-def (vector-ref e 1) info (vector-ref e 2)) d))
    (vector-ref e 0)))

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
;; Lazy body-reader registration: #xref-scope takes a markup body under a fork.

(module* yamd-reader #f
  (require "../reader/registry.rkt")
  (provide body-readers)
  (define body-readers
    (hasheq 'xref-scope (body-reader 'markup #t))))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; Run a thunk under a fresh root context so per-test defs do not collide in
  ;; the process-global default frame.
  (define-syntax-rule (with-doc i body ...)
    (let ([i (make-info)])
      (parameterize ([current-info i])
        (call-with-fresh-context (lambda () body ...)))))

  ;; def emits a <dfn> anchor around the term.
  (check-equal? (call-with-fresh-context
                 (lambda () (def "vm" (list "virtual machine"))))
                (elem 'dfn #:attrs '((id . "def-vm")) (list "virtual machine")))

  ;; non-string/symbol id is rejected.
  (check-exn #rx"must be a symbol or string" (lambda () (def 42 (list "t"))))

  ;; ref returns a delayed node and records a backlink at collect time; every
  ;; ref carries the xref-ref class.
  (with-doc i
    (define d1 (def "vm" (list "virtual machine")))
    (define r (ref "vm"))
    (check-pred delayed? r)
    ;; resolves to a link to the def site with the term as text
    (check-equal? ((delayed-proc r) i)
                  (elem 'a #:attrs '((class . "xref-ref") (id . "ref-1") (href . "#def-vm"))
                        (list "virtual machine"))))

  ;; case-insensitive: #ref("VM") resolves #def("vm"); def casing wins in text.
  (with-doc i
    (def "vm" (list "virtual machine"))
    (define r (ref "VM"))
    (check-equal? ((delayed-proc r) i)
                  (elem 'a #:attrs '((class . "xref-ref") (id . "ref-1") (href . "#def-vm"))
                        (list "virtual machine"))))

  ;; def casing is authoritative for the anchor: #def("VM") -> def-VM.
  (with-doc i
    (def "VM" (list "virtual machine"))
    (check-equal? ((delayed-proc (ref "vm")) i)
                  (elem 'a #:attrs '((class . "xref-ref") (id . "ref-1") (href . "#def-VM"))
                        (list "virtual machine"))))

  ;; #ref{vm} brace-body id.
  (with-doc i
    (def "vm" (list "virtual machine"))
    (check-equal? ((delayed-proc (ref (list "vm"))) i)
                  (elem 'a #:attrs '((class . "xref-ref") (id . "ref-1") (href . "#def-vm"))
                        (list "virtual machine"))))

  ;; #ref(to: "vm"){custom} keyword id + custom text.
  (with-doc i
    (def "vm" (list "virtual machine"))
    ;; custom text bodies nest one level under the anchor (spliced flat in
    ;; output), exactly like a positional #ref("vm"){text}.
    (check-equal? ((delayed-proc (ref #:to "vm" (list "the machine"))) i)
                  (elem 'a #:attrs '((class . "xref-ref") (id . "ref-1") (href . "#def-vm"))
                        (list (list "the machine")))))

  ;; a non-plain brace-body id is an error.
  (check-exn #rx"id must be plain text"
             (lambda () (call-with-fresh-context
                         (lambda () (ref (list (elem 'em "x")))))))

  ;; an empty brace-body id is an error, not a silent dangling "".
  (check-exn #rx"needs a non-empty id"
             (lambda () (call-with-fresh-context (lambda () (ref (list))))))

  ;; anchor collision: a root #def whose id equals a scope's "<scope-id>-<key>"
  ;; must NOT share the scoped def's anchor.
  (with-doc i
    (def "1-foo" (list "ROOT one-foo"))          ; anchor def-1-foo
    (define scoped-anchor #f)
    (xref-scope                                   ; scope-id 1
     (lambda ()
       (set! scoped-anchor
             (cdr (assq 'id (element-attrs (def "foo" (list "SCOPED foo"))))))
       '()))
    ;; the scoped def wanted def-1-foo too; it was bumped to stay unique
    (check-equal? (element-attrs (def "root2" (list "x"))) '((id . "def-root2")))
    (check-not-equal? scoped-anchor "def-1-foo"))

  ;; #defalias: an alias resolves to its target.
  (with-doc i
    (def "vm" (list "virtual machine"))
    (defalias "vm" "vms" "v-m")
    (check-equal? ((delayed-proc (ref "vms")) i)
                  (elem 'a #:attrs '((class . "xref-ref") (id . "ref-1") (href . "#def-vm"))
                        (list "virtual machine"))))

  ;; dangling ref: resolve-time error naming the id.
  (with-doc i
    (define r (ref "missing"))
    (check-exn #rx"dangling cross-reference"
               (lambda () ((delayed-proc r) i))))

  ;; duplicate def in one scope is an error.
  (with-doc i
    (def "dup" (list "a"))
    (check-exn #rx"duplicate #def" (lambda () (def "dup" (list "b")))))

  ;; ---- scoping ----------------------------------------------------------
  ;; #xref-scope: an inner def shadows an outer one; a ref inside resolves the
  ;; inner def, a ref outside the outer.
  (with-doc i
    (def "x" (list "OUTER"))
    (define inner-ref #f)
    (xref-scope
     (lambda ()
       (def "x" (list "INNER"))
       (set! inner-ref (ref "x"))
       '()))
    (define outer-ref (ref "x"))
    (check-equal? (element-children ((delayed-proc inner-ref) i)) (list (list "INNER")))
    (check-equal? (element-children ((delayed-proc outer-ref) i)) (list (list "OUTER"))))

  ;; #def(#:at-root? #t) inside a scope registers at ROOT: a global vocabulary
  ;; (rfc2119 keywords, a house-style #req) must not be stranded in whichever
  ;; scope happened to use it first — an outer ref still resolves it.
  (with-doc i
    (define outer #f)
    (xref-scope (lambda () (def "kw" (list "KW") #:at-root? #t) '()))
    (set! outer (ref "kw"))
    (check-equal? (element-children ((delayed-proc outer) i)) (list (list "KW")))
    ;; and it lands in the ROOT glossary, not the scope's
    (check-equal? (info-ref i 'xref-def-ids '()) (list "kw")))

  ;; a def inside a scope is invisible to an outer sibling ref (falls through
  ;; to root; dangling if root has none).
  (with-doc i
    (define outside #f)
    (xref-scope (lambda () (def "only-inner" (list "I")) '()))
    (set! outside (ref "only-inner"))
    (check-exn #rx"dangling cross-reference" (lambda () ((delayed-proc outside) i))))

  ;; ref-before-def across a scope: a ref may precede its (later) def.
  (with-doc i
    (define r (ref "late"))
    (def "late" (list "defined after"))
    (check-equal? (element-children ((delayed-proc r) i)) (list (list "defined after"))))

  ;; ref-class: a scope's ref-class rides on refs to its defs (def-scope) and
  ;; on refs written inside it (ref-scope), deduped.
  (with-doc i
    (define r #f)
    (xref-scope #:name "api" #:ref-class "api"
     (lambda ()
       (def "call" (list "the call"))
       (set! r (ref "call"))
       '()))
    (check-equal? (assq 'class (element-attrs ((delayed-proc r) i)))
                  '(class . "xref-ref api")))

  ;; #glossary(scope:) lists a named scope's defs.
  (with-doc i
    (def "root-term" (list "R"))
    (xref-scope #:name "g"
     (lambda () (def "scoped-term" (list "S")) '()))
    (define gl ((delayed-proc (glossary #:scope "g")) i))
    (check-equal? (element-tag gl) 'dl)
    ;; the scoped glossary's only <dt> term is "S"
    (define terms
      (let walk ([c gl])
        (cond [(pair? c) (append-map walk c)]
              [(and (element? c) (eq? (element-tag c) 'dt))
               (list (element-children c))]
              [(element? c) (append-map walk (element-children c))]
              [else '()])))
    (check-equal? (length terms) 1))

  ;; doc-ref: delayed link to another document; resolves via the builder-
  ;; installed index to a `#anchor` link, default text = the target's title.
  (let ([i (make-info)])
    (info-set! i 'yamd-docref-index
               (hash "howto/pipe-tables" (cons "sec-42" (list "Pipe tables"))))
    (define d (doc-ref "howto/pipe-tables"))
    (check-pred delayed? d)
    (check-equal? ((delayed-proc d) i)
                  (elem 'a #:attrs '((href . "#sec-42")) (list "Pipe tables")))
    ;; the ".yamd" extension and a leading "./" are normalized away
    (check-equal? ((delayed-proc (doc-ref "./howto/pipe-tables.yamd")) i)
                  (elem 'a #:attrs '((href . "#sec-42")) (list "Pipe tables")))
    ;; a body overrides the link text (bodies are spliced like #ref's custom text)
    (check-equal? ((delayed-proc (doc-ref "howto/pipe-tables" "here")) i)
                  (elem 'a #:attrs '((href . "#sec-42")) (list "here"))))

  ;; dangling doc-ref: unknown path errors at resolve, listing known ids.
  (let ([i (make-info)])
    (info-set! i 'yamd-docref-index (hash "a/b" (cons "sec-1" (list "B"))))
    (check-exn #rx"dangling document reference.*known: a/b"
               (lambda () ((delayed-proc (doc-ref "nope")) i))))

  ;; no index at all (rendered outside a doc-set build): dangling, with a hint.
  (let ([i (make-info)])
    (check-exn #rx"outside a document-set build"
               (lambda () ((delayed-proc (doc-ref "x")) i))))

  ;; non-string path is rejected.
  (check-exn #rx"document path must be a string" (lambda () (doc-ref 42)))

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
