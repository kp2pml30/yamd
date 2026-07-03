#lang racket/base
;; Document-set primitives (multi-source documents, docs/multi-doc.md).
;; Mode-agnostic core: a registry of the documents discovered while
;; evaluating a root file, KEYED BY the shared info instance (see "Registry"
;; below), plus the dynamic state needed to evaluate one file inside another
;; (the cycle-detection stack; the source path itself lives in runtime.rkt
;; as `current-yamd-source`).
;;
;; No policy lives here: this module does not know what a toctree renders to,
;; what an output mode is, or how children are discovered — plugins
;; (std/include, std/toctree) write the registry, build strategies
;; (yamd/build.rkt) read it. It is attached to document namespaces by
;; make-yamd-namespace so plugin instantiations share the stack parameter and
;; struct identities with the builder.

(require racket/path ; incl. normalize-path (symlink resolution)
         (only-in racket/list take drop)
         "ir.rkt"
         "info-table.rkt")

(provide (struct-out doc-record)
         (struct-out toctree-rec)
         current-docset-stack
         current-docset-parent
         doc-identity
         resolve-doc-path
         check-doc-cycle
         docset-section-count
         docset-doc-title
         docset-register-doc!
         docset-doc-ref
         docset-doc-ids
         docset-note-child!
         docset-children
         docset-set-numbering!
         docset-numbering-level
         docset-numbering-group
         docset-register-toctree!
         docset-toctree-ref
         toctree-marker-tag
         make-toctree-marker
         toctree-marker-rec)

;; A document of the set. `id` is the complete simplified source path (also
;; the key everywhere); `content` is UNRESOLVED Content (delayed nodes intact
;; — strategies resolve per output page); `title` is Content for links to the
;; page; `secs-start`/`secs-end` delimit the entries this document's
;; evaluation appended to the 'sections info list (oldest-position indices,
;; end exclusive; grandchildren's entries nest inside — innermost range owns
;; an entry); `base-depth` is the 'header-depth at the #toctree that listed
;; it (0 for the root); `children` are doc ids in toctree order.
(struct doc-record (id path content title secs-start secs-end base-depth children)
  #:transparent)

;; One #toctree call site: `children` doc ids in order, `depth` the depth:
;; option (#f = unlimited), `base-depth` the 'header-depth at the call site,
;; `numbered` the numbered: flag (sphinx-style hierarchical section numbers on
;; the subtree) and `hidden` the hidden: flag (register children but emit no
;; nav at the call site — multi-file only). The emitted %toctree marker element
;; carries the record's number in its `tt` attribute.
(struct toctree-rec (children depth base-depth numbered hidden) #:transparent)

;; ---------------------------------------------------------------------------
;; Embedded-evaluation dynamic state

;; The chain of files currently being evaluated, innermost first (the root is
;; pushed by eval.rkt's file entry points). Guards against include/toctree
;; cycles.
(define current-docset-stack (make-parameter '()))

;; The id of the innermost enclosing PAGE (a document that is or will be
;; registered: the root, or a toctree child) — NOT simply the current file:
;; an #include does not rebind it, so a #toctree inside an included file
;; attaches its children to the page the fragment is spliced into, keeping
;; every child reachable from the root in the doc graph. Bound by eval.rkt
;; (root entry points; yamd-eval-embedded with #:reset-header-depth?, i.e.
;; toctree children). #f outside a file build (string-fed documents).
(define current-docset-parent (make-parameter #f))

;; A document's IDENTITY — the key for the registry, the cycle stack and the
;; one-toctree-per-doc check — is the fully symlink-resolved simple path.
;; simple-form-path alone is not enough: with a directory alias like
;; `sub -> .`, `child.yamd` and `sub/child.yamd` would be two identities for
;; one file (silent duplicate pages), and a self-include through the alias
;; would GROW a fresh spelling per level (never matching the stack) until
;; the kernel's symlink limit turns it into a misleading missing-file error.
;; Nonexistent paths fall back to simple form (error paths only).
(define (doc-identity p)
  (define abs (path->complete-path p))
  (if (file-exists? abs)
      (normalize-path abs)
      (simple-form-path abs)))

;; Resolves a document path written in a source file: relative to `base` (the
;; naming file, i.e. current-yamd-source; #f = string-fed document, resolve
;; against current-directory). Returns the document identity. Missing file is
;; an explicit error naming both the written and the resolved spelling (the
;; latter deliberately BEFORE symlink resolution — a path the author can
;; recognize).
(define (resolve-doc-path who spec base)
  (unless (path-string? spec)
    (error who "document path must be a non-empty string, got ~e" spec))
  (define dir
    (cond
      [(not base) (current-directory)]
      [else
       (define-values (d _name _must?) (split-path (path->complete-path base)))
       (if (path? d) d (current-directory))]))
  (define abs (simple-form-path (path->complete-path spec dir)))
  (unless (file-exists? abs)
    (error who "no such document: ~s (resolved to ~a)" spec abs))
  (doc-identity abs))

;; Explicit cycle error listing the whole chain, innermost last.
(define (check-doc-cycle who abs)
  (define stack (current-docset-stack))
  (when (member abs stack)
    (error who "document cycle: ~a"
           (let loop ([chain (reverse (cons abs stack))])
             (if (null? (cdr chain))
                 (format "~a" (car chain))
                 (format "~a -> ~a" (car chain) (loop (cdr chain))))))))

;; ---------------------------------------------------------------------------
;; Registry, keyed by the shared info instance.
;;
;; NOT stored inside the info table itself, deliberately: passes.rkt is
;; typed, so values round-tripping through its (Mutable-HashTable Any Any)
;; get `Any`-contract-wrapped — procedures (the delayed procs inside a
;; doc-record's Content) come back opaque and error when resolve-content
;; applies them. Instead the registry is module state of this (always
;; attached, hence single-instance) module: a weak table info -> hash, same
;; sharing as the info table, no typed boundary. GC of an info collects its
;; registry.

(define registries (make-weak-hasheq))

(define (registry-of i)
  (hash-ref! registries i make-hash))

(define (doc-key id) (list 'doc id))
(define (children-key id) (list 'children id))
(define (numbering-key id) (list 'numbering id))
(define (toctree-key n) (list 'toctree n))

(define (reg-ref i key default) (hash-ref (registry-of i) key default))
(define (reg-set! i key val) (hash-set! (registry-of i) key val))
(define (reg-add! i key val)
  (hash-update! (registry-of i) key (lambda (old) (cons val old)) '()))

;; A document may be registered once: it appears in exactly one toctree (or
;; is the root). Sequential #include of the same file is unaffected —
;; includes never register.
(define (docset-register-doc! i rec)
  (define id (doc-record-id rec))
  (when (docset-doc-ref i id)
    (error 'toctree "document ~a is already registered in the document set" id))
  (reg-set! i (doc-key id) rec)
  (reg-add! i 'doc-ids id))

(define (docset-doc-ref i id)
  (reg-ref i (doc-key id) #f))

;; Registration order (root last — children register during the root's eval).
(define (docset-doc-ids i)
  (reverse (reg-ref i 'doc-ids '())))

;; Called by #toctree while page `parent-id` (current-docset-parent; #f for a
;; string-fed root) is being evaluated; register-doc!/the builder read the
;; accumulated list back once that evaluation finishes.
(define (docset-note-child! i parent-id child-id)
  (reg-add! i (children-key parent-id) child-id))

(define (docset-children i parent-id)
  (reverse (reg-ref i (children-key parent-id) '())))

;; Section numbering (`#toctree(numbered: …)`). A numbered
;; toctree marks every document of its subtree with a NUMBERING LEVEL — its
;; direct children are level 1, their toctree children level 2, and so on —
;; and a GROUP (the marking toctree's record number): documents of one group
;; share one counter, so each numbered toctree numbers its subtree
;; independently (two sibling numbered toctrees both restart at "1", sphinx
;; parity). Mode-independent bookkeeping; build strategies turn (level +
;; heading depth) into the dotted "1.2.3" labels. Marking happens after the
;; children are evaluated (so the whole subtree is registered) and an outer
;; numbered toctree OVERRIDES an inner one, defining scheme and group for its
;; whole subtree (sphinx `:numbered:` propagates down).
(define (docset-set-numbering! i id level group)
  (reg-set! i (numbering-key id) (cons level group)))

;; The numbering level / group of a document, #f when it is not under a
;; numbered toctree.
(define (docset-numbering-level i id)
  (define v (reg-ref i (numbering-key id) #f))
  (and v (car v)))

(define (docset-numbering-group i id)
  (define v (reg-ref i (numbering-key id) #f))
  (and v (cdr v)))

;; Returns the record's number, to be stored in the %toctree marker's `tt`
;; attribute.
(define (docset-register-toctree! i rec)
  (define n (add1 (reg-ref i 'toctree-counter 0)))
  (reg-set! i 'toctree-counter n)
  (reg-set! i (toctree-key n) rec)
  n)

(define (docset-toctree-ref i n)
  (define rec (reg-ref i (toctree-key n) #f))
  (unless rec
    (error 'docset "unknown toctree record ~a (marker without registration?)" n))
  rec)

;; ---------------------------------------------------------------------------
;; Title discovery. Soft coupling, documented: this reads the 'sections info
;; key — the std/base contract (entries (list depth anchor title), newest-
;; first) — without requiring any std module. A document's title is the first
;; section heading it records ITSELF: entries produced by its own toctree
;; children are excluded (their oldest-position ranges nest inside this
;; document's). Falls back to the source file's stem.

(define (docset-section-count i)
  (length (info-ref i 'sections '())))

(define (docset-doc-title i secs-start secs-end children fallback-path)
  (define secs (info-ref i 'sections '()))
  (define total (length secs))
  ;; oldest-first entries occupying oldest positions [secs-start, secs-end)
  (define slice
    (reverse (take (drop secs (- total secs-end)) (- secs-end secs-start))))
  (define child-ranges
    (for/list ([cid (in-list children)])
      (define rec (docset-doc-ref i cid))
      (cons (doc-record-secs-start rec) (doc-record-secs-end rec))))
  (define own
    (for/first ([entry (in-list slice)]
                [pos (in-naturals secs-start)]
                #:unless (for/or ([r (in-list child-ranges)])
                           (and (>= pos (car r)) (< pos (cdr r)))))
      entry))
  (if own
      (caddr own) ; the entry's title Content
      (path->string (path-replace-extension (file-name-from-path fallback-path) #""))))

;; ---------------------------------------------------------------------------
;; The %toctree marker — the plugin <-> strategy contract, owned here so
;; neither side requires the other. std/toctree EMITS the marker; a build
;; strategy REPLACES it (mode-dependent) after looking up the record. The
;; `%` tag guarantees an unbuilt toctree document fails loudly in a plain
;; single-document render instead of half-working.

(define toctree-marker-tag '%toctree)

;; `loc` (optional): best effort — functions get no call srcloc, but at least
;; the source FILE makes the deliberate "%toctree leaked into backend" error
;; actionable when a toctree document is rendered without the builder.
(define (make-toctree-marker n [loc #f])
  (element toctree-marker-tag (list (cons 'tt (number->string n))) '() loc))

;; marker element -> its toctree-rec.
(define (toctree-marker-rec i e)
  (define a (assq 'tt (element-attrs e)))
  (unless a
    (error 'docset "malformed %toctree marker: no tt attribute (at ~a)"
           (element-loc e)))
  (docset-toctree-ref i (string->number (cdr a))))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; resolve: relative against the naming file's directory; missing errors.
  (check-exn #rx"no such document"
             (lambda () (resolve-doc-path 'include "nope-does-not-exist.yamd" #f)))
  (check-exn #rx"path must be a non-empty string"
             (lambda () (resolve-doc-path 'include 42 #f)))

  ;; cycle check names the chain.
  (parameterize ([current-docset-stack (list (string->path "/a") (string->path "/b"))])
    (check-exn #rx"document cycle: /b -> /a -> /b"
               (lambda () (check-doc-cycle 'include (string->path "/b"))))
    (check-not-exn (lambda () (check-doc-cycle 'include (string->path "/c")))))

  ;; registry round-trips; duplicate registration is an error.
  (let ([i (make-info)])
    (define rec (doc-record "id1" "id1" '() "T" 0 0 0 '()))
    (docset-register-doc! i rec)
    ;; eq?: the registry never crosses the typed info-table boundary (see
    ;; "Registry" above), so values come back unwrapped.
    (check-eq? (docset-doc-ref i "id1") rec)
    (check-equal? (docset-doc-ids i) '("id1"))
    (check-exn #rx"already registered in the document set"
               (lambda () (docset-register-doc! i rec)))
    (docset-note-child! i "id1" "c1")
    (docset-note-child! i "id1" "c2")
    (check-equal? (docset-children i "id1") '("c1" "c2"))
    ;; numbering level/group round-trip; #f before any numbered toctree marks it.
    (check-false (docset-numbering-level i "c1"))
    (check-false (docset-numbering-group i "c1"))
    (docset-set-numbering! i "c1" 2 7)
    (check-equal? (docset-numbering-level i "c1") 2)
    (check-equal? (docset-numbering-group i "c1") 7)
    (define n (docset-register-toctree! i (toctree-rec '("c1") #f 0 #f #f)))
    (check-equal? n 1)
    (check-equal? (toctree-rec-children (docset-toctree-ref i n)) '("c1"))
    (check-exn #rx"unknown toctree record"
               (lambda () (docset-toctree-ref i 99)))))
