#lang racket/base
;; #toctree — the sphinx-style document-set "manifest in the language"
;; (docs/multi-doc.md "toctree"). A plugin: #use(std/toctree), then
;;
;;   #toctree(depth: 2)
;;   	intro.yamd
;;   	guide/setup.yamd
;;
;; Raw body, one child path per non-blank line (trimmed), relative to the
;; current file. At collect time — identically for every output mode — each
;; child is evaluated (fresh namespace, shared info, forked context with
;; 'header-depth reset to 0: a toctree child is a standalone document whose
;; top sections are h1) and registered as a `doc-record` in the shared info
;; table; the call emits a `%toctree` marker element that a build strategy
;; later replaces (single-file: inlined children with shifted headings;
;; multi-file: a nav of links to the child pages). Rendering a toctree
;; document without the builder errors on the leaked %-tag, on purpose.
;;
;; A document may appear in exactly one toctree; cycles and missing files
;; are explicit errors. `depth:` (positive integer) limits how deep the
;; multi-file nav recurses into nested toctrees; it is ignored when inlining.

(require racket/string
         yamd/docset
         yamd/eval
         "../runtime.rkt")

(provide toctree)

;; `body` is optional with an empty default: for `#toctree` over an EMPTY
;; (or whitespace-only) indented block the reader passes no body argument at
;; all, and the "empty toctree" error below beats a raw arity mismatch.
;;
;; `numbered:` gives the child documents' headings sphinx-style hierarchical
;; numbers ("1", "1.1", "2") on their headings and in the nav/#toc entries;
;; identical in both build modes. `hidden:` registers the children (they build,
;; refs work, they are navigable pages) but emits no nav at the call site —
;; a multi-file-only effect (single-file inlines the content regardless, since
;; dropping it would lose the documents), mirroring how `depth:` is ignored
;; when inlining.
(define (toctree #:depth [depth #f] #:numbered [numbered #f] #:hidden [hidden #f] [body ""])
  (unless (string? body)
    (error 'toctree "body must be raw text (one child path per line), got ~e" body))
  (unless (or (not depth) (exact-positive-integer? depth))
    (error 'toctree "depth: must be a positive integer, got ~e" depth))
  (unless (boolean? numbered)
    (error 'toctree "numbered: must be a boolean, got ~e" numbered))
  (unless (boolean? hidden)
    (error 'toctree "hidden: must be a boolean, got ~e" hidden))
  (define i (current-info))
  (unless i
    (error 'toctree "no current info table; #toctree only works inside a document build"))
  (define specs (body-lines body))
  (when (null? specs)
    (error 'toctree "empty toctree: the body must list at least one child document"))
  ;; The enclosing PAGE, not the current file: a toctree inside an #include'd
  ;; fragment attaches its children to the page the fragment lands in.
  (define parent-id (current-docset-parent))
  (define base-depth (context-ref 'header-depth 0))
  (define child-ids
    (for/list ([spec (in-list specs)])
      (evaluate-child! i parent-id spec base-depth)))
  (define rec-n
    (docset-register-toctree! i (toctree-rec child-ids depth base-depth
                                             numbered hidden)))
  ;; Mark the whole subtree numbered, with this toctree's record number as the
  ;; numbering GROUP (one counter per group: sibling numbered toctrees restart
  ;; independently). Done after the children evaluated (so grandchildren,
  ;; registered during that eval, exist); setting unconditionally lets an
  ;; OUTER numbered toctree override an inner one, defining scheme and group
  ;; for its whole subtree (sphinx parity).
  (when numbered
    (let mark ([ids child-ids] [level 1])
      (for ([cid (in-list ids)])
        (docset-set-numbering! i cid level rec-n)
        (mark (docset-children i cid) (add1 level)))))
  (make-toctree-marker
   rec-n
   ;; No call srcloc reaches a function; the file (when known) still makes
   ;; the deliberate "%toctree leaked" error actionable.
   (let ([src (current-yamd-source)])
     (and src (srcloc src #f #f #f #f)))))

;; One path per non-blank line, whitespace-trimmed. Dumb on purpose: no
;; comments, no globs, no options — a line is a path.
(define (body-lines body)
  (for/list ([line (in-list (string-split body "\n"))]
             #:unless (regexp-match? #px"^\\s*$" line))
    (string-trim line)))

;; Evaluates one child document and registers its doc-record. Returns the
;; child id. The pre-check duplicates docset-register-doc!'s guard so a
;; doubly-listed document fails BEFORE being expensively re-evaluated (and
;; before its duplicate #defs would error confusingly).
(define (evaluate-child! i parent-id spec base-depth)
  (define id (resolve-doc-path 'toctree spec (current-yamd-source)))
  (when (docset-doc-ref i id)
    ;; covers both a repeat within ONE toctree and a second toctree; name
    ;; the file doing the re-listing (the first lister is long finished).
    (error 'toctree
           "document ~a is already in the document set; ~a lists it again (a document may appear in exactly one toctree, once)"
           id (or (current-yamd-source) "a string-fed document")))
  (define secs-start (docset-section-count i))
  (define content
    (yamd-eval-embedded id #:who 'toctree #:reset-header-depth? #t))
  (define secs-end (docset-section-count i))
  (define children (docset-children i id))
  (docset-register-doc!
   i
   (doc-record id id content
               (docset-doc-title i secs-start secs-end children id)
               secs-start secs-end base-depth children))
  (docset-note-child! i parent-id id)
  id)

;; ---------------------------------------------------------------------------
;; Body-reader registration: raw body (paths are text, not markup).

(module* yamd-reader #f
  (require "../reader/registry.rkt")
  (provide body-readers)
  (define body-readers
    (hasheq 'toctree (body-reader 'raw #f))))
