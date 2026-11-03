#lang racket/base
;; Doc-set builder (docs/multi-doc.md "Builder interface"). Owns everything
;; mode-dependent: how the mode-agnostic evaluated docset (per-doc unresolved
;; Content + one shared info table) becomes named outputs. A build strategy
;; is ONE function — implement another `build-strategy` to swap the whole
;; assembly policy; nothing in core, plugins or backends changes.
;;
;;   (evaluate-docset root)                            -> docset
;;   (build-docset root #:strategy s #:backend b)      -> (listof build-output)
;;
;; Strategies shipped here:
;;   single-file-strategy       one page: toctree children inlined (headings
;;                              shifted by the toctree's section depth), all
;;                              anchors local, hrefs untouched.
;;   multi-file-strategy        one page per document: %toctree markers become
;;                              navs, cross-page `#anchor` hrefs are rewritten
;;                              to `relative/page.ext#anchor` (backend-agnostic
;;                              IR pass — no backend involvement), prev/next/up
;;                              navigation via the overridable #:wrap-page hook
;;                              of make-multi-file-strategy.

(require racket/list
         racket/path
         racket/string
         "ir.rkt"
         "info-table.rkt"
         "passes.rkt"
         "decode.rkt"
         "docset.rkt"
         "eval.rkt"
         (only-in "runtime.rkt" current-docref-root)
         "build/rewrite.rkt")

(provide (struct-out build-output)
         (struct-out build-backend)
         (struct-out build-strategy)
         (struct-out docset)
         (struct-out page-view)
         evaluate-docset
         build-docset
         current-docref-root
         single-file-strategy
         multi-file-strategy
         make-multi-file-strategy)

;; path : string, relative to the output root (subdirectories preserved).
;; text : the serialized page, no trailing newline (writers add one).
(struct build-output (path text) #:transparent)

;; What a strategy needs to know about a backend: its raw-node name, the
;; output file extension (with dot), and Content -> String serialization.
(struct build-backend (name ext render) #:transparent)

;; build : docset build-backend -> (listof build-output)
(struct build-strategy (name build) #:transparent)

;; The evaluated set: doc-records live in the shared info table (yamd/docset
;; registry); `docset-doc` / `docset-pages` are the reading conveniences.
(struct docset (root-id info) #:transparent)

;; ---------------------------------------------------------------------------
;; Evaluation (mode-independent collect pass over the whole set)

(define (evaluate-docset root-path)
  (unless (file-exists? root-path)
    (error 'evaluate-docset "no such document: ~a" root-path))
  (define abs (doc-identity root-path)) ; same identity rules as include/toctree
  (call-with-document-source
   abs
   (lambda ()
    (define-values (content i) (yamd-eval (yamd-read-file abs)))
    ;; Children registered themselves (via #toctree) during the eval; the
    ;; root's record follows the same shape: full section range, base 0.
    (define secs-end (docset-section-count i))
    (define children (docset-children i abs))
    (docset-register-doc!
     i
     (doc-record abs abs content
                 (docset-doc-title i 0 secs-end children abs)
                 0 secs-end 0 children))
    (docset abs i))))

(define (build-docset root-path #:strategy strategy #:backend backend
                      #:root-dir [root-dir #f])
  (parameterize ([current-docref-root root-dir])
    ((build-strategy-build strategy) (evaluate-docset root-path) backend)))

;; #doc-ref index installed in the info table during resolution: author path
;; string -> (cons anchor raw-title), for every page reachable from the root
;; that has a title heading of its own. `root-dir` (the #doc-ref base) sets the
;; path namespace. std/xref's #doc-ref (a delayed node) reads it under this key
;; and emits a `#anchor` link the cross-page rewriter then routes per mode.
;; Saved/restored around resolution like the decorated 'sections — no docset
;; mutation. The title is stored RAW (not sanitized here): #doc-ref flattens it
;; for link text on demand, exactly as #toc does, so a two-pass title (a #ref in
;; a heading) on an unreferenced document does not error the whole build.
(define docref-index-key 'yamd-docref-index)

(define (docref-index ds root-dir)
  (define i (docset-info ds))
  (for*/hash ([id (in-list (docset-pages ds))]
              [key (in-value (docref-key root-dir id))]
              #:when key
              [doc (in-value (docset-doc ds id))]
              [anchor (in-value (docset-doc-anchor i (doc-record-secs-start doc)
                                                   (doc-record-secs-end doc)
                                                   (doc-record-children doc)))]
              #:when anchor)
    (values key (cons anchor (doc-record-title doc)))))

;; A document's #doc-ref path id: its source path relative to `root-dir`,
;; extension dropped, forward slashes. #f when the document lies outside
;; root-dir (an `..` path is no usable id) — those documents are unreferenceable.
(define (docref-key root-dir id)
  (define rel (find-relative-path root-dir id))
  (and (relative-path? rel)
       (not (memq 'up (explode-path rel)))
       (docref-normalize (path->string (path-replace-extension rel #"")))))

;; Normalize a #doc-ref path (computed here, or author-written in std/xref) to
;; the index key form: drop a leading "./" and a trailing ".yamd", backslashes
;; to forward slashes. Kept in sync with std/xref's docref-normalize.
(define (docref-normalize s)
  (regexp-replace* #rx"\\\\"
                   (regexp-replace #rx"\\.yamd$"
                                   (regexp-replace #rx"^\\./" s "") "")
                   "/"))

;; Resolve `root-dir` for a build: the --root override or the root doc's dir.
;; The override is normalized the same way document identities are (symlink-
;; resolved when it exists), so find-relative-path against the (already
;; normalized) doc ids yields clean relative keys rather than `..` noise.
(define (docref-root-dir ds)
  (define override (current-docref-root))
  (cond
    [(not override) (path-only (docset-root-id ds))]
    [(directory-exists? override) (normalize-path override)]
    [else (simple-form-path override)]))

(define (docset-doc ds id)
  (or (docset-doc-ref (docset-info ds) id)
      (error 'build "unknown document in set: ~a" id)))

;; Depth-first page order: a page, then its toctree children, recursively.
;; This is reading order — prev/next navigation follows it.
(define (docset-pages ds)
  (let walk ([id (docset-root-id ds)])
    (cons id (append-map walk (doc-record-children (docset-doc ds id))))))

;; ---------------------------------------------------------------------------
;; Single-file strategy: inline toctree children at their markers (sphinx
;; latex-builder style), then resolve/decode/render once. For a set of one
;; document this is step-for-step the plain yamd-file->content pipeline.

(define single-file-strategy
  (build-strategy
   'single-file
   (lambda (ds backend)
     (define i (docset-info ds))
     (define root (docset-doc ds (docset-root-id ds)))
     (define expanded (expand-toctrees i (doc-record-content root)))
     (define nums (section-numbering ds))
     ;; The shifted+numbered 'sections list must be visible DURING resolution
     ;; (a #toc reads it from the info), but a strategy must not mutate the
     ;; shared docset — the same evaluated set may be built again (another
     ;; strategy, or twice) — so the pristine list is restored afterwards.
     (define original (info-ref i 'sections '()))
     (define decorated (decorate-sections ds nums #:cap-h6? #t))
     (define idx (docref-index ds (docref-root-dir ds)))
     (define prior-idx (info-ref i docref-index-key #f))
     (define name (output-file-name (docset-root-id ds) backend))
     (define final
       (dynamic-wind
        (lambda () (when decorated (info-set! i 'sections decorated))
                   (info-set! i docref-index-key idx))
        (lambda () (number-headings (decode-flow (resolve-content expanded i)) nums))
        (lambda () (when decorated (info-set! i 'sections original))
                   (info-set! i docref-index-key prior-idx))))
     ;; One merged page: a hand-authored duplicate id would emit a duplicate
     ;; html/markdown id or typst label (a hard compile error). Catch it here.
     (check-unique-ids! final name)
     (list (build-output name ((build-backend-render backend) final))))))

;; Replaces each %toctree marker with its children's content, headings
;; shifted by the marker's base depth (the 'header-depth at the #toctree
;; site), recursively — nested toctrees accumulate shifts naturally because
;; the inner expansion happens inside the outer shift.
(define (expand-toctrees i content)
  (replace-tagged
   content toctree-marker-tag
   (lambda (marker)
     (define rec (toctree-marker-rec i marker))
     (for/list ([cid (in-list (toctree-rec-children rec))])
       (define c (docset-doc-ref i cid))
       (shift-headings (expand-toctrees i (doc-record-content c))
                       (toctree-rec-base-depth rec)
                       #:who (doc-record-path c))))))

;; PURE counterpart of the heading shift for the 'sections info entries
;; (same contract key as docset-doc-title), so #toc/#ref data agrees with the
;; shifted headings in the inlined output. Returns the new list, or #f when
;; no document needs shifting. Entry ownership: the innermost secs range
;; containing the entry's oldest-position (children's ranges nest inside
;; their parent's).
;;
;; `cap-h6?`: single-file renders these depths as real headings, so a shift
;; past h6 is an error there. Multi-file uses the same shift only to give #toc
;; its doc-graph NESTING (depths are list structure, never heading tags), where
;; an arbitrarily deep graph is harmless — it passes #f.
(define (shifted-sections ds #:cap-h6? [cap? #t])
  (define i (docset-info ds))
  ;; cumulative shift per doc: cum(child) = cum(parent) + base-depth(child)
  (define shifts (make-hash))
  (let walk ([id (docset-root-id ds)] [cum 0])
    (hash-set! shifts id cum)
    (for ([cid (in-list (doc-record-children (docset-doc ds id)))])
      (walk cid (+ cum (doc-record-base-depth (docset-doc ds cid))))))
  (cond
    [(for/and ([(_ s) (in-hash shifts)]) (zero? s)) #f]
    [else
     (define secs (info-ref i 'sections '()))
     (define total (length secs))
     ;; (start end shift path doc-id) per non-root doc, innermost = largest start
     (define ranges
       (sort
        (for/list ([id (in-list (docset-doc-ids i))]
                   #:unless (equal? id (docset-root-id ds)))
          (define r (docset-doc ds id))
          (list (doc-record-secs-start r) (doc-record-secs-end r)
                (hash-ref shifts id 0) (doc-record-path r)))
        > #:key car))
     (for/list ([entry (in-list secs)] [j (in-naturals)])
       (define pos (- total 1 j)) ; oldest-position of this entry
       (define owner
         (findf (lambda (r) (and (>= pos (car r)) (< pos (cadr r)))) ranges))
       (cond
         [(or (not owner) (zero? (caddr owner))) entry]
         [else
          (define depth (+ (car entry) (caddr owner)))
          (when (and cap? (> depth 6))
            (error 'build
                   "section ~e (~s) of ~a shifted past h6 when inlining its toctree"
                   (caddr entry) (cadr entry) (cadddr owner)))
          (cons depth (cdr entry))]))]))

;; ---------------------------------------------------------------------------
;; Section numbering (#toctree(numbered: …)) and the decorated 'sections list

;; A document's OWN section entries in document order — those it recorded
;; itself, EXCLUDING entries produced by its toctree children (whose
;; 'sections ranges nest inside this doc's). Mirrors docset-doc-title's
;; ownership logic. Reads whatever 'sections is installed (pristine when
;; computing numbers; the shifted/numbered copy during resolution when the
;; nav consults it — the ranges and anchors are stable across decoration, and
;; the nav uses depths only relatively, so either works). The /pos variant
;; returns (cons oldest-position entry) so callers can interleave the entries
;; with the child pages' range positions.
(define (doc-own-sections/pos i doc)
  (define secs (info-ref i 'sections '()))
  (define total (length secs))
  (define start (doc-record-secs-start doc))
  (define end (doc-record-secs-end doc))
  (define slice (reverse (take (drop secs (- total end)) (- end start))))
  (define child-ranges
    (for/list ([cid (in-list (doc-record-children doc))])
      (define r (docset-doc-ref i cid))
      (cons (doc-record-secs-start r) (doc-record-secs-end r))))
  (for/list ([entry (in-list slice)]
             [pos (in-naturals start)]
             #:unless (for/or ([r (in-list child-ranges)])
                        (and (>= pos (car r)) (< pos (cdr r)))))
    (cons pos entry)))

(define (doc-own-sections i doc)
  (map cdr (doc-own-sections/pos i doc)))

;; anchor -> dotted number ("1", "1.1", "2.1"), for every section under a
;; numbered toctree. Mode-independent (keyed by anchor, computed against the
;; PRISTINE 'sections, in DOCUMENT order — a page's own entries and its child
;; pages interleave at the positions their 'sections ranges actually occupy,
;; i.e. at the #toctree marker, so numbers stay monotonic in the merged
;; single-file page and in #toc even when sections follow the toctree). A
;; section's number level = the page's numbering level (docset-set-numbering!,
;; 1 for a numbered toctree's direct children) plus its heading depth below
;; the page title. One counter per numbering GROUP: each numbered toctree
;; numbers its subtree independently — two sibling numbered toctrees both
;; restart at "1" (sphinx parity). A numbered page with no heading of its own
;; cannot anchor its descendants' numbers: explicit error when it has toctree
;; children (a childless one simply gets no number). Both strategies apply
;; this same map to headings (number-headings) and to the #toc/nav titles
;; (via decorate-sections).
(define (section-numbering ds)
  (define i (docset-info ds))
  (define secs (reverse (info-ref i 'sections '()))) ; oldest first = doc order
  (define out (make-hash))
  (define group-ctxs (make-hash)) ; numbering group -> level counter
  (let walk ([id (docset-root-id ds)])
    (define doc (docset-doc ds id))
    (define lvl (docset-numbering-level i id))
    (define ctx (and lvl (hash-ref! group-ctxs (docset-numbering-group i id)
                                    make-hash)))
    (define start (doc-record-secs-start doc))
    (define end (doc-record-secs-end doc))
    ;; children by range position = document order (they evaluated in sequence)
    (define kids
      (sort (for/list ([cid (in-list (doc-record-children doc))])
              (define r (docset-doc-ref i cid))
              (list (doc-record-secs-start r) (doc-record-secs-end r) cid))
            < #:key car))
    (define own (doc-own-sections i doc))
    (define base (if (pair? own) (apply min (map car own)) 1)) ; page title depth
    (when (and ctx (null? own) (pair? kids))
      (error 'build
             "page ~a is under a numbered toctree but has no section heading of its own; its children's numbers would have no parent number"
             (doc-record-path doc)))
    ;; walk positions [start, end): recurse into a child page where its range
    ;; begins; number an own entry otherwise.
    (let loop ([es (take (drop secs start) (- end start))] [pos start] [kids kids])
      (cond
        [(and (pair? kids) (= pos (car (car kids))))
         (define kid-end (cadr (car kids)))
         (walk (caddr (car kids)))
         (loop (drop es (- kid-end pos)) kid-end (cdr kids))]
        [(null? es)
         ;; defensive: ranges nest, so remaining kids (if any) start at `pos`
         ;; and were consumed above; nothing owns a position past `end`.
         (for ([k (in-list kids)]) (walk (caddr k)))]
        [else
         (when ctx
           (hash-set! out (cadr (car es))
                      (bump-counter! ctx (+ lvl (- (car (car es)) base)))))
         (loop (cdr es) (add1 pos) kids)])))
  out)

;; Counter stack held in `ctx` (level -> count). Increment `level`, clear
;; deeper levels, and return the dotted number for levels 1..level.
(define (bump-counter! ctx level)
  (hash-set! ctx level (add1 (hash-ref ctx level 0)))
  (for ([k (in-list (hash-keys ctx))] #:when (> k level)) (hash-remove! ctx k))
  (string-join
   (for/list ([l (in-range 1 (add1 level))]) (number->string (hash-ref ctx l 0)))
   "."))

;; The 'sections list to install during resolution: depths shifted to the doc
;; graph (so #toc nests child pages' sections under their page instead of flat)
;; and titles prefixed with their number when numbered. Returns #f when neither
;; applies, so plain documents never churn. `cap-h6?` forwards to
;; shifted-sections (single-file caps; multi-file does not).
(define (decorate-sections ds nums #:cap-h6? cap?)
  (define i (docset-info ds))
  (define shifted (shifted-sections ds #:cap-h6? cap?))
  (cond
    [(and (not shifted) (zero? (hash-count nums))) #f]
    [else
     (define base (or shifted (info-ref i 'sections '())))
     (for/list ([entry (in-list base)])
       (define num (hash-ref nums (cadr entry) #f))
       (if num
           (list (car entry) (cadr entry)
                 (list (string-append num " ") (caddr entry)))
           entry))]))

;; ---------------------------------------------------------------------------
;; Multi-file strategy

;; Everything the #:wrap-page hook may want: the page's doc-record, its title
;; (Content), prev/next/up doc ids (#f at the edges; up = toctree parent),
;; href : doc-id -> relative link string from this page, and
;; title-of : doc-id -> title Content (for link text to other pages).
(struct page-view (doc title prev next up href title-of) #:transparent)

(define (make-multi-file-strategy #:wrap-page [wrap default-wrap-page])
  (build-strategy
   'multi-file
   (lambda (ds backend)
     (define i (docset-info ds))
     (define ids (docset-pages ds))
     (define rels (output-paths ds backend)) ; id -> relative path string
     (define nums (section-numbering ds))
     ;; Install the shifted (graph-nested) + numbered 'sections for the whole
     ;; resolution so a multi-file #toc reflects the doc graph — child pages'
     ;; sections nested under their page entry, with numbers — instead of
     ;; listing every page's top sections flat. Restored afterwards (no docset
     ;; mutation). #toc depths are list structure only here, so a deep graph
     ;; does not error (cap-h6? #f), unlike single-file heading inlining.
     (define original (info-ref i 'sections '()))
     (define decorated (decorate-sections ds nums #:cap-h6? #f))
     (define idx (docref-index ds (docref-root-dir ds)))
     (define prior-idx (info-ref i docref-index-key #f))
     ;; markers -> navs, then resolve + decode + number-headings, per page
     (define contents
       (dynamic-wind
        (lambda () (when decorated (info-set! i 'sections decorated))
                   (info-set! i docref-index-key idx))
        (lambda ()
          (for/hash ([id (in-list ids)])
            (define with-navs
              (replace-tagged
               (doc-record-content (docset-doc ds id)) toctree-marker-tag
               (lambda (marker) (toctree-nav i ds (toctree-marker-rec i marker) id rels))))
            (values id (number-headings (decode-flow (resolve-content with-navs i)) nums))))
        (lambda () (when decorated (info-set! i 'sections original))
                   (info-set! i docref-index-key prior-idx))))
     ;; anchor -> page registry from the RESOLVED IR of every page
     (define anchor->page (anchor-registry ids contents rels))
     (define up-of (parent-map ds ids))
     (for/list ([id (in-list ids)]
                [prev (in-list (cons #f ids))]
                [next (in-list (append (cdr ids) '(#f)))])
       (define rel (hash-ref rels id))
       (define (href-to other) (relative-href rel (hash-ref rels other)))
       ;; backend-agnostic cross-page link resolution: qualify `#anchor`
       ;; hrefs whose target lives on another page; leave local/unknown ones
       (define linked
         (rewrite-hrefs
          (hash-ref contents id)
          (lambda (href)
            (and (string-prefix? href "#")
                 (let ([target (hash-ref anchor->page (substring href 1) #f)])
                   (and target
                        (not (equal? target id))
                        (string-append (href-to target) href)))))))
       (define doc (docset-doc ds id))
       ;; page titles for prev/up/next chrome carry the section number too,
       ;; so a numbered book's crumbs agree with its toctree nav and #toc.
       (define (page-title other)
         (numbered-nav-title i nums (docset-doc ds other)))
       (define view
         (page-view doc (page-title id) prev next (hash-ref up-of id #f)
                    href-to page-title))
       (build-output rel ((build-backend-render backend) (wrap view linked)))))))

;; nav-title with the page's section number prepended when its title heading
;; is numbered ('sections is pristine here — crumbs are built after the
;; decorated resolution — so the number comes from the anchor map).
(define (numbered-nav-title i nums doc)
  (define own (doc-own-sections i doc))
  (define num (and (pair? own) (hash-ref nums (cadr (car own)) #f)))
  (define title (nav-title doc))
  (if num (list (string-append num " ") title) title))

;; A %toctree marker on a page becomes a nav of links to the child pages
;; (titles as text). Under each page link the page's own sub-sections and its
;; toctree children nest, interleaved in DOCUMENT order (a child page appears
;; where its #toctree marker sits, not grouped after the sections — so a
;; numbered nav reads monotonically); everything is bounded by depth: — page
;; titles and section entries counted uniformly (sphinx-maxdepth style;
;; depth: #f = unlimited). The numbers of a numbered toctree are already
;; baked into the titles (they come from the shifted+numbered 'sections
;; installed during resolution).
;;
;; hidden: registers the children (they are still pages, still navigable via
;; prev/next/up, refs still resolve) but emits nothing here — for an index that
;; links its children by hand. Single-file mode has no nav to hide, so it
;; ignores hidden: and inlines the content (dropping it would lose documents).
(define (toctree-nav i ds rec page-id rels)
  (cond
    [(toctree-rec-hidden rec) '()]
    [else
     (elem 'nav #:attrs '((class . "toctree"))
           (apply elem 'ul
                  (for/list ([cid (in-list (toctree-rec-children rec))])
                    (page-li i ds cid page-id rels 1 (toctree-rec-depth rec)))))]))

;; One <li> for child page `cid` at nav level `level`: the page link, then —
;; while depth: allows — its sub-items nested below.
(define (page-li i ds cid page-id rels level depth)
  (define doc (docset-doc ds cid))
  (define link
    (elem 'a
          #:attrs (list (cons 'href (relative-href (hash-ref rels page-id)
                                                   (hash-ref rels cid))))
          (page-nav-title doc (doc-own-sections i doc))))
  (define items (page-subitems i ds doc))
  (define subs
    (if (null? items)
        '()
        (let-values ([(lis _rest)
                      (item-lis items (apply min (map item-rel items))
                                i ds doc page-id rels level depth)])
          lis)))
  (if (null? subs) (elem 'li link) (elem 'li link (apply elem 'ul subs))))

;; A page's nav link text: its title (already numbered when the toctree is
;; numbered, since it is read from the decorated 'sections), sanitized.
(define (page-nav-title doc own)
  (sanitize-title (if (pair? own) (caddr (car own)) (doc-record-title doc))
                  (doc-record-path doc)))

;; The document-order items under a page's nav link: (pos 'sec rel entry) for
;; each own section entry below the title, (pos 'page rel cid) for each
;; toctree child at its marker site ('sections range start). `rel` is the
;; item's nesting depth relative to the page title's heading depth — sections:
;; entry depth - title depth (0 = a second top-level section, a sibling of
;; the title's nav level); child pages: base-depth + 1 - title depth (a
;; toctree inside a depth-d section nests its pages under that section).
;; Position ties (an empty child page at a later section's slot) put the page
;; first — it evaluated first.
(define (page-subitems i ds doc)
  (define own (doc-own-sections/pos i doc))
  (define title-depth (if (pair? own) (apply min (map cadr own)) 1))
  (define sec-items
    (for/list ([pe (in-list (if (pair? own) (cdr own) '()))])
      (list (car pe) 'sec (- (cadr pe) title-depth) (cdr pe))))
  (define page-items
    (for/list ([cid (in-list (doc-record-children doc))])
      (define r (docset-doc-ref i cid))
      (list (doc-record-secs-start r) 'page
            (- (add1 (doc-record-base-depth r)) title-depth) cid)))
  (sort (append sec-items page-items)
        (lambda (a b)
          (or (< (car a) (car b))
              (and (= (car a) (car b))
                   (eq? (cadr a) 'page) (eq? (cadr b) 'sec))))))

(define (item-rel it) (caddr it))

;; Sibling <li>s for `items` at rel-depth `d` (deeper items nest under the
;; preceding sibling); returns (values lis remaining). `doc` is the page the
;; 'sec entries live on (their link target); `level` its nav level. A
;; section's nav level is (+ level rel), a child page's (add1 level) — an
;; item past depth: is pruned together with its subtree.
(define (item-lis items d i ds doc page-id rels level depth)
  (let loop ([its items] [acc '()])
    (cond
      [(null? its) (values (reverse acc) '())]
      [(< (item-rel (car its)) d) (values (reverse acc) its)]
      [(> (item-rel (car its)) d) ; deeper without a parent at d: nest defensively
       (define-values (sub rest)
         (item-lis its (add1 d) i ds doc page-id rels level depth))
       (loop rest
             (cond
               [(null? sub) acc]
               [(pair? acc) (cons (li-add-sublist (car acc) sub) (cdr acc))]
               [else (cons (elem 'li (apply elem 'ul sub)) acc)]))]
      [else
       (define it (car its))
       (define-values (sub rest)
         (item-lis (cdr its) (add1 d) i ds doc page-id rels level depth))
       (define li
         (case (cadr it)
           [(sec)
            (and (or (not depth) (<= (+ level d) depth))
                 (let* ([e (cadddr it)]
                        [link (elem 'a
                                    #:attrs (list (cons 'href (page-frag-href
                                                               page-id (doc-record-id doc)
                                                               (cadr e) rels)))
                                    (sanitize-title (caddr e) (doc-record-path doc)))])
                   (if (null? sub) (elem 'li link) (elem 'li link (apply elem 'ul sub)))))]
           [(page)
            (and (or (not depth) (< level depth))
                 (let ([pli (page-li i ds (cadddr it) page-id rels (add1 level) depth)])
                   (if (null? sub) pli (li-add-sublist pli sub))))]))
       (loop rest (if li (cons li acc) acc))])))

;; Fragment link to `anchor` on page `target-id`, relative to `page-id` (bare
;; "#anchor" when they are the same page).
(define (page-frag-href page-id target-id anchor rels)
  (define frag (string-append "#" anchor))
  (if (equal? page-id target-id)
      frag
      (string-append (relative-href (hash-ref rels page-id)
                                    (hash-ref rels target-id))
                     frag)))

(define (li-add-sublist li sub)
  (element 'li (element-attrs li)
           (append (element-children li) (list (apply elem 'ul sub)))
           (element-loc li)))

;; A section/doc title reused as nav/crumb/toc link text must not carry two
;; things along: element `id`s (they would duplicate the source heading's
;; anchor on every page that links it — tripping the ambiguity check) and
;; delayed nodes (two-pass content in a title cannot be resolved outside its
;; page context; after the info round-trip its proc is Any-wrapped anyway).
;; Strip the former, error on the latter, naming `who` (the document path).
(define (sanitize-title c who)
  (let walk ([c c])
    (cond
      [(delayed? c)
       (error 'build
              "the section title of ~a contains two-pass content (#ref/#toc/…); a title reused for toctree/page navigation must be plain content"
              who)]
      [(element? c)
       (element (element-tag c)
                (filter (lambda (a) (not (eq? (car a) 'id))) (element-attrs c))
                (map walk (element-children c))
                (element-loc c))]
      [(pair? c) (map walk c)]
      [else c])))

;; A doc's title is 'sections Content captured at collect time.
(define (nav-title doc)
  (sanitize-title (doc-record-title doc) (doc-record-path doc)))

;; Default #:wrap-page: prepend a prev/up/next nav (nothing for a one-page
;; set). Replace via make-multi-file-strategy for sidebars, breadcrumbs,
;; page shells, …
(define (default-wrap-page view content)
  (define links
    (filter values
            (list (crumb view (page-view-prev view) "prev" "← ")
                  (crumb view (page-view-up view) "up" "↑ ")
                  (crumb view (page-view-next view) "next" "→ "))))
  (if (null? links)
      content
      (cons (apply elem 'nav #:attrs '((class . "docnav")) links) content)))

(define (crumb view id class marker)
  (and id
       (elem 'a #:attrs (list (cons 'class class)
                              (cons 'href ((page-view-href view) id)))
             marker
             ((page-view-title-of view) id))))

;; (After default-wrap-page: module-level value references are ordered.)
(define multi-file-strategy (make-multi-file-strategy))

;; ---------------------------------------------------------------------------
;; Shared helpers

(define (output-file-name id backend)
  (path->string (path-replace-extension (file-name-from-path id)
                                        (build-backend-ext backend))))

;; id -> output path relative to the root document's directory, extension
;; swapped. A child outside the root's directory tree has no sane output
;; location — explicit error.
(define (output-paths ds backend)
  (define root-dir (path-only (docset-root-id ds)))
  (define taken (make-hash)) ; rel output path -> source id
  (for/hash ([id (in-list (docset-pages ds))])
    (define rel (find-relative-path root-dir id))
    (when (or (not (relative-path? rel))
              (memq 'up (explode-path rel)))
      (error 'build
             "document ~a is outside the root document's directory ~a; cannot place its output"
             id root-dir))
    (define out (path->string (path-replace-extension rel (build-backend-ext backend))))
    ;; extension swapping can merge sources (ch.yamd + ch.md -> ch.html):
    ;; a silent clobber is not an output, it is a bug in the set.
    (define prior (hash-ref taken out #f))
    (when prior
      (error 'build "documents ~a and ~a both map to output ~a" prior id out))
    (hash-set! taken out id)
    (values id out)))

;; Relative link from one output-relative page path to another, correct
;; across subdirectories ("guide/setup.html" -> "intro.html" is
;; "../intro.html"). Anchored at a fabricated common base so no filesystem
;; access happens.
(define (relative-href from-rel to-rel)
  (define base (string->path "/"))
  (define from-dir
    (or (path-only (path->complete-path from-rel base)) base))
  (path->string
   (find-relative-path from-dir (path->complete-path to-rel base))))

;; anchor -> page id, from every page's resolved IR. Two failure modes, both
;; explicit errors: the same anchor twice on ONE page (a duplicate html/md id
;; or typst <label> — a hard compile error downstream) and the same anchor on
;; two DIFFERENT pages (a `#anchor` link would be ambiguous). Shared info-table
;; counters make std anchors unique by construction; this guards hand-written
;; ids, which the earlier per-page check catches before the cross-page one.
(define (anchor-registry ids contents rels)
  (define reg (make-hash))
  (for ([id (in-list ids)])
    (define content (hash-ref contents id))
    (check-unique-ids! content (hash-ref rels id))
    (for ([anchor (in-list (content-ids content))])
      (define prior (hash-ref reg anchor #f))
      (when prior ; within-page already ruled out, so prior is another page
        (error 'build "anchor ~s appears on two pages: ~a and ~a"
               anchor (hash-ref rels prior) (hash-ref rels id)))
      (hash-set! reg anchor id)))
  reg)

;; Errors if any element `id` appears more than once in `content` — naming the
;; id and the page. A hand-authored within-page duplicate would otherwise emit
;; a duplicate html/markdown id or a duplicate typst <label> (a hard compile
;; error). Used per page in multi-file mode and on the single merged page.
(define (check-unique-ids! content page)
  (define seen (make-hash))
  (for ([anchor (in-list (content-ids content))])
    (when (hash-ref seen anchor #f)
      (error 'build "anchor ~s is defined more than once on page ~a" anchor page))
    (hash-set! seen anchor #t)))

;; id -> toctree parent id (root absent).
(define (parent-map ds ids)
  (for*/hash ([id (in-list ids)]
              [cid (in-list (doc-record-children (docset-doc ds id)))])
    (values cid id)))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; within-page duplicate id: explicit error naming the id and the page.
  (check-exn #rx"anchor \"dup\" is defined more than once on page p\\.html"
             (lambda ()
               (check-unique-ids!
                (list (elem 'a #:attrs '((id . "dup")) "x")
                      (elem 'a #:attrs '((id . "dup")) "y"))
                "p.html")))
  (check-not-exn
   (lambda ()
     (check-unique-ids!
      (list (elem 'a #:attrs '((id . "a")) "x")
            (elem 'a #:attrs '((id . "b")) "y"))
      "p.html")))

  ;; anchor-registry: the same id on TWO pages is the cross-page ambiguity
  ;; error…
  (check-exn #rx"anchor \"x\" appears on two pages: one\\.html and two\\.html"
             (lambda ()
               (anchor-registry
                '("1" "2")
                (hash "1" (elem 'a #:attrs '((id . "x")) "a")
                      "2" (elem 'a #:attrs '((id . "x")) "b"))
                (hash "1" "one.html" "2" "two.html"))))
  ;; …and a WITHIN-page duplicate is caught per page (the registry's earlier
  ;; guard), not silently accepted as it was before.
  (check-exn #rx"defined more than once on page one\\.html"
             (lambda ()
               (anchor-registry
                '("1")
                (hash "1" (list (elem 'a #:attrs '((id . "x")) "a")
                                (elem 'a #:attrs '((id . "x")) "b")))
                (hash "1" "one.html")))))
