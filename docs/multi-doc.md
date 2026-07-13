# Multi-Source Documents

How a YAMD document set — several `.yamd` files forming one logical document —
is evaluated and built, in two output modes (single page / one page per source)
from the same source tree. Companion to [architecture.md](../architecture.md)
("Multi-source documents" section there is the short version; this file is the
design and its rationale)

## Pluggability Contract (Read This First)

The design is three swappable layers on top of four small core primitives

**To swap any layer, implement its one interface; nothing else changes.**

| You Dislike             | Swap By                                | Untouched |
|-------------------------|----------------------------------------|-----------|
| `#include` semantics    | another `yamd-eval-embedded` caller    | core, builder, toctree |
| `#toctree` manifest     | another doc-record marker plugin       | core, builder strategies |
| single-file assembly    | a `build-strategy`                     | core, plugins, CLI |
| multi-file layout / nav | `#:wrap-page`, or a whole new strategy | everything else |
| a backend               | a backend table entry                  | everything else |

The **core** gains only mode-agnostic primitives, all additive:

1. `current-yamd-source` (runtime.rkt) — the file being evaluated; relative
   paths resolve against it
2. `yamd-eval-embedded` (eval.rkt) — evaluate another file *under the current
   info table* in a fresh namespace and a forked context; cycle-checked
3. `yamd/docset.rkt` — a document registry *keyed by the shared info table
   instance* (`doc-record`s, parent→children edges, toctree records). Pure
   data, no policy. (It lives beside, not inside, the info table: the typed
   table's `Any` boundary would wrap the delayed procs inside stored Content
   opaque.)
4. `yamd/build/rewrite.rkt` (typed) — four generic IR passes: collect ids,
   rewrite hrefs, shift headings, replace marker elements. No knowledge of
   toctree or of any backend

Nothing in core knows what a toctree is beyond storing its record; nothing in
the plugins knows what an output mode is; nothing in the strategies knows how a
child document was discovered. Existing single-file documents never touch any
of this and keep working unchanged

## Why No Manifest

The index document *is* the manifest (sphinx/rst-style): `index.yamd` contains
`#toctree` blocks naming its children; children may contain their own
`#toctree`s. The document-set graph is *discovered during the collect pass* —
evaluating the root evaluates the whole set. Both output modes consume the same
discovered graph, so one source tree serves both. A TOML manifest was
considered and rejected: it would duplicate ordering/nesting information that
the language already expresses, drift from the sources, and require core to
grow a parser for a second language. Everything a manifest would say (member
files, order, nesting, depth) is an argument or body line of `#toctree`

## Include Semantics — `#use(std/include)`, `#include("file.yamd")`

**Eval-time content splice, not read-time text splice.** Typst-like: the child
file is read and evaluated when the `#include` call runs (during the collect
pass), and its Content splices into the call site. Read-time splicing was
rejected because it destroys srclocs, makes cycle detection reader business,
and leaks reader state (`#use` body-reader merges) between files

- **Namespace:** the child evaluates in a **fresh namespace**
  (`make-yamd-namespace`). Host definitions do not leak in either direction;
  `#use` in the child affects only the child. (C's textual `#include` leaks
  defines; typst's doesn't — we follow typst, per the "evaluate/splice" intent.)
- **Context:** the child evaluates under a **forked** context (`#include`
  *does* fork). It reads the includer's values — so a file included under
  `#section` sees `header-depth` and its own `#section`s auto-nest one level
  deeper — but its writes die with the fork
- **Info:** **shared.** The child's `collect!`/`info-add!` land in the
  includer's info table, so `#def` in a child resolves a `#ref` in the parent
  (and vice versa — order independence is unchanged). Anchor counters
  (`sec-<n>`, `ref-<n>`, footnotes) are in that same table, so anchors are
  unique across the whole set by construction
- **Relative paths** resolve against the directory of `current-yamd-source`
  (the *including* file), falling back to `current-directory` for string-fed
  documents. Missing file = explicit error naming both the written and the
  resolved path
- **Cycles:** `current-docset-stack` carries the chain of files currently
  being evaluated (root included). Including (or toctree-ing) a file already
  on the stack is an explicit error printing the whole chain. Including the
  same file twice *sequentially* is legal (it is a function call; anchors stay
  unique via shared counters, though duplicate `#def`s will error as always)
- **Srclocs:** the child is read with its own path as `src`, so every form
  carries child-file srclocs; `eval.rkt`'s error prefixing therefore names the
  right file and line with zero extra machinery

`#include` output is identical in both build modes — included content is
simply part of whatever page it lands on

## Cross-File Two-Pass

Collect spans the whole set for free: children evaluate *during* the root's
collect pass against the one shared info table. Resolution is then per output
page, against that same table:

- **single-file:** the builder splices all pages into one Content tree and
  resolves once. Every anchor is on the page; hrefs stay `#anchor`. This is
  byte-identical to today's pipeline for a set of one document
- **multi-file:** the builder resolves each page separately (same info), then
  runs a **builder-owned, backend-agnostic link-resolution pass**:
  1. Build the anchor registry by walking every page's *resolved* IR and
     collecting `id` attributes → `anchor → page`. An id repeated *within* one
     page (a hand-authored duplicate — it would emit a duplicate html/markdown
     id or typst `<label>`, a hard compile error) and the same anchor on two
     *different* pages (an ambiguous `#anchor` link) are both explicit errors
     naming the id and page. Single-file mode runs the same within-page check
     on its one merged page. (Std anchors are unique by construction via shared
     info counters; these guard hand-written ids.)
  2. Rewrite every `href` of the form `#x` where `x` is an anchor on a
     *different* page into `<relative-page-path>.<ext>#x` (correct `../`
     handling via `find-relative-path`). Same-page hrefs stay `#x`; hrefs not
     found in the registry are left untouched (they may target ids inside
     `raw` nodes, which the IR walk cannot see — documented sharp edge)

Consequences: `#ref`, `#toc`, `#glossary`, footnote backlinks — anything that
emits `href="#…"` + `id="…"` in IR — works across files in both modes with
**zero changes to the plugins and zero changes to the backends**. Plugins keep
emitting plain `#anchor` links; the strategy owns file qualification. A future
plugin gets cross-file links for free by following the same idiom

Registry-by-IR-walk was chosen over "each plugin registers `anchor→file` at
collect time" because it needs no plugin cooperation, cannot drift from what
was actually emitted, and keeps the anchor policy out of core

A cross-set `#toc` reflects the doc graph in **both** modes. Toctree children
evaluate with `header-depth` reset to 0, so their `'sections` entries are all
`h1`-depth in isolation; a strategy therefore installs a *shifted* copy of the
`'sections` list during resolution (each child's entries shifted by its
cumulative toctree base depth, exactly as single-file shifts the inlined
headings) so a child's `h1` nests under the page that lists it. Single-file
caps this shift at `h6` (the depths become real heading tags); multi-file uses
the depths only as `#toc` list structure, so a deep graph is harmless there and
is not capped. `#toc` shows the full section tree in both modes — the toctree
`depth:` bounds the *nav* list, not `#toc` (parallel to `depth:` being ignored
when inlining)

Backend caveat (not solved here, by design): the markdown and typst backends
currently drop element `id`s (typst deliberately renders `href="#id"` as a URL
link, no labels). Cross-*page* links in those backends therefore point at
fragments the serializer does not emit as targets. The rewrite still produces
the right page paths; making md/typst emit anchor targets (typst `<label>`s)
is a documented limitation that can be addressed in backend implementations

## Toctree — `#use(std/toctree)`

```
#section{My book}::

#toctree(depth: 2)
	intro.yamd
	guide/setup.yamd
```

Raw body, one child path per non-blank line (trimmed; paths relative to the
current file). Options: `depth:` (positive integer) bounds nav recursion,
`numbered: #t` adds sphinx-style hierarchical section numbers, `hidden: #t`
registers the children without emitting a nav. Dumb on purpose

### `numbered:` — Hierarchical Section Numbers

`#toctree(numbered: #t)` numbers the child documents' headings ("1", "1.1",
"2", "2.1") and the matching `#toc`/nav entries and prev/up/next crumbs —
**identically in both build modes**, because the numbers are keyed by section
anchor and computed from the doc graph, not from the shift. A section's number
level is its page's *numbering level* (a numbered toctree's direct children are
level 1, their toctree children level 2, …) plus its heading depth below the
page title. Numbers are assigned in **document order**: a page's own sections
and its child pages interleave at the position each child's sections actually
sit (the `#toctree` marker), so the merged single-file page and `#toc` read
monotonically even when sections follow the toctree. The toctree that lists a
document is itself unnumbered: the index's own `#section{Book}` heading gets no
number, only Book's toctree children do. Numbering propagates into nested
toctrees (an outer `numbered:` defines the scheme for its whole subtree, sphinx
parity), and **each numbered toctree counts independently** — two sibling
`numbered:` toctrees on one page both restart at "1" (also sphinx). A document
with no heading of its own gets no number; if such a document also has toctree
children, that is an explicit error (its children's numbers would have no
parent number to hang off). Two mechanisms, one anchor→number map: an IR pass
(`number-headings`) prepends the number to the heading elements, and the same
map decorates the `'sections` titles so `#toc`, the nav and the crumbs render
it — plugins stay unchanged

### `hidden:` — Register Without Rendering a Nav

`#toctree(hidden: #t)` registers its children (they build, `#ref`s across them
resolve, and in multi-file mode they become navigable pages with prev/next/up
links) but emits **no nav** at the call site — for an index that links its
children by hand (sphinx `:hidden:`). This is a multi-file-only effect: the
single-file marker inlines the children's content regardless (a "hidden" toctree
there has no nav list to suppress, and dropping the content would lose the
documents from the one page — parallel to `depth:` being ignored when inlining).
`hidden:` hides exactly the call-site nav, nothing else: the children's sections
still appear in a cross-set `#toc` (they are part of the set) and still get
numbers under a `numbered:` ancestor

**At collect time (mode-independent):** for each child, `#toctree`

- Evaluates it via `yamd-eval-embedded`, with the context forked **and
  `header-depth` reset to 0** — a toctree child is a standalone document whose
  top sections are `h1`, unlike an `#include`d fragment;
- Registers a `doc-record` in the shared info: id (normalized path), source
  path, *unresolved* Content, title (first section heading the child itself
  records, excluding its own toctree-children's sections; falls back to the
  file stem), the section-entry range it produced, its base depth, and its
  children (accumulated by *its* toctrees, recursively — nesting just works);
- Errors if the child is already registered (a document may appear in exactly
  one toctree, like sphinx) or on include/toctree cycles; document identity is
  the fully **symlink-resolved** simple path (`normalize-path`), so `.`/`..`
  spellings AND symlink aliases of one file unify — without this, a directory
  alias like `sub -> .` would both duplicate pages and defeat cycle detection
  (each recursion level would grow a fresh spelling). Missing-file errors
  still print the pre-resolution spelling the author can recognize;
- Emits a `%toctree` **marker element** referencing a toctree record (children,
  `depth:`, and the `header-depth` at the call site)

`%`-tags never reach a backend, so rendering a toctree document through the
plain single-document pipeline (`yamd-file->html`, no builder) fails loudly
instead of half-working — use the builder (the CLI always does)

Markers rather than mode branching keep the collect pass mode-agnostic: the
same evaluated docset feeds every strategy, and a new strategy can interpret
the marker any way it likes

**Single-file mode** (sphinx-latex-style inlining): the marker is replaced by
the children's content, in order, recursively. Header-depth interaction:
children were evaluated at depth 0, so the strategy **shifts their headings**
by the marker's recorded base depth (`h1`→`h(1+d)`, error past `h6`), shifts
nested toctrees cumulatively, and applies the same shift to the children's
`'sections` info entries **before** resolving — so a global `#toc` shows the
merged, correctly-nested tree. `depth:` is ignored when inlining (content is
never truncated), documented sphinx-latex behavior

**Multi-file mode:** the marker becomes a `nav.toctree` — a nested list of
links to the child *pages* (titles as link text). Under each page link the
page's own sub-sections (from its `'sections` range) and its toctree children
nest, interleaved in document order (a child page sits where its `#toctree`
marker is; a toctree inside a section nests its pages under that section); the
whole tree is bounded by `depth:`, counting page titles and section entries
uniformly (sphinx `:maxdepth:` style; unlimited if absent). Children become
pages of their own, recursively. A `hidden:` toctree emits nothing here (see
above)

## Builder Interface — `yamd/build.rkt`

```racket
(struct build-output   (path text))          ; path relative to the output root
(struct build-backend  (name ext render))    ; e.g. 'html ".html" content->html
(struct build-strategy (name build))         ; build : docset build-backend -> (listof build-output)
(struct docset (root-id info))               ; + docset-doc, docset-pages (DFS order)

(evaluate-docset root-path)                       -> docset
(build-docset root-path #:strategy s #:backend b) -> (listof build-output)

single-file-strategy                              ; the strategy value
(make-multi-file-strategy #:wrap-page wrap)       ; nav as an overridable hook
multi-file-strategy                               ; = (make-multi-file-strategy)
```

`evaluate-docset` runs the whole collect pass (root + discovered children) and
returns the docset: per-doc unresolved Content plus the shared info. A
strategy receives exactly that and the backend triple, and returns named
outputs — it owns marker interpretation, resolution order, link rewriting and
serialization. That is the whole contract; `build-docset` is just
`(strategy (evaluate-docset root) backend)`

Multi-file navigation is a hook, not hardcoded: `#:wrap-page` receives a
`page-view` — the page's `doc-record`, its title, `prev`/`next`/`up` ids (DFS
order; `up` = toctree parent), and an `href` function mapping any doc id to a
relative link from this page — plus the page's Content, and returns the final
page Content. The default prepends `nav.docnav` with prev/up/next links (and
adds nothing for a one-page set). Sidebars, breadcrumbs, HTML page shells:
supply your own `wrap-page`, or a whole new strategy for stronger opinions

Strategy contract details: a strategy must **not mutate the docset** — the
same evaluated set may be built again (another strategy, or twice); the
single-file strategy restores the pristine `'sections` list after resolving
against the shifted copy. Titles reused as nav/crumb link text are sanitized:
element `id`s are stripped (they would duplicate the source heading's anchor
on every linking page) and two-pass content (`#ref` in a section title) is an
explicit error naming the document

Output paths: each page keeps its source path relative to the root document's
directory, extension swapped for the backend's. A toctree child outside the
root's directory tree cannot be given a sane output path and is an explicit
error, as are two sources mapping to one output (`ch.yamd` + `ch.md`). This
works identically for markdown/typst (one `.md`/`.typ` per page; hrefs
rewritten with the right extension) — link *target* emission in those backends
is the caveat noted above

## CLI

```
racket -l- yamd/cli file.yamd [--backend html|markdown|typst]
                              [--mode single|multi] [-o out]
```

- `--mode single` (default): exactly today's behavior for existing documents
  (same pipeline steps, same bytes), plus toctree inlining. `-o` is a file;
  default stdout
- `--mode multi`: `-o` is a **required directory** (created if missing);
  every page is written under it, subdirectories preserved
- Modes live in a name→strategy table next to the backend table — a new
  strategy is one table entry

## Testing

- `tests/golden/multidoc/` — single-file-pipeline goldens for `#include`
  (splice, header-depth nesting, cross-file xref). The golden runner skips
  `_`-prefixed `.yamd` files: an underscore marks an auxiliary source that is
  included by a test root rather than being a case itself
- `tests/multidoc.rkt` — rackunit over fixture trees in
  `tests/fixtures/multidoc/`: doc-set builds in both modes (page set, nav,
  cross-page href rewriting incl. `../`, heading shift, `depth:`), all error
  paths (cycle chain, duplicate toctree listing, missing file, shift past h6),
  single↔plain-pipeline parity, and CLI `--mode` end to end
