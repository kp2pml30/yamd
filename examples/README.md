# YAMD examples

Two runnable example projects, plus this guide to building your own. The
sources are the reference: every construct below appears in a file here, and
`tests/examples.rkt` keeps them compiling against all backends.

- **`book/`** — the *Stargazer Handbook*, a multi-file document set
  exercising every major feature, including a project-local plugin under
  `book/ext/`.
- **`raw-html/`** — a single page using the raw-HTML escape hatch, kept
  separate because raw HTML is deliberately non-portable (see
  [The escape hatch](#the-escape-hatch-and-why-it-lives-apart)).

Where to find what in `book/`:

| File | Shows |
|---|---|
| `index.yamd` | `#toctree` (`numbered:`, `depth:`), `#toc`, cross-file `#ref` before its `#def`, `#include` |
| `intro.yamd` | sections + `::`, nested auto-depth headings, lists, `#link`, `#image`, `#blockquote`, `#hr`, `##` literal, `\|{ }\|` fences |
| `usage.yamd` | `#table` with `#row`/`#cell` and positional `#row{a}{b}`, `#ptable`, a nested `#toctree` |
| `scripting.yamd` | `#code` blocks (python, gdscript), inline `#code`, AsciiMath `#m` and `#math` |
| `spec.yamd` | `std/rfc2119` (boilerplate + keyword refs), `#def`/`#ref`, `#glossary`, footnotes (`#note`/`#notes`) |
| `extending.yamd` | host escapes (`#(define …)`, `#(for/list …)` with `#para{…}` re-entry), comments (`# `, `#;{…}`), the local plugin, `#fork-context` + context keys |
| `common/colophon.yamd` | a fragment `#include`d from two places |
| `ext/admonition.rkt` | the local extension (walked through below) |

## Rendering

From the repo root (Racket only runs inside the dev shell):

```sh
# single HTML page to stdout (default backend, default mode)
nix develop --command racket -l- yamd/cli examples/book/index.yamd

# one page per source file
nix develop --command racket -l- yamd/cli examples/book/index.yamd --mode multi -o out/

# other backends
nix develop --command racket -l- yamd/cli examples/book/index.yamd --backend markdown -o book.md
nix develop --command racket -l- yamd/cli examples/book/index.yamd --backend typst -o book.typ

# or without entering the shell
nix run . -- examples/book/index.yamd
```

`--backend` is `html` (default), `markdown`, or `typst`; `--mode` is `single`
(default) or `multi` (requires `-o <directory>`). Note that `--mode multi`
writes only the pages — assets like `book/images/` are copied by you.

## Starting a document set

The index document *is* the manifest: `#toctree` names the children, children
may nest their own (`usage.yamd` does), and evaluating the root discovers the
whole graph. The skeleton of `book/index.yamd`:

```
#use(std/toctree)
#use(std/include)
#use(std/xref)

#section{Stargazer Handbook}::

Intro prose…

#toc

#toctree(numbered: #t depth: 2)
	intro.yamd
	usage.yamd
```

Decisions that shape a set:

- **`#toctree` vs `#include`.** A toctree child is a standalone document —
  fresh heading depth, its own page in multi mode. An `#include` is a splice —
  the fragment becomes part of the including page and inherits its heading
  depth. Chapters are toctree children; a shared colophon is an include.
- **`numbered: #t`** gives sphinx-style hierarchical section numbers,
  identical in both output modes; **`depth:`** bounds the *nav* recursion in
  multi mode (it never truncates inlined content).
- **One info table spans the set**, so `#def` in one file resolves `#ref` in
  another regardless of order (`index.yamd` refs a term `spec.yamd` defines),
  and `#toc`, `#glossary`, footnotes and section numbers are set-wide.
- **`#use` is per-file.** Each document is evaluated in a fresh namespace:
  a chapter that calls `#include` needs its own `#use(std/include)`, even if
  the index already loaded it.

## Single vs multi-file output

Both modes consume the same source tree; choose per build, not per project.
Single mode inlines toctree children (headings shifted under the toctree
site, like a LaTeX book); multi mode makes each child a page, replaces the
toctree with a nav, adds prev/up/next links, and rewrites cross-page
fragment links. Design and sharp edges: [docs/multi-doc.md](../docs/multi-doc.md).

## Plugins

Everything beyond `std/base` (sections, lists, links, …, loaded by default)
is opt-in:

```
#use(std/table, std/math: asciimath)   # collection plugins
#use("ext/admonition.rkt")             # a FILE, relative to this document
```

(`# ` is YAMD's line comment — `;` would be plain text.)

`std/x` maps to the `yamd/std/x` collection; a string is a path relative to
the document — that is all a "project-local plugin" is.

## Writing a local extension

`book/ext/admonition.rkt` is the template; it shows both halves of the
plugin ABI on purpose.

**1. An ordinary function** — Content in, IR out. `#admonition(kind:
"warning"){…}` is just a Racket function with keyword arguments; the reader
turns `kind: "warning"` into `#:kind "warning"` and passes the parsed body
as the last argument:

```racket
(define (admonition #:kind [kind #f] #:title [title #f] . bodies)
  (define k (or kind (context-ref 'admonition-kind "note")))
  …
  (elem 'blockquote #:attrs (list (cons 'class …)) … bodies))
```

Because its body is ordinary markup, this function needs **no reader
registration at all**. Two details worth stealing:

- the default `kind` comes from a *context* key, so a document can scope a
  default with `#fork-context::` + `#(context-set! 'admonition-kind "tip")`
  (see the end of `extending.yamd`) — the setting dies with the block;
- it emits only portable tags (`blockquote`/`p`/`strong`). The typst backend
  renders a fixed tag vocabulary; a plugin emitting a bare `div` would build
  as HTML and fail as typst. Check both before shipping an extension.

**2. A custom body reader** — for bodies that are *not* markup.
`#keys{Ctrl+Shift+P}` wants `+`-separated key names, parsed at read time
with located errors. That is what the `yamd-reader` submodule declares:

```racket
(module* yamd-reader #f
  (require yamd/reader/registry)
  (provide body-readers)

  (define (parse-keys text loc)
    …validate; error with loc on bad input…
    (datum->syntax #f (list 'quote parts) loc))

  (define body-readers
    (hasheq 'keys (body-reader parse-keys #f))))
```

When the reader sees `#use("ext/admonition.rkt")` it merges this table, so
`#keys{…}` bodies bypass the markup parser and arrive at the runtime `keys`
function as parsed data. The `parse` slot may also be `'raw` (verbatim text,
e.g. `std/html`) or `'markup`; the orthogonal second field (`lazy?`) makes
the reader wrap any body in a thunk so the callee controls evaluation —
that is how `#section` forks its context (see `yamd/std/base.rkt`).

Conventions to follow (from `yamd/std/`): one module per plugin, plain
functions, a `module+ test` with rackunit (the book's plugin tests are run
by `tests/examples.rkt`), and never `#(require …)` a plugin from a document
— always `#use`, or struct identities break across a document set.

## The escape hatch (and why it lives apart)

`examples/raw-html/` uses `#use(std/html)`: `#html{…}` for inline fragments,
`#html-block` for flow content. Raw nodes are passed through by the HTML
backend, tolerated by markdown (CommonMark embeds HTML), and **refused** by
typst — an error, not silent loss. That is why the page is its own root
instead of a handbook chapter: one raw node anywhere in a set poisons the
whole set's typst build. Prefer a portable extension (above); reach for raw
HTML only in documents that will only ever be HTML.

## Formatting

The formatter is conservative and reader-verified; CI-style check:

```sh
nix develop --command racket -l- yamd/cli --fmt --check examples/book/index.yamd
nix develop --command racket -l- yamd/cli --fmt examples/book/index.yamd   # rewrite in place
```

Every file here stays `--fmt --check` clean; `tests/examples.rkt` enforces it.

## Gotchas we hit writing this

- Indentation is **tabs**, one per level; spaces at structure positions are
  errors.
- `#name` inside prose is a **call**, even in `#tt{…}` — write `##name` to
  *mention* a function. (`#tt{#row}` silently calls `row`!)
- Consecutive `#section{A}:: … #section{B}::` at one level **nest** (B inside
  A, one heading deeper). For sibling sections, give each its own file (as
  this book does) or use inline `{…}` bodies.
- Commas between call arguments are `#use`-only sugar; elsewhere `(args)` is
  host code — `#toctree(numbered: #t depth: 2)`, no comma.
- In host code, `list` is YAMD's `#list` function (std/base shadows Racket's)
  — build data with `quote`: `#(define xs (quote ("a" "b")))`.
- `#(for …)` discards its body's value; use `#(for/list …)` so the generated
  content actually lands in the document.

## Further reading

- [docs/lang/index.yamd](../docs/lang/index.yamd) — the language docs (spec + per-feature guides).
- [docs/multi-doc.md](../docs/multi-doc.md) — document sets, `#toctree`,
  `#include`, build strategies.
- [docs/architecture.md](../docs/architecture.md) — implementation internals
  and the plugin ABI in full.
