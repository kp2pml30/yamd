# YAMD

[![standard-readme compliant](https://img.shields.io/badge/readme%20style-standard-brightgreen.svg?style=flat-square)](https://github.com/RichardLitt/standard-readme)
![Static Badge](https://img.shields.io/badge/vibe-coded-purple)

Indentation-based extensible markup language, implemented in Racket.

## Table of Contents

- [Background](#background)
- [Install](#install)
- [Usage](#usage)
- [Plugins](#plugins)
- [Testing](#testing)
- [Editor support](#editor-support)
- [Contributing](#contributing)
- [License](#license)

## Background

YAMD is a markup language where structure comes from indentation (tabs), and `#` is the only globally special character. Every other symbol is plain text unless a surrounding function gives it meaning.

Documents are Racket programs. `#` escapes from text mode into host code; `{}` re-enters markup. Headings, lists, links, tables, and math are library functions — not syntax. The core is minimal; everything else is a plugin.

The full language specification lives in [docs/lang/index.yamd](docs/lang/index.yamd); implementation internals in [docs/architecture.md](docs/architecture.md).

**Two-pass evaluation** runs collect (document evaluation, where functions record metadata) followed by resolve (where deferred nodes — TOC entries, cross-references, glossary — are filled in). Plugins use this machinery without touching the core.

The pipeline is: read → eval (collect) → resolve → decode → render. The IR is an element tree; the HTML backend serializes it. A `raw` escape hatch (HTML backend only) is available for verbatim HTML, but it is explicitly non-portable.

## Install

Prerequisites: [Nix](https://nixos.org) with flakes enabled.

Enter the development shell:

```sh
nix develop
```

Install the package into the active Racket environment:

```sh
raco pkg install --auto --batch --link yamd/
```

## Usage

A `.yamd` file (indentation is **tabs**):

```
#use(std/xref)

#section{Getting started}::

Use #emph{emphasis} and #strong{bold} inline.

#list(style: "1")
	#item: First item
	#item
		Second item with a nested list:
		#list
			#item: nested a
			#item: nested b

#section{References}::

We define #def("vm"){virtual machine}{a sandboxed execution environment}.
A deterministic #ref("vm") then runs the program.

#glossary
```

Render from the command line with the bundled CLI:

```sh
racket -l- yamd/cli doc.yamd                 # HTML to stdout (default)
racket -l- yamd/cli doc.yamd --backend html -o doc.html
```

`--backend` selects `html` (default), `markdown`, or `typst`; `-o` writes to a
file instead of stdout. Unknown backends and missing input files fail with a
clean `yamd: …` message and a nonzero exit code.

With Nix, no install step is needed:

```sh
nix run . -- doc.yamd
nix run . -- doc.yamd --backend html -o doc.html
```

Or use the plain evaluator:

```sh
racket -e '(require yamd) (display (yamd-file->html "doc.yamd"))'
```

### Backends

The IR is serialized by a pluggable backend, selected with `--backend`:

- **`html`** (default) — faithful HTML; the `raw` escape hatch (`std/html`)
  passes verbatim HTML through. This is the reference serialization.
- **`markdown`** — CommonMark / GFM. Constructs with a Markdown form are
  rendered natively (headings, emphasis, links, images, lists, blockquotes,
  fenced code, GFM pipe tables); tags with no Markdown equivalent (`dfn`,
  `sup`, `dl`, MathML, …) fall back to embedded HTML so no content is dropped.
  Presentation-only attributes Markdown cannot express (`id`, `class`, …) are
  dropped, so some intra-document anchor links are best-effort.
- **`typst`** — [Typst](https://typst.app) markup. Headings, emphasis, lists,
  quotes, code, tables, description lists and AsciiMath (→ Typst math) are
  mapped structurally. HTML-style `id`s are dropped and `href="#id"` becomes a
  plain URL-fragment `#link`; ordered-list numbering styles are not carried.

Each backend refuses `raw` nodes addressed to a different backend and unresolved
`delayed` nodes rather than silently corrupting output.

### `#lang yamd`

A document may instead be a Racket module by starting with `#lang yamd`:

```
#lang yamd
#section{Hello}::

Some #emph{emphasis} and #strong{bold} text.
```

Such a module `provide`s `doc` (the resolved document Content) and, run
directly (`racket file.rkt`), prints the rendered HTML via its `main`
submodule. `#use`, plugins, contexts and two-pass all behave exactly as with
`yamd-string->html` — the module re-runs the same pipeline at instantiation.

From Racket code:

```racket
(require yamd)
(display (yamd-file->html "doc.yamd"))
;; or for a string:
(display (yamd-string->html "#emph{hello}"))
```

Public API (`(require yamd)`):

| Function | Description |
|---|---|
| `yamd-file->html path` | Read, evaluate, resolve, decode, render to HTML string. |
| `yamd-string->html str` | Same, from a string. |
| `yamd-file->content path` | Resolved + decoded IR (Content), for custom backends. |
| `yamd-eval stxs` | Collect pass only; returns `(values content info)`. |

## Plugins

Plugins are Racket modules. Documents load them with `#use`:

```
#use(std/table, std/math: asciimath)
```

`std/x` maps to `yamd/std/x`; `mod: variant` maps to `mod/variant` (so `std/math: asciimath` → `yamd/std/math/asciimath`). A string is a path relative to the document; any other symbol is a collection path.

### `std/base`

Loaded by default. Provides `emph`, `strong`, `tt`, `para`, `section`, `list`/`item`, `link`.

Sections auto-detect heading level from nesting depth; `#fork-context::` isolates context for a block.

```
#section{Top}::

Introduction.

#section{Nested}::

This becomes h2 automatically.
```

### `std/math/asciimath`

AsciiMath notation via a custom body reader. Inline math uses `#m{…}`; block display uses `#math`.

```
#use(std/math: asciimath)

The identity #m{e^(i pi) + 1 = 0} is famous.

#math
	x = (-b +- sqrt(b^2 - 4a c)) / (2a)
```

### `std/code`

Raw body reader — `#` inside a code block is literal, braces are literal. Syntax highlighting is planned.

```
#use(std/code)
#code(lang: "python")
	def greet(name): # not a call
		msg = {"hi": name}  ## dict, raw braces
		return msg
```

### `std/xref`

Cross-references, glossary, TOC — all via two-pass. `def` records in collect; `ref`/`glossary`/`toc` resolve in pass 2, so order in the document does not matter.

```
#use(std/xref)

#toc

#section{Alpha}{Intro. #section{Alpha One}{A nested subsection.}}
#section{Beta}{A sibling section.}

We define #def("vm"){virtual machine}{a sandboxed execution environment}.
A deterministic #ref("vm") then runs the program.

#glossary
```

### `std/table` and `std/pipe-table`

`std/table` provides `#table`/`#row`/`#cell`. `std/pipe-table` provides `#ptable` with a pipe-separated body reader.

```
#use(std/table)
#table(header: 1)
	#row
		#cell: Name
		#cell: Role
	#row
		#cell: Kiril
		#cell: infra
```

```
#use(std/pipe-table)
#ptable
	Name  | Role  | Since
	Kiril | infra | 2019
	Ann   | web   | 2021
```

### `std/html`

Raw-HTML escape hatch. Non-portable: only the HTML backend handles `raw` nodes for this backend; any other backend errors on them. Bind explicitly with `#use(std/html)`.

```
#use(std/html)
#(html "<strong>verbatim &amp; raw</strong>")
```

## Testing

Tests are golden: each case is a `.yamd` source paired with an expected `.html` (rendered output) and/or `.sexp` (content IR, for pass inspection). At least one expectation file must exist.

**Run:**

```sh
nix develop --command raco test tests/run-golden.rkt
```

Or run the full suite (golden tests + package unit tests):

```sh
nix develop --command raco test tests/ yamd/
```

**Bless** (write or update expectation files):

```sh
nix develop --command racket tests/run-golden.rkt --bless
# or
YAMD_BLESS=1 nix develop --command raco test tests/run-golden.rkt
```

To add a test: write the `.yamd`, create an empty `.html` and/or `.sexp`, bless, then review the diff. A golden you did not read is not a test.

Golden tests live under `tests/golden/<area>/`. Each plugin ships its own golden directory.

## Editor support

A VS Code extension with a TextMate grammar is planned under `editors/vscode/`. It is not yet released.

## Contributing

See [docs/contributing/](docs/contributing/README.md) for dev-environment setup,
building, testing, and the commit/PR workflow. In short: keep the core minimal
(new functionality belongs in plugins) and make sure `nix flake check` passes
before submitting.

## License

GPL-3.0 © 2026 kp2pml30 — see [LICENSE](LICENSE).
