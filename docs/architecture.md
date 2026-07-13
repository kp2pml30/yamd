# YAMD — Implementation Architecture

Concrete architecture for the Racket implementation of [lang/design.yamd](lang/design.yamd).
lang/design.yamd is the language spec; this file decides *how* it is built. Where the
two disagree, lang/design.yamd wins on language semantics, this file wins on code
structure. Skeleton status is marked; every module already has its final
name and location — do not move files.

## Package layout

```
yamd/                      Racket package (raco pkg install --link yamd/)
	info.rkt                 package metadata
	main.rkt                 public API: (require yamd) re-exports everything below
	ir.rkt                   [typed]   element IR: element / raw / delayed / info, Content
	passes.rkt               [typed]   two-pass machinery: current-info, collect!, resolve-content
	decode.rkt               [typed]   flow -> paragraphs (para-break markers, block-tag set)
	backend/html.rkt         [typed]   IR -> HTML string
	build/rewrite.rkt        [typed]   generic builder IR passes (ids, hrefs, heading shift, markers)
	build.rkt                [untyped] doc-set builder: strategies single-file / multi-file
	docset.rkt               [untyped] doc-set registry + embedded-eval dynamic state
	context.rkt              [untyped] auto-scoped context stack
	runtime.rkt              [untyped] support for reader-generated code (yamd-body, ->content)
	eval.rkt                 [untyped] namespace setup, yamd-eval, yamd-file->{content,html}
	reader/reader.rkt        [untyped] the indentation reader (yamd-read-syntax)
	reader/registry.rkt      [untyped] body-reader registry
	lang/reader.rkt          [planned] #lang yamd
	std/base.rkt             [skeleton] emph/strong/tt; planned: para, section, list/item, link
	std/html.rkt             [planned] raw-HTML escape hatch (non-portable, html backend only)
	std/xref.rkt             [planned] def/ref/glossary/backlinks/toc
	std/code.rkt             [planned] code highlighting (raw body reader)
	std/table.rkt            [planned] #table/#row/#cell
	std/pipe-table.rkt       [planned] dumb pipe-table custom body reader
	std/math/asciimath.rkt   [planned] AsciiMath (custom body reader for #m)
	std/include.rkt          #include: eval-time file splice (docs/multi-doc.md)
	std/toctree.rkt          #toctree: doc-set graph in the language (docs/multi-doc.md)
tests/
	run-golden.rkt           golden runner (raco test integration + bless)
	golden/<area>/<name>.yamd + <name>.html and/or <name>.sexp
editors/vscode/            [planned] VS Code extension (TextMate grammar)
```

Core stays minimal: everything under `std/` except `base.rkt` is a plugin in
the ABI sense (own module, own body readers, own golden dir) and must not be
required by core modules.

## Typed / untyped boundary

**Typed** (`typed/racket/base`): `ir.rkt`, `passes.rkt`, `decode.rkt`,
`backend/html.rkt`. These are closed data-model traversals; exhaustive typing
over the `Content` union is exactly what catches missed-node-kind bugs in
every backend/pass, and the types are the documentation for backend authors.

**Untyped**: the reader (`reader/*.rkt`), `context.rkt`, `runtime.rkt`,
`eval.rkt`, all of `std/`. Justification: the reader is port/char plumbing
producing syntax objects and dynamic registry lookups — TR buys little there
and costs a lot of annotation and occurrence-typing friction around
`peek-char`/`read-syntax`. Contexts and stdlib traffic in `Any`-valued host
data and cross into typed code only through `elem`/struct constructors, which
already carry contracts at the typed/untyped boundary. Keep it that way: new
backends and IR passes go on the typed side, new reader features and plugins
on the untyped side.

## Element IR (`yamd/ir.rkt`)

```
Attrs   = (Listof (Pairof Symbol String))
Content = (U String element raw delayed (Listof Content))

(struct element ([tag : Symbol] [attrs : Attrs]
                 [children : (Listof Content)] [loc : (U srcloc False)]))
(struct raw     ([backend : Symbol] [text : String] [block? : Boolean]))
(struct delayed ([proc : (-> info Content)]))
(struct info    ([table : (Mutable-HashTable Any Any)]))
```

- Lists are splices; consumers flatten transparently.
- `raw` is the backend escape hatch; a backend MUST error on raw nodes for a
	different backend, never skip them silently.
- `raw`'s `block?` marks block-level raw content (e.g. `<div>`): `decode`
	treats a block raw like a block element — it breaks the surrounding
	paragraph and is emitted standalone instead of being wrapped in `<p>`
	(inline raw with `block? = #f` still joins the flow). Because the
	struct-type identifier `raw` is a transformer binding, the exported `raw`
	is a *smart constructor* (`(raw backend text [block? #f])`, `block?`
	optional so old 2-arg call sites are unchanged); the struct itself is
	`raw-node` internally with predicate/accessors re-exported under the
	classic `raw?`/`raw-backend`/`raw-text`/`raw-block?` names. `content->sexp`
	keeps the historical `(raw backend text)` shape for inline raw and appends
	a trailing `'block` marker only for block raw, so existing inline goldens
	do not churn. `std/html` exposes both forms: `html` (inline) and
	`html-block` (block).
- `elem` is the convenience constructor: `(elem 'em #:attrs ... #:loc ... child ...)`.
- `content->sexp` is the stable debug/golden view of the IR.
- Tags starting with `%` are reader/decode-internal (only `%para-break` so
	far) and must never reach a backend (the HTML backend errors).
- `#linebreak`/`#pagebreak` (`std/base`) use plain, backend-recognizable
	element tags rather than new IR node kinds. `#linebreak` is `(elem 'br)` —
	inline (not a `decode` block tag), rendered `<br />` (HTML) / CommonMark
	hard break, i.e. `\`+newline (Markdown; the backslash form falls back to
	inline `<br />` in pipe-table cells and headings, where a literal newline is
	destructive) / `#linebreak()` (Typst). `#pagebreak` is
	`(elem 'div #:attrs '((style . "break-after: page")))` — a `div` is a
	`decode` block tag (so it is never `<p>`-wrapped) that HTML emits verbatim
	and Markdown emits as a raw HTML block; the Typst backend pattern-matches
	this exact style and emits `#pagebreak()`. A styled `div` is preferred over
	a dedicated `%`-tag (which backends reject as an internal leak) or a bespoke
	tag like `'pagebreak` (which would serialize as literal `<pagebreak>`).

## The reader (`yamd/reader/reader.rkt`)

`(yamd-read-syntax src in) -> (listof syntax?)` — top-level forms, evaluated
in order by `eval.rkt`. Text mode is the default; `#` escapes to host mode;
`{}` re-enters text mode. Every call form carries the srcloc of its `#`.

Generated code shapes (the contract between reader and runtime):

- text → string literals; each source line ends with a `"\n"` string; each
	blank line (whitespace-only counts as blank) becomes `(para-break)` (a
	`%para-break` element consumed by `decode-flow`). The reader normalizes
	CRLF to LF before scanning.
- `#name(args){b1}{b2}` → `(name arg... body1 body2)` — args spliced, then
	one positional argument per body, each `(yamd-body expr...)` (a Content
	list), wrapped as `(lambda () ...)` when the body reader says `lazy?`.
- `#(expr)` inline → `(->content expr)`; alone on a line → the form itself as
	a top-level statement (this is how `#(define ...)` works).
- host-escape unwrap rule: the parens of `#(...)` delimit host code; exactly
	one datum inside means that datum is the expression (`#(price)` is a
	variable reference), multiple datums form the compound form
	(`#(define price 42)`, `#(item-name x)`). `#()` is an error.
- args keyword sugar: an identifier ending in `:` inside `(args)` becomes a
	keyword — `#list(style: "1")` reads as `(list #:style "1")`. Everything
	else in `(args)` is verbatim host code (full expressions).

Form status: implemented now — plain text, `##`, comments (`#` + space/EOL,
`#;` to EOL, `#;{...}` balanced possibly-multiline), `#(expr)` statement and
inline, `#name`, `#name(args)`, `#name{body}` (markup/raw/custom, nestable,
multi-body, may span lines), `#name(args): rest-of-line`, host-mode markup re-entry (`#name{…}` inside
`(args)` / `#(expr)`, see "Host-mode markup re-entry"), indented block bodies, `::`
hanging blocks, `|{ }|` / `|==={ }===|` fences, `#use`, and definitions inside
`{...}` bodies.

### Indentation algorithm

- Indentation is tabs only, one tab per level. A line starting with spaces
	(before any tab-stripped content) is a reader error. Jumping more than one
	level deeper is a reader error.
- An indented block is legal only as the body of a call that is the *only*
	thing on the preceding line (`#name`, `#name(args)` — bare or args, no
	inline/`: ` body). Anything else followed by a deeper line is an error;
	don't guess.
- Block body dispatch by the call's body reader: `markup` — parse the block
	recursively at depth+1 (paragraph breaks and nested calls work); `raw` —
	take lines verbatim with one leading tab stripped per level until dedent,
	no delimiter scanning at all, blank lines belong to the block; custom —
	gather the same dedented text and hand it to the parser with a srcloc.
- `::` (`#name(args)::` — nothing may follow on the line): reader-level
	transform; parse the *remainder of the enclosing block* (same indent
	level, until dedent/EOF) as the call's body. Recursive: a later `::` at
	the same level ends up nested inside the earlier one, because it captures
	the remainder of the block it now lives in. Top level = rest of file.
- Blank lines never close blocks; only a non-blank line with fewer tabs does.
- **Spaces in raw/custom block bodies.** Leading spaces are a reader error at
	*structure* positions (markup), but a raw/custom block body has no interior
	structure — only the block's own structural tabs. Inside such a body only
	the `strip` leading tabs are peeled; **everything after them is verbatim
	content**, including leading spaces (`#code` bodies indented 4 spaces,
	ASCII art with space gutters) and deeper tabs (which stay literal tabs).
	Dedent is still detected by counting *leading tabs only* (no space error
	there): a line with fewer tabs than the block depth closes the block.
	Consequence for `[tab…][space]content`: at block depth *n*, a line with
	*k*<*n* tabs (e.g. `[tab][space]…` when *n*=2) is a **dedent** — the raw
	block closes, and the enclosing markup context re-scans the line, where the
	leading space is a normal structure-position error; a line with *k*≥*n*
	tabs is a content line whose stripped remainder (leading spaces and all) is
	verbatim. A whitespace-only line is blank regardless of any spaces on it
	(blank lines never close a block); its verbatim content normalizes to empty
	— trailing whitespace on an otherwise-empty line is not preserved. Inline
	raw bodies (`#code{…}`) and fences (`|{…}|`) scan verbatim char-by-char and
	were never affected. Markup block/`: rest-of-line` bodies are unchanged:
	their structure positions still reject leading spaces.

### Fences

`#name|{ ... }|` and `#name|==={ ... }===|`: opener `|X{` (X a possibly-empty
run of `=`) closes only at `}X|`. Available for any inline body kind; raw
scan, no `#` recognition, no brace counting. Block raw bodies never need them.

### Host-mode markup re-entry

Inside host code — the `(args)` of a call and the `#(expr)` escape — `#name`
re-enters text mode and reads as an expression evaluating to Content (DESIGN
"Host mode", Scribble semantics): `#(for/list ([x items]) #p{item #(x)})`.

Supported subset:

- `#name{body}` — a markup body (re-enters text mode; nestable; may span
  lines).
- `#name(args){body}` — args (with the same `key:` keyword sugar as text
  mode) plus one or more markup bodies (`#row{a}{b}`).
- `#name(args)` — args, no body.

The body reader dispatches exactly as in text mode (markup / raw / custom /
lazy), so `#m{…}` in host code still hits the AsciiMath parser, `#code{…}`
its raw reader, etc. Fences `|{ … }|` are accepted as an alternative body
delimiter. Line-oriented body forms (`: rest-of-line`, `::` hanging) are NOT
available in host mode — there is no "rest of line" inside an s-expression.
A **bare** `#name` (no `{` and no `(`) is deliberately not re-entry: in host
mode a zero-arg content call is just `(name)`, and treating `#name` as a call
would collide with `#t`/`#f`/`#px…`.

**Mechanism.** `read-paren-datum` reads the host s-expression under a readtable
that overrides `#` (`host-readtable`). For each `#` the Racket reader meets,
the override either reads a YAMD re-entry call (reusing the same body-reader
machinery as text mode, so raw/custom/lazy bodies dispatch identically) or
hands the form back to Racket's default reader (`read-syntax/recursive` with
the consumed `#` reinserted). References inside a re-entry body resolve against
the *surrounding* host bindings — `#(x)` in the loop above sees the `for/list`
variable — because the escape is read with its real lexical context.

**Collision policy.** The trigger is a name (alphanumeric plus `-`/`/`)
followed by `{`, `(`, or a fence opener `|=*{` — unless the name is
*reserved*, in which case the whole form falls back to Racket. The reserved
check applies before `{` as much as `(`: `{` is a paren-equivalent in
Racket's reader, so `#hash{…}`, `#s{…}`, `#t{a}` are valid Racket. Reserved
names (matched case-insensitively, as Racket accepts e.g. `#Ci(`):

- the Racket reader tokens `t f true false hash hasheq hasheqv hashalw s fl
  fx cs ci reader lang e i x o b d rx px`;
- sized flvector/fxvector literals — `fl`/`fx` followed by digits
  (`#fl3(0.0)`, `#fx2(1)`);
- names that lex as Racket number literals — a number-prefix letter
  (`e i x o b d`) followed by number-ish chars containing at least one
  decimal digit (`#x1F(…)`, `#e5(…)`, `#b101(…)`, `#x1/2(…)`). Best effort:
  a number literal with **no** decimal digit (`#xFF`) or an exotic one
  (`#i+inf.0`) directly followed by `(`/`{` is still misread as re-entry —
  put a space after a number literal before an opener (idiomatic anyway).

Consequences: a host-mode re-entry call cannot target a function whose name
is reserved — write `(name …)` instead. This is a deliberate host/text
asymmetry: in text mode `#x(…)` or `#hash(…)` is an ordinary call (text mode
has no Racket literals), in host mode the same spelling is Racket's. `#use`
is a top-level reader directive, never a host-mode call — the reader rejects
it there explicitly. Anything else after `#` falls back verbatim to Racket:
booleans, chars (`#\c`), keywords (`#:kw`), numbers, regexps (`#px"…"`),
comments (`#| |#`, `#;`), **vectors** (`#(…)` is a Racket vector literal,
*not* a nested host escape — inside host code you are already in host),
hashes, prefab structs, etc.

Re-entry is recognized only in host code proper; a fallback form is read —
including its nested elements — under the default readtable, so inside a
Racket literal (an element of a `#(…)` vector or `#hash(…)`) `#name{}` is
read by Racket, not as re-entry: a self-quoting literal must not embed an
unevaluated call datum. Corollary: `#;` before a re-entry form in host code
is a read error (the commented datum is read by Racket) — use `#|…|#`.
Symmetrically, custom body parsers run under the default readtable
(`call-custom-parser`), so a parser that itself calls `read` never observes
the overridden `#`.

## Body-reader dispatch (`yamd/reader/registry.rkt`)

Decision: **runtime registry consulted at read time**, not syntax properties
— the reader runs before expansion, so binding-attached properties are not
visible to it.

```
(struct body-reader (parse lazy?))
;; parse : 'markup | 'raw | (-> string? srcloc? syntax?)
(current-body-readers)              ; symbol -> body-reader, per-file parameter
(lookup-body-reader name)           ; default: (body-reader 'markup #f)
(merge-body-readers! table)
```

A custom parser gets the body text (inline: brace-balanced scan; block:
dedented) plus its srcloc, and returns the syntax of the expression passed as
that body argument. `lazy?` makes the reader wrap the body expression in
`(lambda () ...)` so the callee controls evaluation order (see Contexts).

Plugins contribute entries via a `yamd-reader` **submodule** exporting
`body-readers : (hash/c symbol? body-reader?)`. When the reader sees
`#use(mod ...)` it (1) emits `(yamd-use 'mod ...)` — a
runtime function in `runtime.rkt` that `namespace-require`s the module into
the document namespace — and (2) `dynamic-require`s `(submod mod
yamd-reader)` if declared and merges its `body-readers` into
`current-body-readers` for the rest of the file. Module path mapping in
`#use`: `std/x` → `yamd/std/x`; `mod: variant` sugar → `mod/variant` (so
`std/math: asciimath` → `yamd/std/math/asciimath`); strings are file paths
relative to the document; any other symbol is used as a collection path
verbatim. `eval.rkt`'s entry points parameterize `current-body-readers` with
the merged tables of the default modules (`yamd/std/base`) before reading.

## Contexts (`yamd/context.rkt`)

A context is a chain of mutable frames in a parameter.

```
(context-ref key [default])         ; search frames innermost-out
(context-set! key val)              ; write the TOPMOST frame (shadow, never clobber)
(context-update! key f [default])   ; ref through the chain, write topmost
(with-forked-context body ...)      ; push a fresh frame for the dynamic extent
(call-with-forked-context thunk)
```

**When forks happen** — exactly two ways, the reader itself never forks:

1. `#fork-context::` — an ordinary stdlib function registered with
	`lazy? = #t`, implemented as `(define (fork-context body) (with-forked-context (body)))`.
2. Functions that want their body under a fork register `lazy?` and force
	the thunk inside `with-forked-context`. Because contexts are dynamically
	scoped, the fork is visible to everything evaluated by the thunk.

Header auto-detection lives here: `section` is registered lazy; it does
`(with-forked-context (context-update! 'header-depth add1 0) (body))` and
emits `h1`…`h6` from the depth. Nesting sections forks per level; the fork
dies with the block. Sharp edge to respect: `delayed` procs run *after* all
contexts are gone — capture any context values you need at creation time,
never call `context-ref` inside a delayed proc.

**Section info for `#toc`** (see `std/base.rkt`): at
collect time each `section` records itself under the info key `'sections`.
The value is a list of entries accumulated newest-first (via `info-add!`), so
a consumer `(reverse …)`s for document order. Each entry is
`(list depth anchor title)` — `depth` an integer 1–6 (heading level),
`anchor` the string `id` of the emitted heading (link target; prefix `#` for
an href), `title` the heading's `Content`. `section` also emits the heading
with that `id`; anchors are `"sec-<n>"` (per-document counter under private
key `'yamd-section-counter`). Depth is captured directly (section runs in
pass 1, not in a delayed proc), per the sharp edge above.

## Two-pass evaluation (`yamd/passes.rkt`)

Own machinery, no scribble dependency. **Pass 1 (collect) is document
evaluation itself**: `eval.rkt` creates a fresh `info` table, parameterizes
`current-info`, and evaluates the reader's forms in order. Functions call
`(collect! key val)` / `(info-add! i key val)` (multi-valued keys: backlinks,
glossary, TOC) and return IR that may contain `delayed` nodes. **Pass 2**:
`(resolve-content content info)` walks the IR replacing each delayed node
with `((delayed-proc d) info)`, recursively, fuel-limited (a delayed proc
returning delayed nodes forever is an error, not a hang). Backends only ever
see resolved IR — `content->html` errors on a delayed node.

## Evaluation pipeline (`yamd/eval.rkt`)

read → eval(=collect) → resolve → decode → render. Resolve happens BEFORE
decode so delayed nodes that resolve to block-level content (toc, glossary)
participate in paragraph grouping instead of being trapped inside a `<p>`.

- `make-yamd-namespace` — fresh base namespace with `yamd/runtime` (and by
	default `yamd/std/base`) *attached* from this instantiation, so IR struct
	identities are shared between document code, plugins, and the caller.
- `yamd-eval stxs` → `(values content info)` — un-resolved, un-decoded;
	applies `->content` to each top-level result (void vanishes —
	statements). Runs under `call-with-fresh-context`, so context state never
	leaks between documents.
- `yamd-file->content` (resolved + decoded IR), `yamd-file->html`,
	`yamd-string->html`.
- `decode-flow` groups inline runs into `p` elements, splitting at
	`%para-break` markers and at block-level elements (`block-tag?` set in
	`decode.rkt`); whitespace at paragraph edges is trimmed.
- `yamd-body` (in `runtime.rkt`) turns a parsed body's expressions into one
	Content list. `yamd-body` is implemented as a splicing macro that hoists
	`define-values` and collects expression values, so block-local `#(define ...)`
	works per DESIGN "defines inside a block are local to that block".

### `#lang yamd` (`yamd/lang/reader.rkt`, `yamd/lang/expander.rkt`)

The API (`yamd-file->html`) ships first — golden tests run end-to-end without
module-instantiation plumbing. The `#lang` is a thin layer over the same
pipeline. `yamd/lang/reader` is the `#lang` reader; `yamd/lang/expander` is the
module language. `read-syntax` emits `(module <name> yamd/lang/expander
<doc-name> <source-text>)` and the expander's `#%module-begin` provides `doc`
(resolved + decoded Content) and defines a `main` submodule that renders it to
HTML on stdout (so `racket file.rkt` prints the document).

**Chosen approach — capture, don't embed (the deliberate tradeoff).** The
reader captures the raw document text verbatim and the expander hands it to the
ordinary string pipeline (`yamd-string->content`) at module-instantiation time,
rather than embedding the indentation reader's forms into the compiled module.
The forms cannot be embedded directly because `#use(...)` performs a runtime
`namespace-require` into the fresh document namespace that `eval.rkt` builds —
that only works inside `make-yamd-namespace`, not at module-expansion time.
Consequence: a `#lang yamd` document is *not* compiled to Racket code; it is
re-parsed and re-evaluated in a fresh namespace every time the module is
instantiated. The upside is that `#use`, plugins, contexts and two-pass behave
*identically* to `yamd-string->html` — one code path, no divergence.

Srclocs stay aligned with the source: the port handed to the reader sits just
after `#lang yamd`, so the leading newline is preserved and the first content
line is line 2 in both the file and the captured string; reader/runtime errors
report `<path>:line:col` against the original file. Nothing else may depend on
`yamd/lang/*`; it is sugar over `eval.rkt`.

### CLI (`yamd/cli.rkt`)

`racket -l- yamd/cli file.yamd [--backend html|markdown|typst] [-o out.file]`
(default backend `html`, default output stdout). Backends live in a table
name → `(module-path function)`, `dynamic-require`d only at dispatch time, so
the markdown/typst backend modules need not exist unless selected: `html` →
`(yamd/backend/html content->html)`, `markdown` →
`(yamd/backend/markdown content->markdown)`, `typst` →
`(yamd/backend/typst content->typst)`. Each function takes resolved Content
(from `yamd-file->content`). Unknown backend and missing input file produce a
clean `yamd: …` message on stderr with exit 1 (no stack trace). `command-line`
stops flag parsing at the first positional argument, so argv is permuted to let
flags follow the input file per the documented usage.

`--mode single|multi` (default `single`) selects a build strategy from a
mode table analogous to the backend table; the CLI always routes through
`build-docset`, which for a toctree-less document in single mode is
step-for-step the historical pipeline (same bytes). `multi` requires `-o
<directory>` and writes one page per source document, subdirectories
preserved. Backend table entries carry the output extension (`.html`,
`.md`, `.typ`) for multi-file page naming.

`--fmt` (optionally with `--check`) routes to the source formatter instead of
a backend: `yamd --fmt file.yamd` rewrites in place, `-o out` writes elsewhere,
`--fmt --check` writes nothing and exits 1 when the file is not already
formatted (CI-friendly). Documents that do not parse are reported and never
rewritten.

### Source formatter (`yamd/fmt.rkt`)

Conservative, semantics-preserving formatting for `.yamd`. YAMD is
whitespace-/indentation-sensitive (tabs are structural, raw and custom bodies
are verbatim, blank lines are paragraph breaks, and inside an inline `{...}`
body or a `|{...}|` fence a blank line is *literal*), so the formatter changes
very little and *proves* each change safe rather than trusting line heuristics.

**The reader is the oracle.** `format-yamd` parses the original and each
candidate with the real `yamd/reader`, and accepts a candidate only if it reads
to a *canonically equivalent* list of top-level forms. The single
canonicalization — collapsing runs of consecutive `(para-break)` forms and
dropping para-breaks at the *document* edges — is render-neutral because
`decode-flow` groups any run of ≥1 blank line into exactly one paragraph split
and trims the top-level edges (edge para-breaks are dropped only at top level,
never inside a nested body, where presence flips `<p>`-wrapping). Any edit that
changes a real string, a call, or host code changes the datum and is therefore
rejected; a small fallback ladder then tries less-normalizing candidates, so
correctness never depends on the line pass being clever.

Normalizations (all gated): CRLF→LF; whitespace-only lines emptied; runs of 3+
blank lines → 2 outside verbatim regions; final newline guaranteed, with a
trailing run of blank lines collapsed to *one blank line, never zero* — full
stripping would be safe standalone but changes the render of a file spliced
mid-flow by `#include` (a trailing blank there is a real paragraph separator),
and the formatter cannot know a file is a fragment, so it preserves presence
and normalizes only multiplicity; the space after a leading call's `:` body
introducer → one space. Verbatim regions are located from the reader's own
srclocs — any top-level form whose datum carries a multi-line string literal
has its whole span protected (covers raw/inline-raw/fence/multi-line-markup
bodies; deliberately over-approximate — a `::` section containing one
multi-line inline body keeps all its interior blank runs). **Deliberately NOT
done:** stripping trailing whitespace on content lines — in YAMD a text line's
trailing spaces are literal output (`"hello   \n"` and `"hello\n"` render to
different bytes), so the usual "strip trailing whitespace" rule is *unsafe*
here and only whitespace-only lines are touched. Known dumbness: custom body
readers that discard their source text (e.g. a math parser) are not detected
as verbatim by the srcloc scan, so a blank-line-collapse inside such a
multi-line block relies on the oracle — when the custom parse is insensitive
to the blanks the collapse is accepted, and when it is sensitive the oracle
rejects it and the ladder falls back to leaving that document's interior
blanks alone.

## Multi-source documents

Full design with rationale: [docs/multi-doc.md](docs/multi-doc.md). Summary
of what was added, all additive — single-document behavior is unchanged:

- **`runtime.rkt`**: `current-yamd-source` — the file being evaluated
	(complete simplified path; #f for string-fed docs). Bound by the file
	entry points and per embedded child; relative `#include`/`#toctree`
	paths resolve against it. Also: `yamd-use` now instantiates plugins
	ONCE, in yamd/runtime's home registry, and *attaches* them into the
	requiring namespace — so every document of a set (each evaluated in its
	own fresh namespace) shares one instantiation per plugin, keeping
	collected struct values (e.g. xref's `xdef`) recognizable across files.
	Consequence: plugin module-level state is process-global; per-document
	state belongs in info/context (already the rule).
- **`eval.rkt`**: `yamd-eval-embedded` — evaluate another file *within the
	current collect pass*: shared `current-info`, fresh namespace, forked
	context (optionally with `'header-depth` reset for toctree children),
	child-file srclocs, cycle check. `make-yamd-namespace` additionally
	attaches (without requiring) `yamd/docset` and `yamd/eval` so plugin
	instantiations share their dynamic state with the builder.
- **`docset.rkt`**: the doc-set registry — `doc-record`s (id = source path,
	unresolved content, title, 'sections range, base depth, children),
	toctree records, parent→children edges — *keyed by* the shared info
	instance in a module-level weak table, deliberately NOT inside the info
	table: the typed table's `Any` boundary would wrap the delayed procs
	inside stored Content opaque. Also owns `current-docset-stack` (cycle
	chain), `current-docset-parent` (the enclosing *page*, not rebound by
	`#include`), and the `%toctree` marker contract shared by plugin and
	strategies.
- **`build/rewrite.rkt`** [typed]: generic IR passes — `content-ids`,
	`rewrite-hrefs`, `shift-headings`, `replace-tagged`.
- **`build.rkt`**: `evaluate-docset` (collect over the whole set) and
	`build-docset` = strategy applied to (docset, backend). Strategies are
	`build-strategy` structs returning `(listof build-output)`;
	`single-file-strategy` inlines toctree children (headings and 'sections
	entries shifted by the toctree site's header depth);
	`make-multi-file-strategy` resolves per page, replaces markers with
	toctree navs, builds an anchor→page registry from resolved IR and
	rewrites cross-page `#anchor` hrefs to relative `page.ext#anchor`
	(backend-agnostic — backends untouched), with prev/next/up navigation
	via the overridable `#:wrap-page` hook.
- **Plugins**: `std/include.rkt` (`#include` = eval-time splice, inherits
	header depth) and `std/toctree.rkt` (raw body, one child path per line;
	registers children, emits the `%toctree` marker). Rendering a toctree
	doc without the builder fails loudly on the leaked `%`-tag.
- **Tests**: `tests/golden/multidoc/` (includes; the golden runner skips
	`_`-prefixed `.yamd` — auxiliary sources included by a test root) and
	`tests/multidoc.rkt` (builder + CLI over `tests/fixtures/multidoc/`).

Known limitation (deliberate, see docs/multi-doc.md): markdown/typst
backends do not emit element `id`s as link *targets*, so cross-page
fragment links in those backends may need backend-specific touch-ups.

## Plugin ABI

A plugin is a Racket module that:

1. exports ordinary functions: `(fn arg-or-#:kw ... body ...)` — each body a
	Content list (or a thunk of one if registered lazy); returns Content;
2. optionally declares a `yamd-reader` submodule exporting `body-readers`;
3. calls `collect!`/`info-add!` and returns `delayed` nodes for two-pass
	behavior;
4. ships golden tests under `tests/golden/<plugin>/`.

Documents get it via `#use(...)`; nothing in core may require a plugin
(`std/base` is the only module attached by default).

## Error reporting

Best effort, three layers: (1) reader errors report `src:line:col` from the
port; (2) every call syntax carries the srcloc of its `#`, so
expansion/compile errors point at the .yamd source; (3) `eval.rkt` wraps each
top-level form's evaluation and prefixes runtime exception messages with the
form's srcloc. Custom body parsers receive a srcloc and must use it in their
errors. `element` carries an optional `loc` for backend-time errors.

## Golden test harness

- Layout: `tests/golden/<area>/<name>.yamd` + `<name>.html` (rendered
	output) and/or `<name>.sexp` (`content->sexp` of resolved IR,
	pretty-printed, write mode). Both may exist; at least one must, an
	expectation-less `.yamd` is an error. Exception: a `_`-prefixed `.yamd`
	is an auxiliary source (e.g. `#include`d by a test root), skipped by the
	runner and needing no expectation.
- Run: `nix develop --command raco test tests/run-golden.rkt` (also runs
	under plain `raco test tests/`).
- Bless: `nix develop --command racket tests/run-golden.rkt --bless` or
	`YAMD_BLESS=1 nix develop --command raco test tests/run-golden.rkt`.
	New test: write the `.yamd`, create an empty `.html`/`.sexp`, bless,
	*review the diff* — a golden you did not read is not a test.
- `flake.nix` `checks.tests` runs `raco test tests/ yamd/` after a link
	install; keep both green.
