# tree-sitter-yamd

Tree-sitter grammar for [YAMD](../../docs/lang/design.yamd), covering the **reader-level
forms only**: call forms, host escapes, comments, fences, and plain text.
Plugin-specific body syntax (math, pipe tables, …) is deliberately not modeled.

This one grammar serves every tree-sitter-based editor: **Neovim** (see
[`../nvim/`](../nvim/README.md)), and — with the usual per-editor language
config — Helix, Zed, and Emacs (`treesit`). **VS Code keeps its TextMate
grammar** at [`../vscode/`](../vscode/README.md): VS Code has no first-class
tree-sitter integration, and the static TextMate extension needs no build step.

## Layout

- `grammar.js` — the grammar; `src/parser.c` is generated from it (committed,
  as is conventional for tree-sitter repos; regenerate after grammar edits).
- `src/scanner.c` — external scanner: INDENT/DEDENT/NEWLINE for tabs-only
  indentation, `|X{ … }X|` fences, `#;{ … }` nested block comments.
- `queries/highlights.scm`, `queries/injections.scm` — nvim-scheme captures
  and the Racket injection (`.scm` is the tree-sitter/nvim convention).
- `test/corpus/*.txt` — corpus tests (38 cases), mirroring
  `tests/golden/reader/` and `editors/vscode/samples/`.

## Build & test

The tree-sitter CLI needs nodejs and a C compiler. This repo's `flake.nix` does
not carry these dependencies; use an ad-hoc shell:

```
nix shell nixpkgs#tree-sitter nixpkgs#nodejs nixpkgs#gcc -c tree-sitter generate
nix shell nixpkgs#tree-sitter nixpkgs#nodejs nixpkgs#gcc -c tree-sitter test
nix shell nixpkgs#tree-sitter nixpkgs#nodejs nixpkgs#gcc -c tree-sitter parse ../vscode/samples/showcase.yamd
```

(If a future devshell grows `tree-sitter` + `gcc` + `nodejs`, `nix develop`
works too.)

## Scanner design

Tabs-only indentation cannot be expressed in the grammar DSL, so an external
scanner (`src/scanner.c`) produces synthetic tokens:

- **`NEWLINE`** consumes the line break, any following blank lines, and the
  next content line's leading tabs, recording that width as *pending*. Blank
  lines therefore never close a block (docs/lang/design.yamd).
- **`INDENT` / `DEDENT`** reconcile the pending width against a stack of open
  levels — one token per level, so dedenting two levels emits two `DEDENT`s.
  At EOF, remaining levels are flushed, and a zero-width `NEWLINE` is emitted
  once for files without a final newline.
- **Fences**: `FENCE_OPEN` matches `|` + `=`-run + `{` and stores the run
  length in scanner state; `FENCE_CONTENT` scans raw text (no `#`, no brace
  counting) up to the matching `}` + same-length `=`-run + `|`; `FENCE_CLOSE`
  consumes that closer. Lookalikes with a shorter run (`}| ` inside `|=={…`)
  are content, per docs/lang/design.yamd.
- **`BLOCK_COMMENT`**: `#;{ … }` with nested-brace counting, multi-line.

State (indent stack, pending width, open-fence `=`-count, EOF flag) is
serialized for incremental parsing.

## Precise vs approximate vs opaque

**Precise** — matches the reader spec exactly:

- Call forms: `#name`, `#name(args)`, inline `{body}` incl. multi-body
  (`#row{a}{b}`), `: rest-of-line` (only after a call — a prose colon is
  text), `::` hanging blocks (recursive nesting of two `::` at the same
  level; stops at the enclosing dedent), indented block bodies.
- Host escapes `#(...)` and `(args)`: delimiter-balanced with string
  isolation and `#name{…}` markup re-entry, spanning lines. Contents are not
  parsed as Racket here — they are exposed as a `host` node for injection.
- `##`, `#;` / hash-space line comments, `#;{…}` nested block comments,
  fences with `=`-run matching, blank-line handling around blocks.

**Approximate**:

- **Raw and custom reader bodies are parsed as markup.** The reader's
  body-reader dispatch depends on runtime `#use` state, which a syntax
  grammar cannot know. Inside a `#code`/`#html` block, a `#` may therefore be
  colored as a call/comment even though the reader treats it verbatim. The
  grammar is engineered to never *error* on such bodies (stray `)`, `}`, `:`
  degrade to plain text), so the damage is cosmetic mis-highlighting only.
- **Racket injection boundaries** are delimiter-based, not lexical: a `(`
  inside a Racket *string* is handled, but exotic reader syntax containing
  unbalanced delimiters (e.g. `#\(` character literals) would confuse the
  balance. Fine in practice for args and typical escapes.
- A lone unmatched `{` in prose (without a closing `}` anywhere after) is a
  parse error to end of scope; balanced `{…}` prose groups are fine. The
  actual reader is stricter here anyway.
- Space-indented lines are tolerated (treated as content) rather than
  rejected; the reader raises an error, but an editor grammar should keep
  highlighting.

**Opaque** — deliberately unmodeled: everything a plugin's custom body reader
would do (AsciiMath, pipe tables, …), and Racket itself (delegated to an
injected `racket` parser via `queries/injections.scm`).

## Queries

`queries/highlights.scm` uses nvim-treesitter capture names: `@function.call`
for call names, `@keyword.import` for `#use`, `@punctuation.special` for the
sigil and `:`/`::` markers, `@comment`, `@string.special` for fence content,
`@string.escape` for `##`. `queries/injections.scm` injects `racket` into all
`host` nodes (combined, so cross-escape defines highlight consistently).
