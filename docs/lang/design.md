# YAMD v2 — Design

Indentation-based markup language compiling to an element IR (HTML backend first).
Inspiration, in order: typst, haml, erb; escape philosophy from LaTeX/Scribble.
Per-feature how-to guides with live snippets: [howto/](howto/index.yamd).

Non-goals: being HTML; being tied to Ruby. The host language is pluggable in
principle, but a document is a program in *one* host (planned: Racket).

## Core principles

1. **One sigil.** `#` is the only globally special character. Every other
   symbol (`|`, `*`, `$`, …) is plain text unless a surrounding function's
   reader gives it meaning.
2. **One form per role.** Call, comment, escape — nothing else. Headings,
   lists, links, tables, math are all library functions, not syntax.
3. **Documents are programs.** The sigil escapes from text mode into host
   mode; `{}` escapes back. Turing completeness comes from the host, not from
   a template mini-language.
4. **Two-pass evaluation** (collect → resolve) for cross-references, TOC,
   numbering, glossaries.

## Syntax

### Text mode

Plain text is content. Structure is defined by indentation (tabs). Lines at a
given indent level belong to the enclosing block.

| Form | Meaning |
|------|---------|
| `#name(args)` | Call. `args` is host code (keyword args welcome; no commas — `,` is Racket unquote. `#use` alone tolerates them). |
| `#name(args){body}` | Call with inline body. Body is markup by default; parsed to content and passed as the last argument. Braces must balance. |
| `#name(args): text` | Call with body = rest of line. |
| `#name(args):` at end of line | Call with body = following indented block. The `:` is optional (see the row below); with an inline body a `:` and nothing indented below is literal text, not a body. |
| `#name(args)::` | Call with body = rest of the **enclosing block** (see Hanging blocks). |
| `#name(args){body}` alone on a line | Body = the `{body}` plus the following indented block (the trailing `:` above is optional sugar; `#section{Title}` and `#section{Title}:` are the same form). |
| `#name(args)` alone on a line | Sugar for `#name(args):` — body = following indented block. |
| `#(expr)` | Host escape. Statement position → statement (e.g. `#(define …)`); inline → expression, result converted to content. |
| `##` | Literal `#`. |
| `# ` `#;` / `#;{…}` | Line / block comment. |

`(args)` and `{body}` are each optional. `#name` bare is a zero-arg call.

### Host mode

Inside `#(...)` and `(args)` you are in the host language. `#name{…}` /
`#…{markup}` re-enters text mode, producing a content value (Scribble
semantics). Example:

```
#(for ([x items])
	#p{#(item-name x) — #(item-price x)¥})
```

### Variables, metadata

```
#(define price 42)
The item costs #(price).

#(meta 'date "2025-01-01")   ; writes document state (collect pass)
Published: #(get-meta 'date) ; reads on resolve pass — order-independent
```

Scoping is host scoping; defines inside a block are local to that block.

## Blocks and indentation

```
#list(style: "1")
	#item: First
	#item
		Second, multi-line
		#list(style: "a")
			#item: nested
```

- A block body is the following lines indented one level deeper. A call alone
  on its line takes it — whether the call is bare (`#foo`) or already carries an
  inline body (`#section{Title}`). A trailing `:` is optional sugar: `#foo`,
  `#foo:`, `#section{Title}`, and `#section{Title}:` each take the block the
  same way. (With an inline body, a `:` and *nothing* indented below is literal
  text, not an empty body — so prose like `#code{index.yamd}:` stays intact.)
- Dedent closes the block. Nesting = indentation; no `"-.1"`-style encodings
  needed (a `list` function may still accept them as sugar — its business).
- Indentation is tabs only, single tab per level.

### Hanging blocks (`::`)

`::` folds the *rest of the current block* into the call's body, without
indentation:

```
#section{Motivation}::

Everything until the enclosing dedent/EOF is inside the section.
#note: including this.
```

- Pure reader-level transform; evaluation sees an ordinary body.
- Recursive: a later `::` at the same level nests inside the earlier one.
  Flat LaTeX-style sectioning, if wanted, is implemented by the `section`
  function (level-aware), not by the reader.
- `::` at top level = rest of file (useful for `#template(...)::`).

## Contexts

Host-language tweaks live in **contexts** that are auto-scoped: by default all
settings go to the current context. `#fork-context::` forks the current
context for the rest of the enclosing block; block-bodied calls fork
implicitly where it makes sense. Contexts also drive auto-detection, e.g.
`section` stores the current header level in the context, so nesting depth
determines `<h1>`/`<h2>`/… without explicit levels.

## Body readers (the extensibility primitive)

A function may register a **reader** for its body, consulted *before* parsing:

| Reader | Behavior |
|--------|----------|
| `markup` (default) | Full YAMD parsing. |
| `raw` | Verbatim text. Block form: dedented, terminated only by dedent — no delimiter scanning at all. Inline form: brace-balanced scan, no `#` recognition. |
| custom | Plugin-supplied parser (tables, math notations, …). |

Examples:

```
#code(lang: python)
	def f(): # not a comment, raw
		return {"x": 1}

Inline #tt{x = 1} and math #m{sum_(i=0)^n i^2}.
```

Context-scoped symbols keep the global symbol budget at one.

### Fences for pathological inline content

Inline raw/markup bodies with unbalanced braces use alternative delimiters
(Scribble-style): `|{ … }|`, extensible fence:

```
#tt|{ unbalanced } here }|
#tt|==={ even }| inside }===|
```

Opener `|X{` closes only at `}X|`. Block raw bodies never need this.

## Plugins

- A plugin is a host module exporting functions (+ optional body readers).
- `#use(std/table, std/math: asciimath)` imports/binds; math notation
  (AsciiMath / LaTeX / typst-math) is chosen by the bound plugin — the core
  knows nothing about math.
- Project-level *reader macros* (e.g. `` `…` `` → `#tt{…}`) are allowed as
  opt-in sugar on top of existing functions; never new core forms.

## Tables

Mainly support HTML-like cells:

```
#table(header: 1)
	#row
		#cell: Name
		#cell: Role
	#row{Kiril}{infra}
```

- `#row{a}{b}` positional multi-body form is supported.
- A *source-beautiful* pipe-table plugin (`Name | Role | Since`) exists as a
  separate plugin and is allowed to be pretty dumb.

## Cross-references / glossary (stdlib, via two-pass)

```
#def("vm"){virtual machine}{A sandboxed execution environment.}
…deterministic #ref("vm") semantics…
#glossary
```

`def` records in collect; `ref`/`glossary` resolve in the second pass. Same
machinery serves TOC, numbering, citations, links and backlinks.

## Rendering

Functions emit an **element IR**, not HTML strings. Backends (HTML first)
serialize the IR. Raw-HTML escape hatch is a function in the HTML backend's
namespace, clearly marked non-portable.

## Implementation notes (Racket plan)

- `#lang yamd`: indentation reader on top of at-exp-style parsing.
- Scribble gives at-notation, fences, and the collect/resolve architecture
  nearly for free; write the indentation layer + IR + HTML backend.
- Body-reader dispatch: compile-time table / syntax property on the function,
  consulted by the reader.
- If the host ever changes, the surface syntax and reader design survive;
  only `#(...)` contents and the plugin ABI are host-specific.
- Prefer Typed Racket with type assertions where it makes sense.

## Consciously accepted tradeoffs

- Documents are programs: no sandboxed rendering of untrusted documents;
  incremental/parallel builds are harder.
- One host at a time: a Ruby YAMD and a Racket YAMD would be different
  languages sharing a surface syntax.
- `#` collides with hex colors, "issue #42", shebangs in prose — mitigated by
  `##` and by code living in raw bodies. (If the host were Ruby, `#{}`/`#`
  comment collisions would argue for a different sigil; for Racket it's fine.)

## Resolved questions

- Indentation: tabs only, single tab per level.
- `args` grammar: full host expressions.
- `::`: no "until next same-name `::`" flat variant for now.
- Inline math: `#m{…}` only, no reader-macro shorthand.
- Error reporting: best effort mapping of IR/runtime errors back to source
  lines through the indentation reader.
