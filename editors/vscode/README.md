# YAMD — VS Code extension

Syntax highlighting for [YAMD](../../docs/lang/design.yamd), an indentation-based markup
language that compiles to an element IR. This is a **static** extension: no
build step, no `node_modules`, no activation code. It contributes a language
definition, a TextMate grammar, and a language configuration only.

## Install (from source)

Symlink or copy this directory into your VS Code extensions folder:

```
ln -s "$PWD/editors/vscode" ~/.vscode/extensions/yamd
```

Reload VS Code. `.yamd` files light up. (`.vscode/` at the repo root is
gitignored; the extension deliberately lives at `editors/vscode/`.)

## What is highlighted

Driven by `syntaxes/yamd.tmLanguage.json` (scope `source.yamd`). YAMD has
**one sigil**, `#`; every construct starts there.

| Construct | Example | Scope highlight |
|-----------|---------|-----------------|
| Call name | `#section`, `#fork-context` | `entity.name.function` |
| Call with args | `#list(style: "1")` | args as embedded Racket |
| Host escape | `#(define price 42)`, `#(price)` | embedded Racket |
| Inline body | `#emph{text}`, `#row{a}{b}` | recursive markup |
| Rest-of-line body | `#note: text` | `:` as body separator |
| Hanging block | `#section{…}::` | `::` as body separator |
| Fences | `#tt\|{ }\|`, `#tt\|==={ }===\|` | raw string content |
| Literal hash | `##`, `##42` | `constant.character.escape` |
| Line comment | `#; …`, `# …` (hash-space) | `comment.line` |
| Block comment | `#;{ … { nested } … }` | `comment.block` (brace-balanced) |
| Keyword args | `style:` inside `(…)` | `entity.other.attribute-name` |

### Embedded Racket

`(args)` and `#(…)` host code, plus `#name{…}` markup re-entry inside host
code, are scoped `meta.embedded.block.racket` and the grammar `include`s
`source.racket`. If you have a Racket grammar installed (e.g. *Magic Racket*,
which provides `source.racket`), host code gets full Racket highlighting; the
`package.json` maps the embedded scope to the `racket` language for
bracket/comment behavior. **Without** a Racket extension installed the include
is a harmless no-op — a built-in fallback still colors strings, `;` and
`#|…|#` comments, `#t`/`#f`/`#:kw`/`#\c` literals, and balances nested
parens/brackets so an inner `)` never ends the argument list early.

## Approximations (read this)

TextMate grammars are per-line regex matchers with no indentation model, so a
few things are matched heuristically. None of these break a valid document;
they can only mis-color an unusual construct.

1. **Indentation blocks are not parsed.** The grammar does not know that the
   lines indented under a bare `#name` are its body. Each line is highlighted
   independently; nested `#calls` inside a block still light up correctly
   because the same patterns apply everywhere. Editor auto-indent after a
   block-opening call is provided on a best-effort basis via
   `language-configuration.json` (tabs only), and dedent is manual.
2. **Body/separator anchoring.** A `{…}` body is recognized only when the `{`
   directly follows a `)`, a `}`, or an identifier char (i.e. attached to a
   call or a preceding body); a stray `{` in prose is treated as balanced
   plain text, not a body. A `: ` body separator is recognized only right
   after a call name (`#note: …`) or after `)`/`}` (`#f(x): …`) — so an
   ordinary prose colon (`Name: value`) is *not* mistaken for a body. `::` is
   anchored the same way.
3. **Call names are simple identifiers** (`[A-Za-z][A-Za-z0-9_-]*`). Exotic
   Racket-style names using `!?*+/.<>=` as a *call name* are not recognized as
   a call (they are still valid inside host code). `.`/`/` are excluded from
   the name so a sentence-ending `#ref.` colors only `ref`.
4. **`#t`/`#f`/`#:kw` inside host code** are matched by a small Racket-literal
   fallback so their `#` is not mistaken for the YAMD sigil. Rarer Racket
   reader syntax (`#(…)` vectors, `#hash…`, `#rx…`) may color imperfectly when
   no `source.racket` grammar is installed.
5. **Fences and block comments balance via regex**: `|X{ … }X|` closes on the
   matching `=`-run (backreference); `#;{ … }` balances nested `{}`. A fence
   left unterminated on the line/file colors to end-of-file, as TextMate
   begin/end blocks do.

## Testing

There is no golden runner for the extension. `samples/` holds `.yamd` fixtures
(`showcase.yamd`, `edges.yamd`) exercising every construct above; open them in
a dev host to eyeball the highlighting.

The grammar was validated offline by (a) JSON validity (`python3 -m
json.tool`) and (b) compiling and spot-matching every `begin`/`end`/`match`
regex against representative inputs, including the prose-colon safety case and
the fence backreference. `npx vscode-tmgrammar-test`-style tokenization tests
were **not** run because Node is unavailable in this environment; the coverage
here is manual regex reasoning plus the sample fixtures.

## Files

- `package.json` — language + grammar + config contributions, embedded-Racket
  mapping, tab defaults for `[yamd]`.
- `syntaxes/yamd.tmLanguage.json` — the TextMate grammar (`source.yamd`).
- `language-configuration.json` — brackets, comments, tab indentation rules.
- `samples/` — manual highlighting fixtures.
