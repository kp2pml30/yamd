; YAMD highlights — nvim-treesitter capture names.

; Calls: `#name(args){body}` — the one sigil plus the function name.
(call_name (sigil) @punctuation.special)
(call_name (name) @function.call)

; `#use(...)` is import-like.
((call_name (name) @keyword.import)
 (#eq? @keyword.import "use"))

; Host escape `#(...)` sigil; contents are injected Racket.
(host_escape (sigil) @punctuation.special)

; Markup re-entry `#name{...}` inside host code.
(markup_reentry (sigil) @punctuation.special)
(markup_reentry (name) @function.call)

; Body delimiters and argument parens.
(body (brace_open) @punctuation.bracket)
(body (brace_close) @punctuation.bracket)
(arguments (paren_open) @punctuation.bracket)
(arguments (paren_close) @punctuation.bracket)
(host_escape (paren_open) @punctuation.bracket)
(host_escape (paren_close) @punctuation.bracket)

; `: rest` / `::` body markers.
(body_marker) @punctuation.special

; Comments.
(line_comment) @comment
(block_comment) @comment

; `##` literal hash.
(escape) @string.escape

; Fences `|X{ ... }X|` — raw string content.
(fence_open) @punctuation.special
(fence_close) @punctuation.special
(fence_content) @string.special

; Fallback for host strings when no Racket parser is installed
; (the injection overrides this when active).
(host_string) @string
