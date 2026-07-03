# YAMD — Neovim support

Minimal filetype plugin plus setup instructions for the tree-sitter grammar at
[`../tree-sitter-yamd/`](../tree-sitter-yamd/README.md).

## 1. Install this plugin directory

It only contains `ftdetect/` (maps `*.yamd` → filetype `yamd`) and
`ftplugin/` (tabs-only indent: `noexpandtab`, `tabstop=2`). Any plugin
manager works; with lazy.nvim:

```lua
{ dir = "/path/to/yamd/editors/nvim" }
```

or without a manager:

```vim
set runtimepath+=/path/to/yamd/editors/nvim
```

## 2. Register the parser with nvim-treesitter

Point nvim-treesitter at the local grammar and install it (requires a C
compiler; the parser is compiled from `grammar.js`'s generated `src/`):

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.yamd = {
	install_info = {
		url = "/path/to/yamd/editors/tree-sitter-yamd", -- local path (or git URL)
		files = { "src/parser.c", "src/scanner.c" },
		generate_requires_npm = false,
		requires_generate_from_grammar = false,
	},
	filetype = "yamd",
}
```

Then run `:TSInstall yamd`.

> nvim-treesitter `main` branch instead uses
> `require("nvim-treesitter.config").add()`-style registration; the
> `install_info` table is the same.

## 3. Queries

nvim-treesitter looks for queries in its own `queries/yamd/` runtime path.
Symlink them from the grammar so they stay in sync:

```sh
mkdir -p ~/.config/nvim/queries/yamd
ln -s /path/to/yamd/editors/tree-sitter-yamd/queries/highlights.scm \
	~/.config/nvim/queries/yamd/highlights.scm
ln -s /path/to/yamd/editors/tree-sitter-yamd/queries/injections.scm \
	~/.config/nvim/queries/yamd/injections.scm
```

Host code inside `#(...)` and `(args)` is injected as **racket**; install the
racket parser too for embedded highlighting (`:TSInstall racket`), otherwise
host code is left plain (harmless).

## 4. Enable highlighting

```lua
vim.api.nvim_create_autocmd("FileType", {
	pattern = "yamd",
	callback = function()
		vim.treesitter.start()
	end,
})
```

(or `highlight = { enable = true }` in your nvim-treesitter setup on the
`master` branch.)

## Other editors

The same grammar + queries serve Helix (`languages.toml` grammar entry), Zed
(extension with `grammars/yamd`), and Emacs 29+ (`treesit`), each consuming
`tree-sitter-yamd` directly. VS Code intentionally stays on the TextMate
grammar in [`../vscode/`](../vscode/README.md) — see the tree-sitter README
for the rationale.
