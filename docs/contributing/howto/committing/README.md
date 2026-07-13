# Committing

## Git Hooks

Hooks are generated from `flake.nix` by
[git-hooks.nix](https://github.com/cachix/git-hooks.nix) — the checked-in
`.pre-commit-config.yaml` is gitignored and regenerated, never edited by hand.
`nix develop` installs them; they also run in CI via `nix flake check`

Run every hook over the whole tree:

```sh
nix fmt
# or, outside the sandbox:
nix develop --command pre-commit run --all-files
```

Hooks: `nixfmt`, trailing-whitespace, end-of-file-fixer, large-file /
merge-conflict / JSON / YAML checks. Byte-exact fixtures (`tests/golden/`) and
the generated tree-sitter `src/` are excluded from the whitespace hooks

## Commit Style

Conventional commits with a trailing gitmoji, e.g.:

```
feat(reader): re-enter host code from markup ✨
fix(typst): resolve cross-page fragment links 🐛
```

Keep the subject imperative and scoped to one change

## Pull Requests

Branch off `main`, keep core changes minimal (new behavior belongs in plugins),
and make sure `nix flake check` passes before opening the PR
