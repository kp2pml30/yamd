# Formatter Integration

`yamd --fmt` is a conservative, semantics-preserving formatter for `.yamd`
sources (what it normalizes — and deliberately doesn't — is documented in
the language guide's formatting page). It accepts **many files per
invocation**; `--check` reports *every* unformatted file before exiting 1,
and `-o` requires exactly one input

```sh
yamd --fmt a.yamd b.yamd            # rewrite in place
yamd --fmt --check $(git ls-files '*.yamd')   # CI check, exit 1 if any dirty
```

## As a Git Hook (git-hooks.nix)

With `yamd` as a flake input (see [README.md](README.md)):

```nix
pre-commit-check = git-hooks.lib.${system}.run {
  src = ./.;
  hooks.yamd-fmt = {
    enable = true;
    name = "yamd fmt";
    entry = "${yamd.packages.${system}.default}/bin/yamd --fmt --check";
    files = "\\.yamd$";
  };
};
```

Use `--fmt` without `--check` as the entry to rewrite files in place instead
of failing the commit. The same block works for plain pre-commit: put the
`entry`/`files` pair in `.pre-commit-config.yaml` with `language: system`

## Note on `nix fmt`

The YAMD repository's own `formatter` flake output runs its *repo* hooks
(nixfmt, whitespace) — the `nix fmt` convention — not the `.yamd` formatter.
Do the same in your repo: point your `formatter` output at your generated
pre-commit config, and the `yamd-fmt` hook above runs under `nix fmt`
together with your other formatters
