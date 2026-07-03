# Setup

Prerequisite: [Nix](https://nixos.org) with flakes enabled.

From the repo root, enter the dev shell. This also installs the git hooks into
`.git/hooks/` (via `git-hooks.nix`) and writes the gitignored
`.pre-commit-config.yaml`:

```sh
nix develop
```

Link the in-tree `yamd` collection into the active Racket environment so
`(require yamd)` and `racket -l- yamd/cli` resolve:

```sh
raco pkg install --auto --batch --link yamd/
```

You are ready to [build](building/build.md) and [test](testing/README.md).
