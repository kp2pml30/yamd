# Using YAMD

How to consume YAMD from another repository. YAMD is packaged as a Nix
flake; no Racket setup is needed on the consumer side.

- [formatter.md](formatter.md) — wiring `yamd --fmt` into a dev shell, git hook, or CI

## One-off rendering

```sh
nix run <yamd-flake> -- page.yamd                     # html to stdout
nix run <yamd-flake> -- page.yamd -o page.html
nix run <yamd-flake> -- book/index.yamd --mode multi -o site/
nix run <yamd-flake> -- page.yamd --backend markdown
```

`<yamd-flake>` is any flake reference: a local checkout path or the
repository URL (`git+https://git.kp2pml30.moe/ya/yamd`).

## As a flake input

```nix
{
  inputs.yamd.url = "git+https://git.kp2pml30.moe/ya/yamd";

  outputs = { self, nixpkgs, yamd, ... }: {
    devShells.x86_64-linux.default = pkgs.mkShell {
      packages = [ yamd.packages.x86_64-linux.default ];
    };
  };
}
```

The package installs one binary, `yamd` — the same CLI as `nix run`:
rendering, build modes (`--mode single|multi`), backends
(`--backend html|markdown|typst`), and the [formatter](formatter.md).

Plugin collections outside the document tree resolve via `PLTCOLLECTS`
(Racket's search path): `PLTCOLLECTS=":/path/to/plugins" yamd doc.yamd`
makes `#use(mycol/mod)` find `/path/to/plugins/mycol/mod.rkt` — the wrapper
appends its own collection without clobbering yours (keep the leading colon;
it preserves Racket's default paths).

## Writing documents

The language itself is documented in [docs/lang/](../lang/design.md); the
per-feature guides live in [docs/lang/howto/](../lang/howto/) as a YAMD
doc-set (render it with `nix run <yamd-flake> -- docs/lang/howto/index.yamd`).
A complete worked example, including a project-local plugin, is under
[examples/](../../examples/README.md).
