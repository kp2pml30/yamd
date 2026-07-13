# Testing

Tests are golden: each case is a `.yamd` source paired with an expected `.html`
(rendered output) and/or `.sexp` (content IR). At least one expectation file
must exist. Golden cases live under `tests/golden/<area>/`

Run the golden suite:

```sh
nix develop --command raco test tests/run-golden.rkt
```

Run everything (golden + package unit tests):

```sh
nix develop --command raco test tests/ yamd/
```

Or run the sandboxed check, which also runs the git hooks:

```sh
nix flake check
```

## Blessing Expectations

Writing or updating expectation files:

```sh
nix develop --command racket tests/run-golden.rkt --bless
# or
YAMD_BLESS=1 nix develop --command raco test tests/run-golden.rkt
```

To add a case: write the `.yamd`, create an empty `.html` and/or `.sexp`, bless,
then review the diff. A golden you did not read is not a test
