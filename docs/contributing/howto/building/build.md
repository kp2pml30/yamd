# Build

Build the package (compiles the `yamd` collection and wraps the CLI):

```sh
nix build
```

Render a document without installing anything:

```sh
nix run . -- doc.yamd                       # HTML to stdout
nix run . -- doc.yamd --backend typst -o doc.typ
```

Inside the dev shell, after linking the package (see [setup.md](../setup.md)),
run the CLI directly:

```sh
racket -l- yamd/cli doc.yamd --backend html -o doc.html
```
