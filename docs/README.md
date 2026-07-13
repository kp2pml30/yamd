# Documentation

- YAMD language docs: [lang/index.yamd](lang/index.yamd), written in YAMD
  itself: a `spec/` reference plus per-feature guides under `syntax/`,
  `stdlib/` and `cli/`. A doc-set rendered with
  `nix run . -- docs/lang/index.yamd` (add `--mode multi -o out/` for one page
  per feature)
- Usage guide: [usage/](usage/README.md), consuming YAMD from another
  repository: flake input, rendering, formatter as a git hook
- Architecture: [architecture.md](architecture.md), implementation architecture
  (reader, IR, contexts, two-pass, plugin ABI, backends)
- Multi-source documents: [multi-doc.md](multi-doc.md), `#include`, `#toctree`,
  single/multi-file builds
- Contributing guide: [contributing/](contributing/README.md), dev setup,
  building, testing, commit/PR workflow
