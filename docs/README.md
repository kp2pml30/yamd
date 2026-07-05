# Documentation

- [lang/design.md](lang/design.md) — the YAMD language specification (syntax, semantics, tradeoffs)
- [lang/howto/](lang/howto/index.yamd) — per-feature how-to guides, written in YAMD itself; a doc-set rendered with `nix run . -- docs/lang/howto/index.yamd` (add `--mode multi -o out/` for one page per feature)
- [usage/](usage/README.md) — consuming YAMD from another repository: flake input, rendering, formatter as a git hook
- [architecture.md](architecture.md) — implementation architecture (reader, IR, contexts, two-pass, plugin ABI, backends)
- [multi-doc.md](multi-doc.md) — multi-source documents: `#include`, `#toctree`, single/multi-file builds
- [contributing/](contributing/README.md) — dev setup, building, testing, commit/PR workflow
