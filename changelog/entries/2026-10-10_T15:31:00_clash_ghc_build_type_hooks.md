---
issues: []
prs: []
---

# CHANGED
`clash-ghc` now uses `build-type: Hooks` to record the package databases it is built against. Building it requires cabal-install 3.14 or newer, or Stack 3.9.1 or newer. Stack users on a snapshot older than GHC 9.12 need `Cabal-3.14.2.0`, `Cabal-syntax-3.14.2.0` and `Cabal-hooks-3.14` as `extra-deps`.
