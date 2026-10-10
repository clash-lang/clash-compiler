---
issues: []
prs: []
---

# FIXED
Clash installed with `cabal install` now works outside of a project. Before, it failed with errors like ``Could not find module ‘GHC.TypeLits.KnownNat.Solver’`` unless a GHC package environment file provided `clash-prelude` and the type checker plugins. If no package environment provides `clash-prelude`, Clash now uses the package databases it was built against.
