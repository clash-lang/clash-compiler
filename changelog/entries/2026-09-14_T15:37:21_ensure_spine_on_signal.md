---
issues: [3432]
prs: []
---

# FIXED
`ensureSpine` on a `Signal` no longer crashes Clash with an internal error. When generating HDL, `ensureSpine` on a `Signal` is the identity function.
