---
issues: []
prs: [3472]
---

# CHANGED
In `clash-lib`, the `inlineWorkFree` transformation now normalizes an application of a function to constant arguments only once, and reuses the result at every place the same application occurs. Before, every occurrence was normalized separately. This speeds up normalization of designs that compute a lot with type-level naturals at compile time, for example through `KnownNat` constraints.
