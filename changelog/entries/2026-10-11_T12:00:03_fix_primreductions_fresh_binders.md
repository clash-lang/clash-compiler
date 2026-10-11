---
issues: []
prs: []
---

# FIXED
Fix the reductions of `map`, `zipWith`, `foldr` and `init` on vectors binding the elements of a vector with fixed identifiers, which could capture type variables of the element type.
