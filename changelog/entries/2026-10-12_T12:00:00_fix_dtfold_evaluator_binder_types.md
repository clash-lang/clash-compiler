---
issues: []
prs: []
---

# FIXED
The evaluator's reduction of `dtfold` gave the binders that select the halves of the vector the type `2^(k-1)` instead of `Vec (2^(k-1)) a`.
