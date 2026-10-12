---
issues: []
prs: []
---

# FIXED
`disjointExpressionConsolidation` no longer gives a lifted argument a type that mentions an existential type variable bound by a pattern inside the case-alternatives. This could leave that type variable free outside of its pattern.
