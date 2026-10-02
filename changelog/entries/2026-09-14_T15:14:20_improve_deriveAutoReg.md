---
issues: []
prs: [3111]
---

# FIXED
`deriveAutoReg`: improved the calculated constraints

It now looks up the constraints on the superclass (NFDataX) instance and adds them as
constraints on the generated AutoReg instance.
This make it possible to use `deriveAutoReg` on types whose NFDataX instances have
some extra constraints.
