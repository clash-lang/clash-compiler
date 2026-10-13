---
issues: [3432]
prs: []
---

# FIXED
`ensureSpine` on `Signal` now applies the sample's instance in HDL, preserving fixed bits in custom representations. Undefined stream recovery in simulation also uses that instance instead of `deepErrorX`, fixing mismatches for function-valued samples.
