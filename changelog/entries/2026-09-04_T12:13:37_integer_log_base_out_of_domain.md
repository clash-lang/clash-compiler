---
issues: []
prs: [3351]
---

# FIXED
`GHC.Num.Integer.integerLogBase#` and `GHC.Num.Natural.naturalLogBase#` were only constant folded for a positive second argument, even though they are total. The evaluator now uses those functions directly, eliminating any difference in behavior.
