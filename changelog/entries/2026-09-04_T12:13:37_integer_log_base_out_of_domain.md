---
issues: []
prs: [3492]
---

# FIXED
`GHC.Num.Integer.integerLogBase#` and `GHC.Num.Natural.naturalLogBase#` are now constant folded accurately, even in cases where these functions return errors.
