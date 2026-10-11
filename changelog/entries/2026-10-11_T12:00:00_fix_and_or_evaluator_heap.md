---
issues: []
prs: []
---

# FIXED
The compile-time evaluator's rules for `&&` and `||` dropped the let-bindings it made while evaluating their arguments. This could result in Clash errors about free variables, or silently wrong HDL.
