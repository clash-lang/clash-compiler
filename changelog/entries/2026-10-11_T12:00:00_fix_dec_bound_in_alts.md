---
issues: []
prs: []
---

# FIXED
Fix `disjointExpressionConsolidation` lifting applications that refer to variables bound inside the alternatives of a case-expression out of those alternatives. This could lead to Clash crashing on free variables, or to subtly wrong HDL output when a pattern binder shadowed a variable bound outside the case-expression.
