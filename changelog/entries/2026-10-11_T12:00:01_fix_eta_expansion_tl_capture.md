---
issues: []
prs: []
---

# FIXED
Fix `etaExpansionTL` moving lambdas out of a let-expression without renaming them. When a lambda binder shadowed a let-binder, or a variable used by a let-binding, this could lead to subtly wrong HDL output.
