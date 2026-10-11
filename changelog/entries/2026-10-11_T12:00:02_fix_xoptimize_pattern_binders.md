---
issues: []
prs: []
---

# FIXED
Fix aggressive X-optimization (`-fclash-aggressive-x-optimization`) turning the binders of a pattern into let-binders without renaming them, and leaving the existential type variables of the pattern unbound. This could lead to Clash crashing, or to subtly wrong HDL output when a pattern binder shadowed a variable used in the subject of the case-expression.
