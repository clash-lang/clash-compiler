---
issues: []
prs: []
---

# FIXED
Clash no longer misses free variables of a term when a type variable and a term variable share a unique. A type binder used to hide a free term variable with the same unique, and the other way around, which could lead to variable capture during normalization.
