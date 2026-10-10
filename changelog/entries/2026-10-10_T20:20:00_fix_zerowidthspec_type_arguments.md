---
issues: []
prs: []
---

# FIXED
Clash no longer fails with "Unexpected application" when a function takes an argument of a zero-width data type with type arguments, such as `data T n = T (Proxy n)`.
