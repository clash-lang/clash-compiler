---
issues: []
prs: []
---

# FIXED
Clash no longer mixes up type variables when it pushes a cast into the alternatives of a `case` expression whose patterns bind existential type variables, and it now gives the resulting `case` expression the type the cast produces.
