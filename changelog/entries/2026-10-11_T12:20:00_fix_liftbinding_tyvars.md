---
issues: []
prs: []
---

# FIXED
When Clash lifts a let-binding to a new global function, it no longer captures the free type variables of the binding in a type abstraction inside it, and it abstracts over a type variable only after the type variables in its kind.
