---
issues: []
prs: []
---

# FIXED
When specializing a function on an argument with free type variables, Clash now binds every type variable after the type variables in its kind. Previously, a kind variable could be bound after a type variable of that kind, leaving it free in the kind.
