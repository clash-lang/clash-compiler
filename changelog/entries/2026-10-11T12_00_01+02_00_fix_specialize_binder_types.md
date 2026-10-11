---
issues: []
prs: []
---

# FIXED
When specializing a function applied to type arguments, Clash now gives the binders of the specialized function the types of the function's parameters. Previously it used the types of the arguments, which could mention type variables bound at the call site, leaving them free in the specialized function.
