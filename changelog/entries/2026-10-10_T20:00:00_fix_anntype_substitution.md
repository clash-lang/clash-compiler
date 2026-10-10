---
issues: []
prs: []
---

# FIXED
Clash no longer fails with "Cannot reduce to an integer" or "Cannot translate domain tag" when a function that is polymorphic in the type of an argument annotated with `Annotate` gets specialized. Type substitution and free variable traversals skipped over `Annotate`d types, so the type variable under the annotation was never instantiated.
