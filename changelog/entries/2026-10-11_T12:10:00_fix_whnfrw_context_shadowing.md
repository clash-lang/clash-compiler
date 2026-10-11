---
issues: []
prs: []
---

# FIXED
The compile-time evaluator used let-bindings from the surrounding expression even where a lambda, case alternative, or other binder shadowed the let-binder or a variable its right-hand side refers to. This could result in silently wrong HDL.
