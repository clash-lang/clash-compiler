---
issues: []
prs: []
---

# FIXED
Clash no longer fails with "'reduceBindersCleanup' encountered a variable reference that was neither in 'doneInl', 'origInl', or in the transformation's in scope set", or with "Not in normal form: RHS of case-projection is not a variable", for functions that pattern match on the `:-` constructor of `Signal`.
