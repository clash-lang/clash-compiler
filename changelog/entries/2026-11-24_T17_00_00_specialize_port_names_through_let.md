---
issues: []
prs: [3552]
---

# FIXED
Specialized functions no longer get port names like `c$pTS0` when the original function has a `let` between its lambdas. The ports now keep the names of the function's arguments.
