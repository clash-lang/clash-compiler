---
issues: []
prs: []
---

# FIXED
When a local variable is passed as a function argument to a primitive, Clash no longer instantiates the global function that happens to share its unique. It reports an error instead.
