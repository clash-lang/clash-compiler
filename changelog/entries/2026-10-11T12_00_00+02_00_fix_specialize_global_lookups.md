---
issues: []
prs: []
---

# FIXED
Specialization no longer confuses a local variable with a global function that has the same unique. Previously, a local function could be specialized using the body of the global function, or a local function argument could be replaced by that global function's body, silently changing the generated HDL. Clash also no longer eta-expands a local variable as if it were a function with a `Synthesize` annotation.
