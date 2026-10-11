---
issues: []
prs: []
---

# FIXED
`collapseRHSNoops` no longer mistakes a local variable for a global binder with the same unique. Before, if that global binder was a no-op, an expression like `map f xs` could be replaced by `xs`, leading to wrong HDL.
