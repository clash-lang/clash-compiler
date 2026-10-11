---
issues: []
prs: []
---

# FIXED
Clash no longer replaces a local variable by a global function that has the same unique during substitution. This could happen when Clash lifted a let-binding to a new global function, which then referenced itself instead of one of its arguments, leading to wrong HDL output.
