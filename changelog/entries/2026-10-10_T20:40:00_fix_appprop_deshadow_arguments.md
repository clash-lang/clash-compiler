---
issues: []
prs: []
---

# FIXED
Fix accidental free variable capture in `appProp` when an argument it substitutes binds a variable that occurs free in a later argument, which could lead to subtly wrong HDL output
