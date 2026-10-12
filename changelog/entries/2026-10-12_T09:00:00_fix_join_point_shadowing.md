---
issues: []
prs: []
---

# FIXED
Clash no longer mistakes a local function for a join point when a variable shadowing it occurs in tail position, as in `let j = (\j -> j) f in j 1`. Clash lifted such functions into separate HDL components instead of inlining them.
