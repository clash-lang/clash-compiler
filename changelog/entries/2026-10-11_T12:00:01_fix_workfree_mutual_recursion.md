---
issues: []
prs: []
---

# FIXED
`isWorkFree` no longer loops on mutually recursive global binders. It now considers them not work free, like self-recursive binders.
