---
issues: []
prs: []
---

# FIXED
Clash no longer risks giving the binder that `splitCastWork` introduces the same unique as another binder of the same `let`, which would make references to either binder ambiguous.
