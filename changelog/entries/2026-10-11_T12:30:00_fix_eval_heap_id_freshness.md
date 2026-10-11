---
issues: []
prs: []
---

# FIXED
The compile-time evaluator could give a let-binding it evaluates the same unique as a variable in scope, capturing references to that variable.
