---
issues: []
prs: [3328]
---

# CHANGED
Normalization now knows which `Term` constructors each transformation can fire on, and only offers a node to the transformations that can match it. Measured ~25% faster HDL generation end-to-end on a large industrial design, with identical HDL. Transformations that cannot match a node no longer show up in `-fclash-debug-transformations` and `-fclash-debug-count-transformations` output.
