---
issues: []
prs: [3462]
---

# CHANGED
In `clash-lib`, the `reduceNonRepPrim` transformation is renamed to `reducePrim`, because it replaces primitives for more reasons than non-representable arguments or results. To debug it with `-fclash-debug-transformations`, use the new name.
