---
issues: []
prs: []
---

# FIXED
Clash no longer turns a non-recursive `let` into a recursive one when flattening let-expressions or when adding a binder for a function's result. When the binder shadowed a variable used in its own definition, as in `let x = f x`, this produced a combinational loop or a hang.
