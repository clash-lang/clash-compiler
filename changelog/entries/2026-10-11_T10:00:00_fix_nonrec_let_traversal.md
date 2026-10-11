---
issues: []
prs: []
---

# FIXED
Clash no longer turns a non-recursive `let` into a recursive one while normalizing. When the binder shadowed a variable used in its own definition, as in `let x = f x`, this produced a combinational loop or a hang.
