---
issues: []
prs: []
---

# FIXED
Clash no longer turns a non-recursive `let` inside the definition of a let-binding into a recursive one when flattening let-expressions. When the binder shadowed a variable used in its own definition, as in `let y = (let x = f x in x) in y`, this produced a combinational loop.
