---
issues: [3310]
prs: []
---

# FIXED
Extending arithmetic (`add`, `sub`, and `mul`) on `Index` now handles zero-width
arguments correctly. VHDL subtraction from zero-width `BitVector`, `Signed`, and
`Unsigned` values now negates the widened operand correctly, including boundary
values.
