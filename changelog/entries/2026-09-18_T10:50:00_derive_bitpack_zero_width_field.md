---
issues: []
prs: []
---

# FIXED
`deriveBitPack` no longer fails with `Unexpected empty list of intervals` on a
field that a custom bit representation annotates as zero-width. Such a field
occupies no bit ranges, and the generated `unpack` now selects a zero-width
`BitVector` for it rather than erroring out.
