---
issues: []
prs: [3409]
---

# FIXED
`popCount`, `countLeadingZeros` and `countTrailingZeros` on `BitVector` (and hence on `Unsigned`, `Signed` and `Index`, which delegate to it) went through `Integer`, which made Clash emit a `Dubious primitive instantiation` warning for `GHC.Num.Integer.integerToInt#` and put an `Integer` in the generated HDL. They now use `Clash.Sized.Internal.Index.fromEnum#`, which has a black box of its own.
