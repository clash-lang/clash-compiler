---
issues: []
prs: [3351]
---

# FIXED
The `ClearOnReset` reset logic of `blockRamU` and `blockRam1` converted its write address counter with `fromInteger . toInteger`, which put an `Integer` in the generated HDL and made Clash emit a `Dubious primitive instantiation` warning for `GHC.Num.Integer.integerToInt#`. It now uses `fromEnum`, which has a black box of its own.
