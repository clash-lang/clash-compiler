---
issues: []
prs: [3351]
---

# FIXED
`caseCon` matched a case subject against its alternatives by constructor tag alone. A newtype constructor shares its tag with the first constructor of the type it represents, so a case on a newtype value carrying alternatives for the represented type - as GHC's `someNatVal` produces for `SNat` over `Natural` - bound a field to a pattern variable of the wrong type. That left the surrounding expression unfoldable, reported as `Unmatchable constant as case subject`, and put a redundant comparison in the generated HDL.
