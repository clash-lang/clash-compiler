---
issues: [3448]
prs: []
---

# FIXED
Custom bit representations on types with a single constructor holding a single
field no longer fail with `Unexpected HWType`. `mkADT` collapsed such a type
into the `HWType` of its field, leaving `convertToCustomRepr` without a
`Product` to attach the representation to.
