---
issues: [3306]
prs: []
---

# DEPRECATED
`-fclash-no-prim-warn` is deprecated in favor of `-Wno-clash-dubious-primitive` and `-Wno-clash-non-synthesizable`.
Correspondingly, the `opt_primWarn` field of `ClashOpts` has been replaced by `opt_warnings`,
which holds the enabled/fatal state of all named warnings.
