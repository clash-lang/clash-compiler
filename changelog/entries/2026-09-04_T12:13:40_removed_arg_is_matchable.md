---
issues: []
prs: [3351]
---

# FIXED
`caseCon` reported `Unmatchable constant as case subject` for a case whose subject evaluates to `Clash.Normalize.Primitives.removedArg`. That subject stands for a value that was proven dead, so like Clash's other bottom values it now collapses the whole case expression instead.
