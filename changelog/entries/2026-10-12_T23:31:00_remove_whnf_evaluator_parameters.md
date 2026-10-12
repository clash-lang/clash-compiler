---
issues: []
prs: []
---

# CHANGED
`generateHDL`, `normalizeEntity` and `genNetlist` no longer take a whnf evaluator (`Clash.GHC.Evaluator.evaluator`): normalization and Netlist generation run on scoped Clash Core, with the evaluator of scoped Clash Core. `NetlistEnv` no longer has a `_whnfEvaluator` field.
