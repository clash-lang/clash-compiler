---
issues: []
prs: []
---

# REMOVED
The experimental partial evaluator (`Clash.Core.PartialEval` and `Clash.GHC.PartialEval`) is removed. The compiler never used it, other than to evaluate the attributes of `annotate`, which the whnf evaluator now does. `generateHDL` and `normalizeEntity` no longer take a partial evaluator, `genNetlist` takes the whnf evaluator instead of the partial one, and `RewriteEnv` no longer has a `_peEvaluator` field.
