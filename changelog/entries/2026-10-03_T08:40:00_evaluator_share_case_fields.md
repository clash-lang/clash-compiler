---
issues: []
prs: []
---

# FIXED
Clash no longer takes quadratic or exponential time to compile designs that use `iterateI` with a function that inspects its argument more than once, e.g. `iterateI (\(Just n) -> if n == maxBound then Nothing else Just (n + 1)) (Just 0)`, when the resulting vector is consumed by functions such as `takeI` or `foldr`. Clash's compile-time evaluator now shares the fields of a matched constructor instead of copying them into every place they are used.
