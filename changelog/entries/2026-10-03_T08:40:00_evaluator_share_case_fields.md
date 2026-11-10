---
issues: [3477]
prs: [3539]
---

# FIXED
Clash no longer takes quadratic or exponential time to compile designs that use `iterateI` with a function that inspects its argument more than once, e.g. `iterateI (\(Just n) -> if n == maxBound then Nothing else Just (n + 1)) (Just 0)`, when the resulting vector is consumed by functions such as `takeI` or `foldr`.
