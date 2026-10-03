---
issues: []
prs: []
---

# FIXED
Clash compiles applicative-style code over vectors, such as `f <$> xs <*> ys <*> zs`, much faster. Clash used to take time exponential in the number of `<*>`s to unroll the intermediate vectors of functions this creates.
