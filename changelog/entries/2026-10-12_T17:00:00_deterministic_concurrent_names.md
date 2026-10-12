---
issues: []
prs: []
---

# FIXED
Concurrently compiled top entities could get different names in the HDL from one run to the next, depending on which top entity finished normalization first. Top entities now claim names in a fixed order, the order in which `-fclash-no-concurrent-topentity-compilation` compiles them.
