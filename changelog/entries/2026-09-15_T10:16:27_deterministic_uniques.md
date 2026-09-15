---
issues: []
prs: []
---

# CHANGED
Clash's uniques no longer depend on the values GHC assigned to names. Generated HDL is now independent of the order in which GHC allocated uniques, which previously changed declaration order and generated names for about a quarter of the test suite's designs under `-dunique-increment=-1`. Some generated names change once as a result.
