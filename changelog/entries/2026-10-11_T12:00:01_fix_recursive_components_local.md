---
issues: []
prs: []
---

# FIXED
Fix a potential infinite loop during normalization: Clash could mistake a recursive function for a non-recursive one, and keep inlining it, if a local variable had the same unique as that function.
