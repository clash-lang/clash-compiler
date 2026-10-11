---
issues: []
prs: []
---

# FIXED
Fix the reductions of `fold`, `imap` and `traverse` on vectors rewinding Clash's supply of unique identifiers, which made it hand out identifiers that were already in use.
