---
issues: []
prs: []
---

# FIXED
Clash no longer risks binding two fields to the same variable when it builds a `case` expression that selects a field of a data constructor, which could make it select the wrong field.
