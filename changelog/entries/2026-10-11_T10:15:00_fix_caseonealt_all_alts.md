---
issues: []
prs: []
---

# FIXED
Clash no longer replaces a `case` expression whose alternatives look the same by its first alternative when another alternative returns a variable bound by its own pattern, which made that alternative return the wrong value.
