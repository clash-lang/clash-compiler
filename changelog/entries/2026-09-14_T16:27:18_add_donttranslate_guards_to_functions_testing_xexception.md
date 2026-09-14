---
issues: []
prs: [3434]
---

# FIXED
Functions testing for `XException` (`isX`, `hasX`, `maybeIsX`, and `maybeHasX`) are now marked non-translatable. This makes the Clash compiler throw a proper error, instead of an internal one when it encounters their use.
