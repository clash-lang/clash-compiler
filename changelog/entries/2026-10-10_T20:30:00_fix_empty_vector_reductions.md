---
issues: []
prs: []
---

# FIXED
Clash no longer fails with "extractElems must be called with maxN >= 1" when it unrolls `imap`, `!!`, `replace` or `reverse` on an empty vector. For `imap`, this happened with default settings; for the others, only with `-fclash-compile-ultra`.
