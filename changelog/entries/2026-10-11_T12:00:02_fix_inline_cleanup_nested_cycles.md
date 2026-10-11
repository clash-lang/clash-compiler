---
issues: []
prs: []
---

# FIXED
`inlineCleanup` no longer drops a let-binding that is still referred to. This could happen when the let-bindings it inlines form nested cycles.
