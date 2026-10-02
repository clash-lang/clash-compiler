---
issues: []
prs: []
---

# FIXED
Errors raised while compiling top entities concurrently now report the
backtrace they were thrown with, instead of one pointing into `async`.
`mapConcurrently_` re-throws whatever a worker threw, and since base-4.20 a
re-throw collects a fresh backtrace that replaces the original.
