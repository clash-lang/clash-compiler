---
issues: []
prs: []
---

# FIXED
Unhandled `error` calls are reported with their `HasCallStack` backtrace again
on GHC 9.10 and later. Since base-4.20 the backtrace lives in the exception's
`ExceptionContext` rather than in `ErrorCallWithLocation`, and
`displayException` does not include it. Note that the backtrace is still lost
when compiling top entities concurrently, as `async` re-throws with a fresh
one; use `-fclash-no-concurrent-topentity-compilation` to get the original.
