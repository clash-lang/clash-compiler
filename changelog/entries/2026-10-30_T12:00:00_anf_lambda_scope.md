---
issues: []
prs: []
---

# FIXED
ANF normalization no longer lifts bindings out of nested term or type lambdas, which could introduce free variables. Unsupported function-valued top-level inputs now reach the type diagnostic instead of failing the ANF scope invariant when invariant checking is enabled.
