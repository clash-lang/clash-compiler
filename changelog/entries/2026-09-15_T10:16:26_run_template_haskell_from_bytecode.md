---
issues: []
prs: []
---

# FIXED
Template Haskell splices referring to another module of the design no longer make GHC link a temporary shared library against every package in scope, which took several seconds; splices run from bytecode instead.
