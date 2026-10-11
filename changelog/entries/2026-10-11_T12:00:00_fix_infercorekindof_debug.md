---
issues: []
prs: []
---

# FIXED
A `+debug` build of Clash no longer errors with `Unexpected application` when inferring the kind of an applied type constructor, such as `Maybe a`. This happened, for example, when specializing a function on an argument that follows such a type argument.
