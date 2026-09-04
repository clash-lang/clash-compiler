---
issues: []
prs: [3351]
---

# FIXED
Reducing `split#` on a `BitVector` with a zero-width half produced a `removedArg` bottom value for that half rather than the zero-width `BitVector` it evaluates to in simulation. The evaluator could not fold through it, so a constant case subject derived from it made `caseCon` report `Unmatchable constant as case subject` and left a redundant multiplexer in the generated HDL.
