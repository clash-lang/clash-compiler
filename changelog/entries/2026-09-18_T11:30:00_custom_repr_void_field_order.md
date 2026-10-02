---
issues: []
prs: []
---

# FIXED
Custom bit representations no longer misplace field annotations when a
constructor has a zero-width field that is not its last. Such fields were
dropped from the type before the annotations were paired up with it, shifting
every annotation after the dropped field onto the wrong one; generating HDL
for such a type failed with `Prelude.!!: index too large`.
