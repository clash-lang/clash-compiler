---
issues: []
prs: [3500]
---

# FIXED
The VHDL backend now generates valid VHDL for entities without ports, such as a top entity whose ports are all zero-width. This can be helpful when creating self-contained VHDL test benches.
