---
issues: []
prs: []
---

# FIXED
The VHDL backend now generates valid VHDL for entities without ports, such as a top entity whose ports are all zero-width. It used to omit the `end;` of the entity declaration, and generate an empty `port map ()` when instantiating it.
