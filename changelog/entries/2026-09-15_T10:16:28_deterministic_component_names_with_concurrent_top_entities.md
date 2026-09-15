---
issues: []
prs: []
---

# FIXED
With concurrent top entity compilation (the default), same-named components of different top entities could get their names (e.g. `calc` versus `calc_0`) depending on scheduling. Netlist generation now claims names in a fixed order.
