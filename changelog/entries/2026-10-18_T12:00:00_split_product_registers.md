---
issues: []
prs: []
---

# ADDED
`-fclash-split-registers`, which renders registers of product types, such as records and tuples, as a register per field. Each register is named after the original register and the field, e.g. `state_baudCount`, so that the fields can be found in a waveform viewer or logic analyzer without slicing a wide bitvector. This doesn't change the behavior of the circuit.
