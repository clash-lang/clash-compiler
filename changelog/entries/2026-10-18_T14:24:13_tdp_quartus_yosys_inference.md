---
issues: []
prs: [3498]
---

# FIXED
Quartus now infers a block RAM for `trueDualPortBlockRam` in Verilog and SystemVerilog, instead of implementing it in registers or failing with "Cannot synthesize dual-clock dual-port RAM". When both ports use the same clock, Quartus and Yosys no longer add logic to resolve simultaneous accesses of the same address on both ports, as their result is undefined.
