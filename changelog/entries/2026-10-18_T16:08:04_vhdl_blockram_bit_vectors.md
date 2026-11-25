---
issues: []
prs: [3498]
---

# FIXED
Vivado now infers block RAMs for `blockRam`, `blockRamU`, `blockRam1` and `trueDualPortBlockRam` storing `Vec`s or `RTree`s in VHDL, also without `-fclash-hdlsyn Vivado`. It used to implement them in registers, or fail to synthesize `trueDualPortBlockRam`.
