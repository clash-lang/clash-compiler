---
issues: []
prs: [3351]
---

# CHANGED
`Clash.Signal.Internal.resetGenN` now reports `clash-non-synthesizable` on every backend. Its VHDL and Verilog black boxes render nothing outside `translate_off`, just like the SystemVerilog one that already carried the warning, so instantiating it outside a test bench never produced hardware on any of them.
