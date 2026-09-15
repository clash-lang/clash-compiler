---
issues: []
prs: []
---

# CHANGED
`:vhdl`, `:verilog` and `:systemverilog` in `clashi` compile the design in the interactive session, with the same flags as the `clash` executable, instead of starting a new GHC session and linking a temporary shared library. Their output now matches `clash` and they run several times faster.
