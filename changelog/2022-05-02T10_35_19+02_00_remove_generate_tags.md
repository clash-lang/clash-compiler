CHANGED: Clash no longer renders the "generate" and "endgenerate" keywords in (System)Verilog, since these are purely optional.
This means the ~GENERATE and ~ENDGENERATE blackbox tags are now noops, however for backwards compatibility they can still be parsed.
