---
issues: [3041]
prs: [3223]
---

# FIXED
Run `bindConstantVar` after post-normalization `inlineCleanup`/`caseCon` so constant let-bindings exposed late are inlined before netlist generation.
