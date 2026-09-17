---
issues: [3414]
prs: []
---

# FIXED
Clash now checks all input targets before loading modules or generating HDL. Invalid arguments, including Clash flags passed without their leading dash, are reported immediately, instead of after compiling earlier inputs.
