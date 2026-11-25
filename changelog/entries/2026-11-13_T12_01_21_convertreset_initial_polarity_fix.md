---
issues: []
prs: [3550]

---

# FIXED
`convertReset` could briefly release the reset of the destination domain at power-up before asserting it. This could happen when the source domain had synchronous resets and initial (or "power up") values for memory elements, the domains had different reset polarities, and the source domain's reset was asserted at power-up.
