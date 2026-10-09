---
issues: []
prs: []
---

# CHANGED
Clash spends less time flattening the component hierarchy after normalization. It no longer re-traverses let-bindings in which nothing changed, which especially helps designs that inline large components through thin wrappers.
