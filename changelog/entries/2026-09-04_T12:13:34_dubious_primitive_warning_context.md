---
issues: []
prs: [3351]
---

# CHANGED
`Dubious primitive instantiation` warnings now name the component, the binder the primitive's result is assigned to, and the primitive's arguments (resolving variable references against sibling let-bindings). They are also deduplicated per *(component, primitive)* pair instead of per primitive, so a single run reveals every offending site rather than only the first. Designs with many offending components therefore emit more warnings than before.
