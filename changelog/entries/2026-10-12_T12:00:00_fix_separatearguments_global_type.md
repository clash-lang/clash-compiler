---
issues: []
prs: []
---

# FIXED
`separateArguments` gave a global function it applies to split arguments the wrong type: it repeated the type of the split argument once per field, and dropped the types of the parameters it wasn't applied to. When the global was applied to more arguments than its type showed, for example when its result type was a type variable instantiated with a function type, it dropped those arguments.
