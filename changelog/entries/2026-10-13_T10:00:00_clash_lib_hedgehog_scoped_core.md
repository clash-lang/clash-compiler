---
issues: []
prs: []
---

# CHANGED
The generators of `clash-lib-hedgehog` generate scoped Clash Core (`Clash.Core.Scoped.Syntax`) instead of Clash Core. Kinds, types and terms are indexed by their scope, and are generated in an environment `Clash.Hedgehog.Core.Var.Env n`, which has the scope and the variables in it that they may mention, instead of a `UniqMap` of free variables. For example:

```hs
-- before
genTermFrom :: TyConMap -> UniqMap (Either TyVar Id) -> Type -> CoreGenT m Term
genClosedMonoType :: TyConMap -> Kind -> CoreGenT m Type

-- after
genTermFrom :: Distinct n => TyConMap -> Env n -> Type n -> CoreGenT m (Term n)
genClosedMonoType :: TyConMap -> Kind 'VoidS -> CoreGenT m (Type 'VoidS)
```

Use `emptyEnv` (or the new `genClosedTerm`) for closed types and terms, and `genTyVar`, `genTyVars`, `genLocalId` and `genLocalIds` to bind variables in an environment. `genId` and `genVars` are removed, `genGlobalId` takes a closed type, and the sampling functions of `Clash.Hedgehog.Unique` take a function that gives the type of an element instead of a `HasType` constraint.
