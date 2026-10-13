---
issues: []
prs: []
---

# CHANGED
Blackbox functions now see scoped Clash Core, and blackbox functions on Clash Core are no longer accepted. A blackbox function has type `Clash.Netlist.Types.BlackBoxFunction`:

```hs
type BlackBoxFunction =
     forall n
   . Distinct n
  => BlackBoxEnv n
  -> Bool
  -> Text
  -> [Either (Term n) (Type n)]
  -> [Type n]
  -> NetlistMonad (Either String (BlackBoxMeta, BlackBox))
```

To port a blackbox function:

1. Import `BlackBoxFunction` from `Clash.Netlist.Types`. If you import `Clash.Netlist.BlackBox.Types` wholesale, hide its `BlackBoxFunction`.
2. Add a leading `BlackBoxEnv n` argument. A function that only looks at the number of arguments, or at literals, can ignore it.
3. Take the term and type constructors from `Clash.Core.Syntax`, and functions on them from `Clash.Core.Term` (e.g. `collectArgs`) and `Clash.Core.Type` (e.g. `coreView`, `splitTyConAppM`, `isIntegerTy`). Functions that look through type synonyms or type families take the type environment and the scope of the arguments from the `BlackBoxEnv`: for example `coreView (bbTypeEnv env) (bbScope env) ty`. Payloads such as `NumTy`, `SymTy`, `TyCon`, `DataCon` and `Name` still come from the `Clash.Core.*` modules.
4. Decode literal arguments with `Clash.Core.TermLiteral` and `Clash.Core.TermLiteral.TH`. Their `termToData` and `termToDataError` strip ticks themselves, so the workaround for #2926 is no longer needed. Hand-written `TermLiteral` instances implement `termToData# :: Term n -> Either (Term n) a`, polymorphic in `n`. Helpers should be polymorphic in `n` as well, or name the scope of the arguments with a pattern signature `(env :: BlackBoxEnv n)`.

Primitive definitions that refer to blackbox functions don't change. For example, a blackbox function that looks through a type family to find the length of a vector:

```hs
-- before
myBBF :: BlackBoxFunction
myBBF _isD _primName args _resTys = do
  tcm <- Lens.view tcCache
  case rights args of
    (coreView tcm -> LitTy (NumTy n)) : _ -> ...

-- after
import Clash.Core.Syntax (Type (LitTy))
import Clash.Core.Type (coreView)
import Clash.Netlist.Types (BlackBoxFunction, bbScope, bbTypeEnv)

myBBF :: BlackBoxFunction
myBBF env _isD _primName args _resTys =
  case rights args of
    (coreView (bbTypeEnv env) (bbScope env) -> LitTy (NumTy n)) : _ -> ...
```
