{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Subst (tests) where

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.SrcLoc        (noSrcSpan)
#else
import           SrcLoc                  (noSrcSpan)
#endif

import           Test.Tasty
import           Test.Tasty.HUnit

import           Clash.Core.Name         (Name(..), NameSort(..), OccName)
import           Clash.Core.Term         (Bind(..), Pat(..), Term(..))
import           Clash.Core.Type         (ConstTy(..), Type(..))
import           Clash.Core.Subst
import           Clash.Core.VarEnv
import           Clash.Core.Var          (Id, IdScope(..), TyVar, Var(..))
import           Clash.Unique            (Unique)

import           Test.Clash.Rewrite
  (assertAlphaEq, inScopeOf, intTy, localId, showPprU, tyVar)

fakeName :: Name a
fakeName =
  Name
    { nameSort=User
    , nameOcc="fake"
    , nameUniq=0
    , nameLoc=noSrcSpan
    }

unique :: Unique
unique = 20

mkTestId :: IdScope -> OccName -> Unique -> Id
mkTestId scope occ uniq = Id {
    varName = fakeName {nameUniq=uniq, nameOcc=occ}
  , varUniq = uniq
  , varType = ConstTy (TyCon fakeName)
  , idScope = scope
  }

termVar :: Var Term
termVar = mkTestId LocalId "term" unique

term1 :: Term
term1 = Var termVar

fakeType :: Type
fakeType = ConstTy (TyCon fakeName)

localX, localY, localZ, localW, globalG :: Id
localX = mkTestId LocalId "x" 21
localY = mkTestId LocalId "y" 22
localZ = mkTestId LocalId "z" 23
localW = mkTestId LocalId "w" 24
globalG = mkTestId GlobalId "g" 25

-- | The term substituted for 'localX' in the 'unsafeSubstTm' tests
payload :: Term
payload = Var localY

-- | Deshadowed w.r.t. an in-scope set holding 'localX' and 'localY', so it
-- satisfies 'unsafeSubstTm's precondition for substituting 'localX'
deshadowedTerm :: Term
deshadowedTerm =
  Lam localZ
    (Let (NonRec localW (Var localX))
      (Case (Var localW) fakeType
        [(DefaultPat, App (Var localX) (Var localZ))]))

-- Variables for the capture-avoidance tests below
x2, y1, z3 :: Id
x2 = localId User "x" 2 intTy
y1 = localId User "y" 1 intTy
z3 = localId User "z" 3 intTy

a2, b1, c3 :: TyVar
a2 = tyVar User "a" 2
b1 = tyVar User "b" 1
c3 = tyVar User "c" 3

-- | Substituting @[y := x]@ in @\\x -> y@ must rename the binder @x@, rather
-- than capture the free @x@ of the substitution range. This is the core
-- promise of the capture-avoiding substitution introduced by
-- https://github.com/clash-lang/clash-compiler/pull/361.
substTmLam :: Assertion
substTmLam =
  assertAlphaEq (Lam z3 (Var x2)) (substTm "substTmLam" subst (Lam x2 (Var y1)))
 where
  subst = extendIdSubst (mkSubst (inScopeOf [x2])) y1 (Var x2)

-- | Like 'substTmLam', but the term variable is captured through its /type/:
-- substituting @[y := (x :: a)]@ in @/\\a -> y@ must rename the type binder.
-- Note that alpha equivalence doesn't look at the types of variable
-- occurrences, so this checks the binder itself.
-- https://github.com/clash-lang/clash-compiler/pull/361
substTmTyLam :: Assertion
substTmTyLam =
  case substTm "substTmTyLam" subst (TyLam a2 (Var y1)) of
    tm@(TyLam a (Var x))
      | a == a2 -> assertFailure ("binder not renamed: " <> showPprU tm)
      | otherwise -> x @=? xa
    tm -> assertFailure ("unexpected result: " <> showPprU tm)
 where
  xa = localId User "x" 5 (VarTy a2)
  subst = extendIdSubst (mkSubst (inScopeOf [a2])) y1 (Var xa)

-- | Substituting @[b := a]@ in @forall a. b@ must rename the binder @a@.
-- https://github.com/clash-lang/clash-compiler/pull/361
substTyForAll :: Assertion
substTyForAll =
  assertAlphaEq (ForAllTy c3 (VarTy a2)) (substTy subst (ForAllTy a2 (VarTy b1)))
 where
  subst = extendTvSubst (mkSubst (inScopeOf [a2])) b1 (VarTy a2)

-- | 'deShadowTerm' must rename a binder that is already in scope.
-- https://github.com/clash-lang/clash-compiler/pull/361
deShadowTermRenames :: Assertion
deShadowTermRenames =
  case deShadowTerm (inScopeOf [x2]) (Lam x2 (Var x2)) of
    tm@(Lam x (Var x'))
      | x == x2 -> assertFailure ("binder not renamed: " <> showPprU tm)
      | otherwise -> x @=? x'
    tm -> assertFailure ("unexpected result: " <> showPprU tm)

-- | 'freshenTm' must rename a binder that is already in scope, and return an
-- in-scope set that includes the new binder.
-- https://github.com/clash-lang/clash-compiler/pull/361
freshenTmRenames :: Assertion
freshenTmRenames =
  case freshenTm (inScopeOf [x2]) (Lam x2 (Var x2)) of
    (is1, tm@(Lam x (Var x')))
      | x == x2 -> assertFailure ("binder not renamed: " <> showPprU tm)
      | otherwise -> do
          x @=? x'
          assertBool "new binder not in returned in-scope set"
            (x `elemInScopeSet` is1)
    (_, tm) -> assertFailure ("unexpected result: " <> showPprU tm)

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Core.Subst"
    [ testCase "deShadow type/term" $
        term1 @=? deShadowTerm (extendInScopeSet emptyInScopeSet termVar) term1

    , testCase "unsafeSubstTm substitutes a local variable" $
        App payload (Var localZ) @=?
          unsafeSubstTm emptyVarEnv (unitVarEnv localX payload)
            (App (Var localX) (Var localZ))

    , testCase "unsafeSubstTm leaves unmatched variables alone" $
        Var localZ @=?
          unsafeSubstTm emptyVarEnv (unitVarEnv localX payload) (Var localZ)

    , testCase "unsafeSubstTm looks globals up in the global substitution" $ do
        payload @=? unsafeSubstTm (unitVarEnv globalG payload) emptyVarEnv
                      (Var globalG)
        -- A global is never looked up in the local substitution, nor the other
        -- way around
        Var globalG @=? unsafeSubstTm emptyVarEnv (unitVarEnv globalG payload)
                          (Var globalG)
        Var localX @=? unsafeSubstTm (unitVarEnv localX payload) emptyVarEnv
                         (Var localX)

    , testCase "unsafeSubstTm agrees with substTm on a deshadowed term" $
        let
          is = extendInScopeSetList emptyInScopeSet [localX, localY]
          subst = extendIdSubst (mkSubst is) localX payload
        in
          substTm "unsafeSubstTm test" subst deshadowedTerm @=?
            unsafeSubstTm emptyVarEnv (unitVarEnv localX payload)
              deshadowedTerm

    , testCase "substTm renames a Lam binder that would capture (#361)"
        substTmLam
    , testCase "substTm renames a TyLam binder that would capture (#361)"
        substTmTyLam
    , testCase "substTy renames a ForAllTy binder that would capture (#361)"
        substTyForAll
    , testCase "deShadowTerm renames a binder that is in scope (#361)"
        deShadowTermRenames
    , testCase "freshenTm renames a binder that is in scope (#361)"
        freshenTmRenames
    ]
