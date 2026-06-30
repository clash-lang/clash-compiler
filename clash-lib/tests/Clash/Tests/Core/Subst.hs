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
import           Clash.Core.Type         (ConstTy(..), Type(ConstTy))
import           Clash.Core.Subst
import           Clash.Core.VarEnv
import           Clash.Core.Var          (Id, IdScope(..), Var(..))
import           Clash.Unique            (Unique)

fakeName :: Name a
fakeName =
  Name
    { nameSort=User
    , nameOcc="fake"
    , nameUniq=0
    , nameLoc=noSrcSpan
    , nameStable="fake"
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
    ]
