{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.FreeVars (tests) where

import           GHC.Types.SrcLoc        (noSrcSpan)
import qualified Control.Lens            as Lens
import qualified Data.List               as List

import           Test.Tasty
import           Test.Tasty.HUnit

import           Clash.Core.FreeVars     (freeLocalVars, globalIds, typeFreeVars)
import           Clash.Core.Name         (Name(..), NameSort(..))
import           Clash.Core.Term         (Term(Var, App, Lam, TyLam))
import           Clash.Core.Type         (ConstTy(..), Type(ConstTy, ForAllTy, VarTy))
import           Clash.Core.Var          (IdScope(..), Var(..))

import           Test.Clash.Rewrite      (kindedTyVar, localId, tyVar)

-- TODO: We need tooling to create these mock constructs
fakeName :: Name a
fakeName =
  Name
    { nameSort=User
    , nameOcc="fake"
    , nameUniq=0
    , nameLoc=noSrcSpan
    }

f :: IdScope -> Var Term
f scope =
  let unique = 20 in
  Id { varName = Name { nameSort=User
                      , nameOcc="f"
                      , nameUniq=unique
                      , nameLoc=noSrcSpan }
     , varUniq = unique
     , varType = ConstTy (TyCon fakeName)
     , idScope = scope }

fLocalId, fGlobalId :: Var Term
fLocalId = f LocalId
fGlobalId = f GlobalId

-- 'term1' is a simple lambda function:
--
--   \f -> g f
--
-- where f and g have the same unique, but f has been marked as _local_ while
-- g is _global_. In other words:
--
--   \f[l] -> f[g] f[l]
--
-- This term is tested against to check whether various functions account for
-- the distinction between local/global variables correctly.
term1 :: Term
term1 =
  Lam fLocalId (Var fGlobalId `App` Var fLocalId)

-- | In @forall k. b@ with @b :: k@, the @k@ in the kind of the free @b@ is not
-- the @k@ bound by the @forall@, so 'typeFreeVars' must report it. It used to
-- close over the kind of @b@ with the in-scope set at the occurrence, in which
-- @k@ is bound. https://github.com/clash-lang/clash-compiler/issues/398,
-- https://github.com/clash-lang/clash-compiler/pull/446
typeFreeVarsClosesOverKinds :: Assertion
typeFreeVarsClosesOverKinds =
  [1, 2] @=? List.sort (map varUniq (Lens.toListOf typeFreeVars ty))
 where
  k = tyVar User "k" 1
  b = kindedTyVar User "b" 2 (VarTy k)
  ty = ForAllTy k (VarTy b)

-- | The term level equivalent of 'typeFreeVarsClosesOverKinds': in
-- @/\\k -> a@ with @a :: k@, the @k@ in the type of the free @a@ is free.
-- https://github.com/clash-lang/clash-compiler/issues/398,
-- https://github.com/clash-lang/clash-compiler/pull/446
termFreeVarsClosesOverTypes :: Assertion
termFreeVarsClosesOverTypes =
  [1, 2] @=? List.sort (map varUniq (Lens.toListOf freeLocalVars tm :: [Var ()]))
 where
  k = tyVar User "k" 1
  a = localId User "a" 2 (VarTy k)
  tm = TyLam k (Var a)

tests :: TestTree
tests =
  let globs1 = Lens.toListOf globalIds term1 in
  testGroup
    "Clash.Tests.Core.FreeVars"
    [ testCase "globalIds1" $ globs1 @=? [fGlobalId]
    , testCase "globalIds2" $
        assertBool
          "Global and local id can't BOTH be in globs1"
          (fLocalId `notElem` globs1)
    , testCase "typeFreeVars closes over kinds with an empty scope (#446)"
        typeFreeVarsClosesOverKinds
    , testCase "termFreeVars closes over types with an empty scope (#446)"
        termFreeVarsClosesOverTypes
    ]
