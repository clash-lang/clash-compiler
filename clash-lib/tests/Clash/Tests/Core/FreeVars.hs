{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.FreeVars (tests) where

import           GHC.Types.SrcLoc        (noSrcSpan)
import qualified Control.Lens            as Lens

import           Test.Tasty
import           Test.Tasty.HUnit

import           Clash.Core.FreeVars     (globalIds)
import           Clash.Core.Name         (Name(..), NameSort(..))
import           Clash.Core.Term         (Term(Var, App, Lam))
import           Clash.Core.Type         (ConstTy(..), Type(ConstTy))
import           Clash.Core.Var          (IdScope(..), Var(..))

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
    ]
