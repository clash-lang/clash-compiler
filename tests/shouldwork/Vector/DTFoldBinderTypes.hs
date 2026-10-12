{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The evaluator reduces @dtfold@ to an application of its combining function
-- to two @dtfold@s over the halves of the vector, which it selects with
-- case-expressions on @splitAt@. It used to give the binders of the patterns
-- of these case-expressions the type @2^(k-1)@, a number, instead of
-- @Vec (2^(k-1)) a@.
module DTFoldBinderTypes where

import Clash.Prelude

import Data.Kind (Type)
import qualified Data.List as List
import Data.Proxy (Proxy (..))
import Data.Singletons (Apply, TyFun, type (@@))

import Clash.Backend
import Clash.Core.DataCon (DataCon (..))
import Clash.Core.Evaluator.Types (whnf')
import Clash.Core.HasType (inferCoreTypeOf)
import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Term (Pat (..), Term (..), collectBndrs, walkTerm)
import Clash.Core.Type (TypeView (..), coreView, tyView)
import Clash.Core.Subst (aeqType)
import Clash.Core.Util (substArgTys)
import Clash.Core.Var (Var (..))
import Clash.Core.VarEnv (eltsVarEnv, mkInScopeSet, mkVarSet)
import Clash.Driver.Types
import Clash.GHC.Evaluator (evaluator)
import Clash.Rewrite.Scoped.Globals (toClashBindings)
import Clash.Util.Supply (newSupply, splitSupply)

import Test.Tasty.Clash
import Test.Tasty.Clash.CoreTest

data PairOf (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (PairOf a) l = (a, a)

step :: SNat l -> PairOf (Unsigned 8) @@ l -> PairOf (Unsigned 8) @@ l -> PairOf (Unsigned 8) @@ (l + 1)
step _ (p, q) (r, s) = (p + r, q * s)

-- | The tail of the vector is unknown, so evaluation gets stuck on the
-- case-expressions that select the halves of the vector
topEntity :: Unsigned 8 -> Vec 3 (Unsigned 8) -> Unsigned 8
topEntity a rest =
  fst (dtfold (Proxy @(PairOf (Unsigned 8))) (\x -> (x, x)) step (a :> rest))

testPath :: FilePath
testPath = "tests/shouldwork/Vector/DTFoldBinderTypes.hs"

mainCommon
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -> IO ()
mainCommon hdl = do
  (env, design, _) <- runToCoreStage hdl id testPath
  (convSupply, supply) <- splitSupply <$> newSupply
  let tcm = envTyConMap env
      -- The evaluator of Clash Core, on Clash Core
      bm = toClashBindings convSupply (designBindings design)
  te <- case List.find ((== "DTFoldBinderTypes.topEntity") . nameOcc . varName . bindingId) (eltsVarEnv bm) of
    Just b -> pure (bindingTerm b)
    Nothing -> error "Could not find topEntity"
  let (bndrs, body) = collectBndrs te
      is = mkInScopeSet (mkVarSet [v | Left v <- bndrs])
  subj <- case body of
    Case scrut _ _ -> pure scrut
    _ -> error ("Expected a case-expression:\n" <> showPpr body)
  let (_, heap, res) = whnf' evaluator bm mempty tcm (mempty, 0) supply is True subj
      tms = res : eltsVarEnv heap
      -- The binders of a pattern of a constructor without existential type
      -- variables, whose types differ from those of the fields
      badBinders tm =
        [ (dc, bndr)
        | Case scrut _ alts <- walkTerm Just tm
        , (DataPat dc [] bndrs, _) <- alts
        , List.null (dcExtTyVars dc)
        , TyConApp _ tyArgs <- [tyView (coreView tcm (inferCoreTypeOf tcm scrut))]
        , (bndr, fieldTy) <- List.zip bndrs (substArgTys dc tyArgs)
        , not (aeqType (coreView tcm (varType bndr)) (coreView tcm fieldTy))
        ]
  case List.concatMap badBinders tms of
    [] -> pure ()
    bad -> error $ List.unlines $
      "Pattern binders with types that differ from the fields of their constructor:" :
      [ showPpr bndr <> " in a pattern of " <> showPpr (dcName dc) | (dc, bndr) <- bad ] <>
      ["in the result of evaluating:", showPpr subj]

mainVHDL :: IO ()
mainVHDL = mainCommon SVHDL

mainVerilog :: IO ()
mainVerilog = mainCommon SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainCommon SSystemVerilog
