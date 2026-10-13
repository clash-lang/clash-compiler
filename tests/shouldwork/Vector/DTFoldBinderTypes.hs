{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The evaluator reduces @dtfold@ to an application of its combining function
-- to two @dtfold@s over the halves of the vector, which it selects with
-- case-expressions on @splitAt@. It used to give the binders of the patterns
-- of these case-expressions the type @2^(k-1)@, a number, instead of
-- @Vec (2^(k-1)) a@.
--
-- This test evaluates the subject of the case-expression in @topEntity@ with
-- the scoped evaluator ("Clash.Core.Evaluator"), in the environment
-- Clash uses, and checks the types of the binders of the patterns in the
-- result, which must have some.
module DTFoldBinderTypes where

import Clash.Prelude

import Data.Kind (Type)
import qualified Data.List as List
import Data.Proxy (Proxy (..))
import Data.Singletons (Apply, TyFun, type (@@))

import Clash.Backend
import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import Clash.Core.Alpha (aeqTy)
import Clash.Core.DataCon (dataConInstArgTys)
import Clash.Core.Evaluator (bindAllocated, whnf)
import Clash.Core.Evaluator.Types (Machine (..), initMachine)
import Clash.Core.HasType (inferCoreTypeOf)
import Clash.Core.NameMap (emptyNameMap, foldrWithGlobalEnv)
import Clash.Core.Subst (openAlt, openBind, openIdBinder, openTyBinder, teleIdBinderTypes)
import Clash.Core.Syntax (Distinct, Scope)
import qualified Clash.Core.Syntax as S
import Clash.Core.DataCon (dcName)
import Clash.Core.Term (stripTicks)
import Clash.Core.Type (TypeView (..), coreView, tyView)
import Clash.Core.TypeEnv (TypeEnv, mkTypeEnv)
import Clash.Driver.Types
import Clash.Rewrite.Scoped.Whnf (evaluatorEnv)
import qualified Data.IntMap.Strict as IntMap

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
  let te = mkTypeEnv (envTyConMap env)
      bm = designBindings design
      isTop b = nameOcc (S.globalIdName (bindingId b)) == "DTFoldBinderTypes.topEntity"
  top <- case foldrWithGlobalEnv (\_ b bs -> [b | isTop b] <> bs) [] bm of
    [b] -> pure (bindingTerm b)
    _ -> error "Could not find topEntity"
  -- Evaluate the subject of the case-expression in the body of topEntity, in
  -- the scope of its arguments, with the environment Clash uses
  openLams S.emptyScope top $ \sc body -> case stripTicks body of
    S.Case subj _ _ ->
      whnf (evaluatorEnv te) True (initMachine sc emptyNameMap bm (IntMap.empty, 0) subj) $ \m ->
        -- The result, bound by the bindings the machine allocated, which
        -- holds the heap
        let res = bindAllocated m (mTerm m)
            binders = patBinders te sc res
            bad = [b | (False, b) <- binders]
            failure msg = error $ List.unlines $
              msg <> ["in the result of evaluating:", showPpr subj, "which is:", showPpr res]
        in if List.null binders then failure ["No pattern binders"]
           else if List.null bad then pure ()
           else failure ("Pattern binders with types that differ from the fields of their constructor:" : bad)
    e -> error ("Expected a case-expression:\n" <> showPpr e)

-- | Go under the lambdas of a term
openLams
  :: Distinct n => Scope n -> S.Term n -> (forall l. Distinct l => Scope l -> S.Term l -> r) -> r
openLams sc e k = case stripTicks e of
  S.Lam b body -> openIdBinder sc b body $ \sc' _ body' -> openLams sc' body' k
  _ -> k sc e

-- | The binders of the patterns of constructors without existential type
-- variables, and whether their types are those of the fields of the
-- constructor at the type of the subject
patBinders :: Distinct n => TypeEnv -> Scope n -> S.Term n -> [(Bool, String)]
patBinders te sc = \case
  S.Lam b e -> openIdBinder sc b e $ \sc' _ e' -> patBinders te sc' e'
  S.TyLam b e -> openTyBinder sc b e $ \sc' _ e' -> patBinders te sc' e'
  S.App e1 e2 -> patBinders te sc e1 <> patBinders te sc e2
  S.TyApp e _ -> patBinders te sc e
  S.Let bs0 e0 -> openBind sc bs0 e0 $ \sc' bs e -> case bs of
    S.NonRec _ x -> patBinders te sc x <> patBinders te sc' e
    S.Rec _ xs -> List.concatMap (patBinders te sc') xs <> patBinders te sc' e
  S.Case subj _ alts ->
    patBinders te sc subj <> List.concatMap (alt (inferCoreTypeOf te sc subj)) alts
  S.Cast e _ _ -> patBinders te sc e
  S.Tick _ e -> patBinders te sc e
  _ -> []
 where
  alt subjTy a = openAlt sc a $ \sc' p e ->
    let bad = case p of
          S.DataPat dc S.TNil ids
            | TyConApp _ tyArgs <- tyView (coreView te sc subjTy)
            , Just fieldTys <- dataConInstArgTys te sc dc tyArgs
            -> [ ( aeqTy (coreView te sc bndrTy) (coreView te sc fieldTy)
                 , showPpr bndrTy <> " in a pattern of " <> showPpr (dcName dc) )
               | (bndrTy, fieldTy) <- List.zip (teleIdBinderTypes ids) fieldTys ]
          _ -> []
    in bad <> patBinders te sc' e

mainVHDL :: IO ()
mainVHDL = mainCommon SVHDL

mainVerilog :: IO ()
mainVerilog = mainCommon SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainCommon SSystemVerilog
