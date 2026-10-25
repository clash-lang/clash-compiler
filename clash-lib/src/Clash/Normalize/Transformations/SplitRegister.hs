{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Splitting registers of product types into a register per field.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Normalize.Transformations.SplitRegister
  ( splitRegister
  , splitRegisterWorker
  ) where

import qualified Control.Lens as Lens
import Data.Either (lefts, rights)
import qualified Data.List.Extra as List
import Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Text.Extra as Text (showt)
import GHC.Stack (HasCallStack)

import qualified Clash.Signal.Internal as Signal

import Clash.Core.DataCon (DataCon(..))
import Clash.Core.HasType (inferCoreTypeOf)
import Clash.Core.Name (Name(..), isIdentifierText)
import Clash.Core.Subst (extendIdSubstList, mkSubst, substTm)
import Clash.Core.Term
  ( Bind(..), LetBinding, NameMod(..), PrimInfo(..), Term(..), TickInfo(..)
  , bindToList, collectArgsTicks, mkApps, mkTicks)
import Clash.Core.TyCon (TyConMap, tyConDataCons)
import Clash.Core.Type
  (LitTy(..), Type(..), TypeView(..), coreView, mkTyConApp, tyView)
import Clash.Core.Util (dataConInstArgTysE, mkSelectorCase)
import Clash.Core.Var (Var(..))
import Clash.Core.VarEnv
  (InScopeSet, extendInScopeSet, extendInScopeSetList)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Netlist.Types (FilteredHWType(..), HWType(..))
import Clash.Netlist.Util (coreTypeToHWType)
import Clash.Normalize.Primitives (removedArg)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.StrategyDSL (Transformation, onLet, toTransformation)
import Clash.Rewrite.Types
  ( TransformContext(..), customReprs, splitRegisters, tcCache
  , typeTranslator)
import Clash.Rewrite.Util
  (changed, mkTmBinderFor, runWithHWTypeCache)

{- | Split a register of a product type into a register per field. Turns:

> letrec
>   s = register# @dom @(T a b) kd nfdx clk rst en (T i0 i1) (T r0 r1) next
>   ...
> in ...

into

> letrec
>   s_fieldA = register# @dom @a kd nfdx_a clk rst en i0 r0 (case next of T x _ -> x)
>   s_fieldB = register# @dom @b kd nfdx_b clk rst en i1 r1 (case next of T _ y -> y)
>   ...
> in ...

and replaces all references to @s@ in the let-expression by @T s_fieldA s_fieldB@.
Registers are named after the original binder and the record field (or
@sel0@, @sel1@, ... for products without field names). Fields that are products
themselves are split as well.

The point of this transformation is debuggability: rather than a single wide
register (a bitvector in Verilog) whose fields can only be found by slicing, the
HDL gets a named register per field. It doesn't change the behavior of the
circuit, nor (after synthesis) the number of flip-flops. It's enabled with
@-fclash-split-registers@.

Types with a custom bit representation are not split, as their fields might not
map onto consecutive bits.
-}
splitRegister :: Transformation
splitRegister = toTransformation "splitRegister" (onLet 'splitRegisterWorker)

-- | The 'Let' handler of 'splitRegister'.
splitRegisterWorker
  :: HasCallStack
  => TransformContext -> Term -> Bind Term -> Term -> NormalizeSession Term
splitRegisterWorker (TransformContext is0 _) node bind body = do
  enabled <- Lens.view splitRegisters
  if not enabled then pure node else do
    let binds0 = bindToList bind
        is1 = extendInScopeSetList is0 (map fst binds0)
    (is2, results) <- List.mapAccumLM splitBinding is1 binds0
    if not (any isJust results) then pure node else do
      let
        -- References to split registers are replaced by a data constructor
        -- applied to the field registers
        replacements = [(x, repl) | ((x, _), Just (repl, _)) <- zip binds0 results]
        binds1 = concat (zipWith keepOrSplit binds0 results)
        subst = extendIdSubstList (mkSubst is2) replacements
        substBind (v, e) = (v, substTm "splitRegister" subst e)
      changed
        (Let (Rec (map substBind binds1)) (substTm "splitRegister" subst body))
 where
  keepOrSplit b Nothing = [b]
  keepOrSplit _ (Just (_, newBinds)) = newBinds

-- | Split a binding if it's a register of a splittable product type. Returns
-- the term that should replace references to the binder, and the bindings for
-- the field registers.
splitBinding
  :: InScopeSet
  -> LetBinding
  -> NormalizeSession (InScopeSet, Maybe (Term, [LetBinding]))
splitBinding is0 (x, rhs)
  | (Prim pInfo, args, ticks) <- collectArgsTicks rhs
  , Just (nfdxIx, valueIxs) <- registerArgs (primName pInfo)
  , [domTy, aTy] <- rights args
  , let tmArgs = lefts args
  , length tmArgs > maximum (nfdxIx:valueIxs)
  = do
    tcm <- Lens.view tcCache
    splittable <- isSplittable tcm aTy
    case productFields is0 tcm aTy of
      Just (dc, tyConArgs, fieldTys) | splittable -> do
        let nfdxTy = inferCoreTypeOf tcm (tmArgs !! nfdxIx)
            labels = map Just (dcFieldLabels dc) ++ repeat Nothing
        (is1, fields) <-
          List.mapAccumLM
            (splitField tcm dc pInfo domTy nfdxIx valueIxs nfdxTy tmArgs ticks)
            is0
            (zip3 [0..] fieldTys labels)
        let repl = mkApps (Data dc) (map Right tyConArgs <> map (Left . fst) fields)
        pure (is1, Just (repl, concatMap snd fields))
      _ -> pure (is0, Nothing)
 where
  -- Create the register for a single field. Recursively splits it if the field
  -- type is a splittable product too.
  splitField tcm dc pInfo domTy nfdxIx valueIxs nfdxTy tmArgs ticks is1 (i, fieldTy, label) = do
    projected <- mapM (project is1 tcm dc i) (zip [0..] tmArgs)
    let
      -- We can't conjure up an 'NFDataX' dictionary for the field type, but
      -- the register primitives don't use it.
      fieldArg j arg
        | j == nfdxIx = TyApp (Prim removedArg) (replaceClassArg nfdxTy fieldTy)
        | otherwise = arg
      newTmArgs = zipWith fieldArg [0 :: Int ..] projected
      fieldRegister =
        mkTicks
          (mkApps (Prim pInfo) (map Right [domTy, fieldTy] <> map Left newTmArgs))
          (fieldTicks label i ticks)
      suffix = case label of
        Just l | isIdentifierText l -> l
        _ -> "sel" <> Text.showt (i :: Int)
      xNm = varName x
      fieldNm = xNm { nameOcc = nameOcc xNm <> "_" <> suffix }
    r <- mkTmBinderFor is1 tcm fieldNm fieldRegister
    let is2 = extendInScopeSet is1 r
    (is3, nested) <- splitBinding is2 (r, fieldRegister)
    pure $ case nested of
      Nothing -> (is3, (Var r, [(r, fieldRegister)]))
      Just (repl, binds) -> (is3, (repl, binds))
   where
    -- Project the i-th field out of a value argument. Leave other arguments
    -- alone.
    project is tcm' dc' fieldIx (j, arg)
      | j `elem` valueIxs = case collectArgsTicks arg of
          (Data dc1, dcArgs, _) | dc1 == dc' ->
            pure (lefts dcArgs !! fieldIx)
          _ ->
            mkSelectorCase "splitRegister" is tcm' arg (dcTag dc') fieldIx
      | otherwise = pure arg

  -- A register renamed with 'Clash.Magic.setName' should not lead to field
  -- registers with the same name, so suffix them with the field name.
  fieldTicks label i ticks
    | any isSetName ticks =
        NameMod SuffixName (LitTy (SymTy (Text.unpack suffix))) : ticks
    | otherwise = ticks
   where
    suffix = case label of
      Just l | isIdentifierText l -> l
      _ -> "sel" <> Text.showt (i :: Int)
    isSetName = \case
      NameMod SetName _ -> True
      _ -> False

splitBinding is0 _ = pure (is0, Nothing)

-- | For register primitives: the index of the 'NFDataX' dictionary and the
-- indices of the arguments of the register's value type (initial value, reset
-- value, and input), counting term arguments only.
registerArgs :: Text.Text -> Maybe (Int, [Int])
registerArgs nm
  | nm == Text.showt 'Signal.register# = Just (1, [5, 6, 7])
  | nm == Text.showt 'Signal.asyncRegister# = Just (1, [5, 6, 7])
  | nm == Text.showt 'Signal.delay# = Just (1, [4, 5])
  | otherwise = Nothing

-- | The data constructor, its type arguments, and its field types if the given
-- type is a product type: a type with a single data constructor without
-- existentially quantified type variables.
productFields
  :: InScopeSet
  -> TyConMap
  -> Type
  -> Maybe (DataCon, [Type], [Type])
productFields is tcm ty = case tyView (coreView tcm ty) of
  TyConApp tcNm tyConArgs
    | Just tc <- UniqMap.lookup tcNm tcm
    , [dc] <- tyConDataCons tc
    , null (dcExtTyVars dc)
    , Just fieldTys <- dataConInstArgTysE is tcm dc tyConArgs
    -> Just (dc, tyConArgs, fieldTys)
  _ -> Nothing

-- | Only split types rendered as products with at least two (non-zero width)
-- fields. That excludes, among others, types with a custom bit representation.
isSplittable :: TyConMap -> Type -> NormalizeSession Bool
isSplittable tcm ty = do
  tran <- Lens.view typeTranslator
  reprs <- Lens.view customReprs
  hwTy <- runWithHWTypeCache (coreTypeToHWType tran reprs tcm ty)
  pure $ case hwTy of
    Right (FilteredHWType hty _) -> isProduct hty
    Left _ -> False
 where
  isProduct = \case
    Annotated _ hty -> isProduct hty
    Product _ _ (_:_:_) -> True
    _ -> False

-- | Replace the argument of a single-parameter class constraint, i.e. turn
-- @NFDataX a@ into @NFDataX b@.
replaceClassArg :: Type -> Type -> Type
replaceClassArg classTy newArg = case tyView classTy of
  TyConApp tcNm [_] -> mkTyConApp tcNm [newArg]
  _ -> classTy
