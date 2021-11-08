{-|
Copyright   : (C) 2020, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

This module contains functions which extract information from primitives /
their arguments which are needed for evaluating primitives.
-}

{-# LANGUAGE LambdaCase #-}

module Clash.GHC.PartialEval.Primitive.Info
  ( resultType
  , resultTyCon
  , resultDataCons
  , typeSize
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad.Except (runExcept)
import Data.Bifunctor (first)
import Data.Foldable (asum)
import GHC.Stack (HasCallStack)

import Clash.Core.DataCon
import Clash.Core.HasType (applyTypeToArgs)
import Clash.Core.Literal
import Clash.Core.PartialEval.AsTerm
import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Term (Term(..), PrimInfo(..))
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Util (tyNatSize)
import Clash.Unique

-- | Given a primitive and its arguments, determine the exact result type of
-- the result of the primitive.
--
resultType :: PrimInfo -> Args Value -> Eval Type
resultType pr args = do
  tcm <- getTyConMap
  let tmArgs = first asTerm <$> args

  pure (applyTypeToArgs (Prim pr) tcm (primType pr) tmArgs)

-- | Assuming the given type is a TyConApp, get the name of the TyCon
-- and the types of its arguments.
--
resultTyCon :: (HasCallStack) => Type -> Eval (TyConName, [Type])
resultTyCon ty =
  case tyView $ snd (splitFunForallTy ty) of
    TyConApp tcNm args -> pure (tcNm, args)
    _ -> empty

-- | Get the data constructors for the result type.
--
resultDataCons :: (HasCallStack) => Type -> Eval [DataCon]
resultDataCons ty = do
  tcm  <- getTyConMap
  tcNm <- fst <$> resultTyCon ty

  case lookupUniqMap tcNm tcm of
    Just tc -> pure (tyConDataCons tc)
    Nothing -> empty

-- | For a type of kind Nat, get the size of the type as an integer.
-- If there is a KnownNat constraint, this can be given as well.
--
typeSize :: (HasCallStack) => Type -> Maybe Value -> Eval Integer
typeSize ty = \case
  Just (VLiteral (NaturalLiteral i)) -> pure i
  Just (VLiteral (IntegerLiteral i)) -> pure i
  _ -> inspectType ty
 where
  inspectType a = do
    tcm <- getTyConMap

    case tyView (normalizeType tcm a) of
      TyConApp _ args -> asum (fmap inspectType args)
      FunTy _ b -> inspectType b
      OtherType b ->
        let result = runExcept (tyNatSize tcm b)
         in either (const empty) pure result
