{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.TypeNats" in the evaluator. See
  "Clash.Core.Evaluator.Primitives" for how to write them.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Evaluator.Primitives.GHC.TypeNats
  ( primitives
  ) where

import Data.Text (Text)

import Clash.Core.Basic (LitTy (..))
import Clash.Core.Literal (Literal (..))
import Clash.Core.TyCon (TyConName, tyConDataCons)
import Clash.Util (textNameLit)

import qualified GHC.TypeNats

import Clash.Core.Evaluator.Primitives.Util
import Clash.Core.Evaluator.Types
import Clash.Core.Syntax
import Clash.Core.Term (mkApps)
import Clash.Core.Type (PiTele (..), SplitFunForall (..), TypeView (..), splitFunForallTy, tyView)
import Clash.Core.TypeEnv (lookupTyCon)

primitives :: [(Text, PrimReduction)]
primitives =
  [ primStepEntry $(textNameLit 'GHC.TypeNats.natVal) $ \case
      PrimStepContext{..}
        | [Lit (NaturalLiteral n), _] <- args
        -> reduce (Literal (NaturalLiteral n))
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.TypeNats.someNatVal) $ \case
      PrimStepContext{..}
        | [Lit (NaturalLiteral n)] <- args
        -> let resTy = getResultTy te sc ty tys
            in reduce (mkSomeNat te n resTy)
      _ -> Nothing

  , primStepEntry $(textNameLit 'GHC.TypeNats.withSomeSNat) $ \case
      PrimStepContext{..}
        | Lit (NaturalLiteral n) : fun : _ <- args
        , Just snatTcNm <- snatTyConName ty
        , Just snatTc <- lookupTyCon te snatTcNm
        , [snatDc] <- tyConDataCons snatTc
        -> let nTy = LitTy (NumTy n)
               snat = mkApps (Data snatDc) [Right nTy, Left (Literal (NaturalLiteral n))]
               ret = mkApps (valToTerm fun) [Right nTy, Left snat]
            in reduce ret
      _ -> Nothing

  ]

-- | The type constructor of the argument of the function that is the second
-- argument of
--
-- @
-- withSomeSNat :: forall rep (r :: TYPE rep). Natural -> (forall n. SNat n -> r) -> r
-- @
--
-- that is, @SNat@
snatTyConName :: Type n -> Maybe TyConName
snatTyConName ty = case splitFunForallTy ty of
  SplitFunForall tele _ -> argTy 1 funArgTyCon tele
 where
  funArgTyCon :: Type m -> Maybe TyConName
  funArgTyCon funTy = case splitFunForallTy funTy of
    SplitFunForall funTele _ -> argTy 0 tyConOf funTele

  tyConOf :: Type m -> Maybe TyConName
  tyConOf t = case tyView t of
    TyConApp tcNm _ -> Just tcNm
    _ -> Nothing

  -- The given function of the argument type with the given index
  argTy :: Int -> (forall k. Type k -> Maybe a) -> PiTele m l -> Maybe a
  argTy i f = \case
    PiNil -> Nothing
    PiForall _ rest -> argTy i f rest
    PiArg t rest
      | i == 0 -> f t
      | otherwise -> argTy (i - 1) f rest
