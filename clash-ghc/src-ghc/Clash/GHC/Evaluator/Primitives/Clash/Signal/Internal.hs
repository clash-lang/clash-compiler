{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.GHC.Evaluator.Primitives.Clash.Signal.Internal
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type (TypeView (..), tyView)
import Clash.GHC.Domain (isConcreteDomain, normalizeDomain)
import Clash.Core.Util (primCo, substArgTys)
import Clash.Core.TyCon (tyConDataCons)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util (textNameLit)

import qualified Clash.Signal.Internal

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ primStepEntry $(textNameLit 'Clash.Signal.Internal.sameDomain) $ \case
      PrimStepContext{..}
        | [domA, domB] <- tys
        , TyConApp maybeTc [eqTy] <- tyView (getResultTy tcm ty tys)
        , TyConApp eqTc eqArgs <- tyView eqTy
        , Just maybeCon <- UniqMap.lookup maybeTc tcm
        , [nothingDc, justDc] <- tyConDataCons maybeCon
        , let a = normalizeDomain tcm domA
              b = normalizeDomain tcm domB
        -> if a == b then do
             eqCon <- UniqMap.lookup eqTc tcm
             [reflDc] <- pure (tyConDataCons eqCon)
             [coTy] <- pure (substArgTys reflDc eqArgs)
             let refl = mkApps (Data reflDc) (map Right eqArgs ++ [Left (primCo coTy)])
             reduce (mkApps (Data justDc) [Right eqTy, Left refl])
           else if isConcreteDomain tcm a && isConcreteDomain tcm b then
             reduce (mkApps (Data nothingDc) [Right eqTy])
           else Nothing
      _ -> Nothing
  ]
