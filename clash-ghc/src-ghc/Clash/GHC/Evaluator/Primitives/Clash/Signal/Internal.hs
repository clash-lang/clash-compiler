{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Clash.GHC.Evaluator.Primitives.Clash.Signal.Internal
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type (Type (..), TypeView (..), tyView)
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
        , ForAllTy
            _tyVar_domA
            ( ForAllTy
                _tyVar_domB
                ( AppTy
                    _ -- KnownDomain domA ->
                    ( AppTy
                        _ -- KnownDoman domB ->
                        ( AppTy
                            maybeTy
                            ( AppTy
                                ( AppTy
                                    eqTy
                                    _varTy_domA
                                )
                                _varTy_domB
                            )
                        )
                    )
                )
            ) <- ty
        ->  let eqTy2 = AppTy (AppTy eqTy domA) domB
                maybeEqTy = AppTy maybeTy eqTy2

                TyConApp maybeEqTcNm _ = tyView maybeEqTy
                TyConApp eqTcNm _ = tyView eqTy2

                (Just maybeEqTc) = UniqMap.lookup maybeEqTcNm tcm
                (Just eqTc) = UniqMap.lookup eqTcNm tcm
                [nothingDc,justDc] = tyConDataCons maybeEqTc
                [reflDc] = tyConDataCons eqTc
             in if domA==domB then
                    reduce $ mkApps (Data justDc) [Right eqTy2, Left $ mkApps (Data reflDc) [Right domA, Right domB]]
                  else
                    reduce $ mkApps (Data nothingDc) [Right eqTy2]
      _ -> Nothing
  ]
