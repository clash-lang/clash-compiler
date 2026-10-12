{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Adapters for the parts of Netlist generation that still run on Clash Core,
  during the port to scoped Clash Core. TODO(Phase 7): remove.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Netlist.Scoped.Legacy
  ( legacyBlackBoxFunction
  ) where

import qualified Control.Lens                 as Lens
import           Control.Monad.IO.Class       (liftIO)
import           Data.Bifunctor               (bimap)
import           Data.Either                  (lefts)
import qualified Data.List                    as List
import           Data.Function                (on)

import qualified Clash.Core.FreeVars          as C
import qualified Clash.Core.Term              as C
import qualified Clash.Core.Type              as C
import qualified Clash.Core.Var               as C
import qualified Clash.Util.Supply            as Supply

import           Clash.Core.Scoped.Convert
  (convScope, fromClashTerm, fromClashType, withConvEnv)
import           Clash.Core.Scoped.Syntax

import qualified Clash.Netlist.BlackBox.Types as Legacy (BlackBoxFunction)
import qualified Clash.Netlist.Id             as Id
import           Clash.Netlist.Scoped.Types
import           Clash.Netlist.Types          (typeEnv)

-- | A blackbox function as one on Clash Core, for the callers that still run
-- on Clash Core. A blackbox function on scoped Core gets its arguments
-- converted into a scope with their free variables, whose HDL identifiers are
-- their occurrence names, see 'Clash.Netlist.Id.unsafeFromCoreId'.
legacyBlackBoxFunction :: SomeBlackBoxFunction -> Legacy.BlackBoxFunction
legacyBlackBoxFunction = \case
  LegacyBBF f -> f
  ScopedBBF f -> \isD nm args resTys -> do
    tenv <- Lens.view typeEnv
    supply <- liftIO Supply.newSupply
    let ids = List.nubBy ((==) `on` C.varUniq)
                (concatMap (Lens.toListOf C.freeLocalIds) (lefts args))
        tvs = List.nubBy ((==) `on` C.varUniq)
                (concatMap (Lens.toListOf C.termFreeTyVars) (lefts args)
                  <> concatMap (Lens.toListOf C.typeFreeVars)
                       (either (const []) pure =<< args)
                  <> concatMap (Lens.toListOf C.typeFreeVars) (resTys <> map C.varType ids))
    withConvEnv tvs ids $ \env ->
      let sc = convScope env
          tvNames = [ (n, tv) | tv <- tvs, VarTy _ _ n <- [fromClashType env (C.VarTy tv)] ]
          idNames = [ (n, v, Id.unsafeFromCoreId v)
                    | v <- ids, Var _ _ n <- [fromClashTerm env (C.Var v)] ]
          ctx = legacyCompCtx sc tenv supply tvNames idNames
      in f ctx isD nm
           (map (bimap (fromClashTerm env) (fromClashType env)) args)
           (map (fromClashType env) resTys)
