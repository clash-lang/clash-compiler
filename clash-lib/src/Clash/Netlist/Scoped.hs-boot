{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The signatures of the handlers of Netlist generation on scoped Clash Core,
  for the modules that "Clash.Netlist.Scoped" imports, e.g. those of
  blackboxes. Each handler gets the t'Clash.Netlist.Types.CompCtx' of
  the terms it is given.
-}

{-# LANGUAGE DataKinds #-}

module Clash.Netlist.Scoped
  ( genComponent
  , mkDeclarations
  , mkDeclarations'
  , mkExpr
  , mkNetDecl
  , mkFunApp
  , mkSelection
  , mkProjection
  , mkDcApplication
  ) where

import Data.List.NonEmpty (NonEmpty)
import GHC.Stack (HasCallStack)

import Clash.Core.DataCon (DataCon)
import Clash.Core.Scoped.Syntax (Alt, Distinct, GlobalId, Term, Type)
import Clash.Netlist.Types (CompCtx, LocalId, NetlistId)
import Clash.Netlist.Types
  (Component, ComponentMeta, Declaration, DeclarationType, Expr, HWType, Identifier,
   NetlistMonad)

genComponent
  :: HasCallStack
  => GlobalId
  -> NetlistMonad (ComponentMeta, Component)

mkDeclarations
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> LocalId n
  -> Term n
  -> NetlistMonad [Declaration]

mkDeclarations'
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> DeclarationType
  -> LocalId n
  -> Term n
  -> NetlistMonad [Declaration]

mkExpr
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> Bool
  -> DeclarationType
  -> NetlistId n
  -> Term n
  -> NetlistMonad (Expr, [Declaration])

mkNetDecl :: Distinct n => CompCtx n -> (LocalId n, Term n) -> NetlistMonad [Declaration]

mkFunApp
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> DeclarationType
  -> Identifier
  -> Either (LocalId n) GlobalId
  -> [Term n]
  -> [Declaration]
  -> NetlistMonad [Declaration]

mkSelection
  :: Distinct n
  => CompCtx n
  -> DeclarationType
  -> NetlistId n
  -> Term n
  -> Type n
  -> NonEmpty (Alt n)
  -> [Declaration]
  -> NetlistMonad [Declaration]

mkProjection
  :: Distinct n
  => CompCtx n
  -> DeclarationType
  -> Bool
  -> NetlistId n
  -> Term n
  -> Type n
  -> Alt n
  -> NetlistMonad (Expr, [Declaration])

mkDcApplication
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> DeclarationType
  -> [HWType]
  -> NetlistId n
  -> DataCon
  -> [Term n]
  -> NetlistMonad (Expr, [Declaration])
