{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackboxes of Netlist generation on scoped Clash Core. For now, these are
  adapters that run their counterparts in "Clash.Netlist.BlackBox" on Clash
  Core, see 'Clash.Netlist.Scoped.Types.toLegacy'. TODO(Phase 5, BlackBox):
  port.
-}

module Clash.Netlist.Scoped.BlackBox
  ( mkBlackBoxContext
  , mkPrimitive
  , extractPrimWarnOrFail
  ) where

import           Data.Text                    (Text)
import           GHC.Stack                    (HasCallStack)

import qualified Clash.Core.Term              as C (PrimInfo)
import           Clash.Core.Scoped.Syntax
import           Clash.Netlist.BlackBox       (extractPrimWarnOrFail)
import qualified Clash.Netlist.BlackBox       as Legacy
import           Clash.Netlist.Scoped.Types
import           Clash.Netlist.Types
  (BlackBoxContext, Declaration, DeclarationType, Expr, NetlistMonad)

import           Clash.Netlist.Scoped.Legacy  (toLegacyNetlistId)

-- | Create a template instantiation context for a blackbox: see
-- 'Clash.Netlist.BlackBox.mkBlackBoxContext'
mkBlackBoxContext
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> Text
  -- ^ Blackbox function name
  -> DeclarationType
  -- ^ Are the declarations concurrent or sequential?
  -> [LocalId n]
  -- ^ Identifiers binding the primitive/blackbox application
  -> [Either (Term n) (Type n)]
  -- ^ Arguments of the primitive/blackbox application
  -> NetlistMonad (BlackBoxContext, [Declaration])
mkBlackBoxContext ctx bbName declType resIds args =
  Legacy.mkBlackBoxContext bbName declType
    (map (toLegacyLocalId ctx) resIds) (toLegacyArgs ctx args)

-- | Turn a primitive application into an expression and declarations: see
-- 'Clash.Netlist.BlackBox.mkPrimitive'
mkPrimitive
  :: (HasCallStack, Distinct n)
  => CompCtx n
  -> Bool
  -- ^ Put BlackBox expression in parenthesis
  -> Bool
  -- ^ Treat BlackBox expression as declaration
  -> DeclarationType
  -- ^ Are the declarations concurrent or sequential?
  -> NetlistId n
  -- ^ Id to assign the result to
  -> C.PrimInfo
  -- ^ Primitive info
  -> [Either (Term n) (Type n)]
  -- ^ Arguments
  -> [Declaration]
  -- ^ Tick declarations
  -> NetlistMonad (Expr, [Declaration])
mkPrimitive ctx bbEParen bbEasD declType dst pInfo args tickDecls =
  Legacy.mkPrimitive bbEParen bbEasD declType (toLegacyNetlistId ctx dst) pInfo
    (toLegacyArgs ctx args) tickDecls
