module Clash.Netlist.Ast.Declaration where

import Data.Text (Text)

import Clash.Netlist.Ast.Type
import Clash.Netlist.Types

data Declaration
  -- | A signal declaration
  = SignalDecl
      (Maybe Comment)
      -- ^ Comment to insert in HDL before the declaration
      !Identifier
      -- ^ The name of the signal
      (Either Text HWType)
      -- ^ The type of the signal
      (Maybe Expr)
      -- ^ The initial value of the signal

  -- | A type declaration
  | TypeDecl
      (Maybe Comment)
      -- ^ Comment to insert in HDL before the declaration
      !Identifier
      -- ^ The name of the type
      HWType
      -- ^ The type

  -- | A constant declaration
  | ConstDecl
      (Maybe Comment)
      -- ^ Comment to insert in HDL before the declaration
      !Identifier
      -- ^ The name of the constant
      !Expr
      -- ^ The value of the constant
  deriving Show

