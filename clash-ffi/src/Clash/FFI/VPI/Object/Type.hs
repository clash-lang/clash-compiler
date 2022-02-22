{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Object.Type
  ( ObjectType(..)
  ) where

import           Control.Exception (Exception)
import           Foreign.C.Types (CInt)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View

data ObjectType
  = ObjModule
  | ObjNet
  | ObjParameter
  | ObjPort
  | ObjReg
#if defined(VERILOG_2001)
  | ObjCallback
#endif
  deriving stock (Eq, Show)

instance UnsafeSend ObjectType where
  type Sent ObjectType = CInt

  unsafeSend =
    pure . \case
      ObjModule -> 32
      ObjNet -> 36
      ObjParameter -> 41
      ObjPort -> 44
      ObjReg -> 48
#if defined(VERILOG_2001)
      ObjCallback -> 107
#endif

instance Send ObjectType where
  send = unsafeSend

data UnknownObjectType
  = UnknownObjectType CInt CallStack
  deriving anyclass (Exception)

instance Show UnknownObjectType where
  show (UnknownObjectType x c) =
    mconcat
      [ "Unknown object type: "
      , show x
      , "\n"
      , prettyCallStack c
      ]

instance UnsafeReceive ObjectType where
  type Received ObjectType = CInt

  unsafeReceive = \case
    32 -> pure ObjModule
    36 -> pure ObjNet
    41 -> pure ObjParameter
    44 -> pure ObjPort
    48 -> pure ObjReg
#if defined(VERILOG_2001)
    107 -> pure ObjCallback
#endif
    ty -> Sim.throw (UnknownObjectType ty callStack)

instance Receive ObjectType where
  receive = unsafeReceive

