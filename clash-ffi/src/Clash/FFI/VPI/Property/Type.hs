{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Property.Type
  ( Property(..)
  ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt)

import Clash.FFI.View

data Property a where
  TypeOf :: Property CInt
  Name :: Property CString
  FullName :: Property CString
  Size :: Property CInt
  File :: Property CString
  LineNo :: Property CInt
  IsScalar :: Property Bool
  IsVector :: Property Bool
  Direction :: Property CInt
  NetType :: Property CInt
  PortIndex :: Property CInt
  ConstType :: Property CInt
#if defined(VERILOG_2001)
  IsSigned :: Property Bool
  IsLocalParam :: Property Bool
#endif

deriving stock instance Show (Property a)

instance UnsafeSend (Property a) where
  type Sent (Property a) = CInt

  unsafeSend =
    pure . \case
      TypeOf -> 1
      Name -> 2
      FullName -> 3
      Size -> 4
      File -> 5
      LineNo -> 6
      IsScalar -> 17
      IsVector -> 18
      Direction -> 20
      NetType -> 22
      PortIndex -> 29
      ConstType -> 40
#if defined(VERILOG_2001)
      IsSigned -> 65
      IsLocalParam -> 70
#endif

instance Send (Property a) where
  send = unsafeSend

