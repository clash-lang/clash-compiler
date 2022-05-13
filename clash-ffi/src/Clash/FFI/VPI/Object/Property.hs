{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Object.Property
  ( Property(..)
  , InvalidProperty(..)
  ) where

import Control.Exception (Exception)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import GHC.Stack (CallStack, prettyCallStack)

import Clash.FFI.View

-- | VPI objects can have different data associated with them, which is indexed
-- by property. Properties can have different types, with VPI supporting
--
--   * Integer properties (with 64-bit integer supported in SystemVerilog)
--   * Boolean properties
--   * String properties
--
-- Not every property is available on every type of object. Users should be
-- careful to check that the type of object supports the requested property.
-- If it does not, attempting to access the property throws 'InvalidProperty'.
--
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
#if defined(VERILOG_2001)
  IsSigned :: Property Bool
  IsLocalParam :: Property Bool
#endif

deriving stock instance Show (Property a)

type instance CRepr (Property _) = CInt

instance Send (Property a) where
  send =
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
#if defined(VERILOG_2001)
      IsSigned -> 65
      IsLocalParam -> 70
#endif

-- | An exception thrown when attempting to get the value of a property which
-- is not defined for the given object.
--
data InvalidProperty p a
  = InvalidProperty (Property p) a CallStack
  deriving anyclass (Exception)

instance (Show a) => Show (InvalidProperty p a) where
  show (InvalidProperty p a c) =
    mconcat
      [ "The property "
      , show p
      , " is not defined for the object "
      , show a
      , "\n"
      , prettyCallStack c
      ]
