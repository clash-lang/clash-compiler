{-# LANGUAGE GADTs #-}

module Clash.FFI.VPI.Property.Type
  ( Property(..)
  ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt)

data Property a where
  TypeOf :: Property CInt
  Name :: Property CString
  FullName :: Property CString
  Size :: Property CInt
  File :: Property CString
  LineNo :: Property CInt
  IsScalar :: Property Bool
  IsVector :: Property Bool

