{-|
Copyright  :  (C) 2017, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Annotations.BitRepresentation.Internal
  ( coreToType'
  , bitToBit
  , bitsToBits
  ) where

import Clash.Util (curLoc)
import Clash.Core.Name (name2String)
import qualified Clash.Core.Type as C
import qualified Clash.Annotations.BitRepresentation.Util as BitRepresentation
import qualified Clash.Netlist.Types as Netlist

import Clash.Annotations.BitRepresentation ( Type'(..) )

import qualified Data.Text.Lazy as Text

import Prelude

bitToBit
  :: BitRepresentation.Bit
  -> Netlist.Bit
bitToBit BitRepresentation.H = Netlist.H
bitToBit BitRepresentation.L = Netlist.L
bitToBit BitRepresentation.U = Netlist.U

bitsToBits
  :: [BitRepresentation.Bit]
  -> [Netlist.Bit]
bitsToBits = map bitToBit


coreToType'
  :: C.Type
  -- ^ Type to convert to bit representation type
  -> Either
       String
       -- Error message
       Type'
       -- Bit representation type
coreToType' (C.AppTy t1 t2) =
  AppTy' <$> coreToType' t1 <*> coreToType' t2
coreToType' (C.ConstTy (C.TyCon name)) =
   return $ ConstTy' (Text.pack $ name2String name)
coreToType' (C.LitTy (C.NumTy n)) =
   return $ LitTy' n
coreToType' e =
  Left $ $(curLoc) ++ "Unexpected type: " ++ show e


