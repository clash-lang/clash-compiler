{-|
Copyright  :  (C) 2018, Google Inc.
                  2022, LUMI GUIDE FIETSDETECTIE B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Houses internal BitRepresentation code which cannot be housed in clash-prelude
due to its dependencies.
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Annotations.BitRepresentation.ClashLib
  ( coreToType'
  , bitsToBits
  ) where

import           Clash.Annotations.BitRepresentation.Internal
  (Type'(..))
import qualified Clash.Annotations.BitRepresentation.Util as BitRepresentation
import qualified Clash.Core.Type                          as C
import           Clash.Core.Name                          (nameOcc)
import qualified Clash.Netlist.Types                      as Netlist
import           Clash.Util                               (curLoc)
import qualified Data.Text as T                           (pack)

-- Convert Core type to BitRepresentation type
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
  return $ ConstTy' (nameOcc name)
coreToType' (C.LitTy (C.NumTy n)) =
  return $ LitTy' n
coreToType' (C.LitTy (C.SymTy lit)) =
  return $ SymLitTy' (T.pack lit)
coreToType' e =
  Left $ $(curLoc) ++ "Unexpected type: " ++ show e


bitToBit
  :: BitRepresentation.Bit
  -> Netlist.Bit
bitToBit BitRepresentation.H = Netlist.H
bitToBit BitRepresentation.L = Netlist.L
bitToBit BitRepresentation.U = Netlist.U

-- | Converts a list of /BitRepresentation.Bit/s to their Netlist counterpart.
bitsToBits
  :: [BitRepresentation.Bit]
  -> [Netlist.Bit]
bitsToBits = map bitToBit
