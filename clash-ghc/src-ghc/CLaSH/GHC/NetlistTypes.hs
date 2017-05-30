{-|
  Copyright   :  (C) 2013-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module CLaSH.GHC.NetlistTypes
  (ghcTypeToHWType)
where

import Data.Coerce                      (coerce)
import Data.Functor.Identity            (Identity (..))
import Data.HashMap.Strict              (HashMap,(!))
import Control.Monad.Trans.Except       (ExceptT (..), mapExceptT, runExceptT)
import Unbound.Generics.LocallyNameless (name2String)

import CLaSH.Core.DataCon               (DataCon (..))
import CLaSH.Core.Pretty                (showDoc)
import CLaSH.Core.TyCon                 (TyCon (..), TyConName, tyConDataCons)
import CLaSH.Core.Type                  (Type (..), TypeView (..), tyView)
import CLaSH.Core.Util                  (tyNatSize)
import CLaSH.Netlist.Util               (coreTypeToHWType)
import CLaSH.Netlist.Types              (HWType(..))
import CLaSH.Util                       (curLoc)

ghcTypeToHWType :: Int
                -> Bool
                -> HashMap TyConName TyCon
                -> Type
                -> Maybe (Either String HWType)
ghcTypeToHWType iw floatSupport = go
  where
    go m ty@(tyView -> TyConApp tc args) = runExceptT $
      case name2String tc of
        "GHC.Int.Int8"                  -> return (Signed 8)
        "GHC.Int.Int16"                 -> return (Signed 16)
        "GHC.Int.Int32"                 -> return (Signed 32)
        "GHC.Int.Int64"                 ->
          if iw < 64
             then case tyConDataCons (m ! tc) of
                    [dc] -> case dcArgTys dc of
                      [tyView -> TyConApp nm _]
                        | name2String nm == "GHC.Prim.Int#"   ->
                            error $ unlines ["Int64 not supported in forced 32-bit mode on a 64-bit machine."
                                            ,"Run CLaSH with `-clash-intwidth=64`."
                                            ]
                        | name2String nm == "GHC.Prim.Int64#" ->
                            return (Signed 64)
                      _  -> error $ $(curLoc) ++ "Int64 DC has unexpected amount of arguments"
                    _    -> error $ $(curLoc) ++ "Int64 TC has unexpected amount of DCs"
             else return (Signed 64)
        "GHC.Word.Word8"                -> return (Unsigned 8)
        "GHC.Word.Word16"               -> return (Unsigned 16)
        "GHC.Word.Word32"               -> return (Unsigned 32)
        "GHC.Word.Word64"               ->
          if iw < 64
             then case tyConDataCons (m ! tc) of
                    [dc] -> case dcArgTys dc of
                      [tyView -> TyConApp nm _]
                        | name2String nm == "GHC.Prim.Word#"   ->
                            error $ unlines ["Word64 not supported in forced 32-bit mode on a 64-bit machine."
                                            ,"Run CLaSH with `-clash-intwidth=64`."
                                            ]
                        | name2String nm == "GHC.Prim.Word64#" ->
                            return (Unsigned 64)
                      _  -> error $ $(curLoc) ++ "Word64 DC has unexpected amount of arguments"
                    _    -> error $ $(curLoc) ++ "Word64 TC has unexpected amount of DCs"
             else return (Unsigned 64)
        "GHC.Integer.Type.Integer"      -> return (Signed iw)
        "GHC.Natural.Natural"           -> return (Unsigned iw)
        "GHC.Prim.Char#"                -> return (Unsigned 21)
        "GHC.Prim.Int#"                 -> return (Signed iw)
        "GHC.Prim.Word#"                -> return (Unsigned iw)
        "GHC.Prim.Int64#"               -> return (Signed 64)
        "GHC.Prim.Word64#"              -> return (Unsigned 64)
        "GHC.Prim.Float#" | floatSupport -> return (BitVector 32)
        "GHC.Prim.Double#" | floatSupport -> return (BitVector 64)
        "GHC.Prim.ByteArray#"           ->
          fail $ "Can't translate type: " ++ showDoc ty

        "GHC.Types.Bool"                -> return Bool
        "GHC.Types.Float" | floatSupport-> return (BitVector 32)
        "GHC.Types.Double" | floatSupport -> return (BitVector 64)
        "GHC.Prim.~#"                   -> return (Sum "GHC.Prim.~#" ["GHC.Types.Eq#"])

        "GHC.Prim.Any" -> return (BitVector 1)

        "CLaSH.Signal.Internal.Signal'" ->
          ExceptT $ return $ coreTypeToHWType go m (args !! 1)

        "CLaSH.Sized.Internal.BitVector.BitVector" ->
          (BitVector . fromInteger) <$> mapExceptT (Just . coerce) (tyNatSize m (head args))

        "CLaSH.Sized.Internal.Index.Index" ->
          Index <$> mapExceptT (Just . coerce) (tyNatSize m (head args))

        "CLaSH.Sized.Internal.Signed.Signed" ->
          (Signed . fromInteger) <$> mapExceptT (Just . coerce) (tyNatSize m (head args))

        "CLaSH.Sized.Internal.Unsigned.Unsigned" ->
          (Unsigned . fromInteger) <$> mapExceptT (Just . coerce) (tyNatSize m (head args))

        "CLaSH.Sized.Vector.Vec" -> do
          let [szTy,elTy] = args
          sz     <- mapExceptT (Just . coerce) (tyNatSize m szTy)
          elHWTy <- ExceptT $ return $ coreTypeToHWType go m elTy
          return $ Vector (fromInteger sz) elHWTy

        "CLaSH.Sized.RTree.RTree" -> do
          let [szTy,elTy] = args
          sz     <- mapExceptT (Just . coerce) (tyNatSize m szTy)
          elHWTy <- ExceptT $ return $ coreTypeToHWType go m elTy
          return $ RTree (fromInteger sz) elHWTy

        "String" -> return String
        "GHC.Types.[]" -> case tyView (head args) of
          (TyConApp (name2String -> "GHC.Types.Char") []) -> return String
          _ -> fail $ "Can't translate type: " ++ showDoc ty

        _ -> ExceptT Nothing

    go _ _ = Nothing
