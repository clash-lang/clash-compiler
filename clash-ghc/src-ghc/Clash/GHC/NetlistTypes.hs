{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.GHC.NetlistTypes
  (ghcTypeToHWType)
where

import Data.Coerce                      (coerce)
import Data.Functor.Identity            (Identity (..))
import Data.HashMap.Strict              (HashMap,(!))
import Data.Text.Lazy                   (pack)
import Control.Monad.Trans.Except
  (ExceptT (..), mapExceptT, runExceptT, throwE)

import Clash.Core.DataCon               (DataCon (..))
import Clash.Core.Name                  (Name (..), name2String)
import Clash.Core.Pretty                (showDoc)
import Clash.Core.TyCon                 (TyCon (..), TyConOccName, tyConDataCons)
import Clash.Core.Type
  (LitTy (..), Type (..), TypeView (..), coreView, tyView)
import Clash.Core.Util                  (tyNatSize)
import Clash.Netlist.Util               (coreTypeToHWType)
import Clash.Netlist.Types              (HWType(..), PortDirection (..))
import Clash.Signal.Internal            (ClockKind (..), ResetKind (..))
import Clash.Util                       (curLoc)

import Clash.Annotations.BitRepresentation.Internal
  (CustomReprs)

ghcTypeToHWType
  :: Int
  -> Bool
  -> CustomReprs
  -> HashMap TyConOccName TyCon
  -> Bool
  -> Type
  -> Maybe (Either String HWType)
ghcTypeToHWType iw floatSupport = go
  where
    go reprs m keepVoid (AnnType attrs typ) = runExceptT $ do
      typ' <- ExceptT $ return $ coreTypeToHWType go reprs m keepVoid typ
      return $ Annotated attrs typ'

    go reprs m keepVoid ty@(tyView -> TyConApp tc args) = runExceptT $
      case name2String tc of
        "GHC.Int.Int8"                  -> return (Signed 8)
        "GHC.Int.Int16"                 -> return (Signed 16)
        "GHC.Int.Int32"                 -> return (Signed 32)
        "GHC.Int.Int64"                 ->
          if iw < 64
             then case tyConDataCons (m ! nameOcc tc) of
                    [dc] -> case dcArgTys dc of
                      [tyView -> TyConApp nm _]
                        | name2String nm == "GHC.Prim.Int#"   ->
                            throwE $ unlines ["Int64 not supported in forced 32-bit mode on a 64-bit machine."
                                             ,"Run Clash with `-fclash-intwidth=64`."
                                             ]
                        | name2String nm == "GHC.Prim.Int64#" ->
                            return (Signed 64)
                      _  -> throwE $ $(curLoc) ++ "Int64 DC has unexpected amount of arguments"
                    _    -> throwE $ $(curLoc) ++ "Int64 TC has unexpected amount of DCs"
             else return (Signed 64)
        "GHC.Word.Word8"                -> return (Unsigned 8)
        "GHC.Word.Word16"               -> return (Unsigned 16)
        "GHC.Word.Word32"               -> return (Unsigned 32)
        "GHC.Word.Word64"               ->
          if iw < 64
             then case tyConDataCons (m ! nameOcc tc) of
                    [dc] -> case dcArgTys dc of
                      [tyView -> TyConApp nm _]
                        | name2String nm == "GHC.Prim.Word#"   ->
                            throwE $ unlines ["Word64 not supported in forced 32-bit mode on a 64-bit machine."
                                             ,"Run Clash with `-fclash-intwidth=64`."
                                             ]
                        | name2String nm == "GHC.Prim.Word64#" ->
                            return (Unsigned 64)
                      _  -> throwE $ $(curLoc) ++ "Word64 DC has unexpected amount of arguments"
                    _    -> throwE $ $(curLoc) ++ "Word64 TC has unexpected amount of DCs"
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
          throwE $ "Can't translate type: " ++ showDoc ty

        "GHC.Types.Bool"                -> return Bool
        "GHC.Types.Float" | floatSupport-> return (BitVector 32)
        "GHC.Types.Double" | floatSupport -> return (BitVector 64)
        "GHC.Prim.~#"                   -> return (Void Nothing)

        "GHC.Prim.Any" -> return (Void Nothing)

        "Clash.Signal.Internal.Signal" ->
          ExceptT $ return $ coreTypeToHWType go reprs m keepVoid (args !! 1)

        "Clash.Signal.BiSignal.BiSignalIn" -> do
          let [_, _, szTy] = args
          (BiDirectional In . BitVector . fromInteger) <$> mapExceptT (Just .coerce) (tyNatSize m szTy)

        "Clash.Signal.BiSignal.BiSignalOut" -> do
          let [_, _, szTy] = args
          (Void . Just . BiDirectional Out . BitVector . fromInteger) <$>
            mapExceptT (Just .coerce) (tyNatSize m szTy)

        "Clash.Signal.Internal.Clock"
          | [dom,clkKind] <- args
          -> do (nm,rate) <- domain m dom
                gated     <- clockKind m clkKind
                return (Clock (pack nm) rate gated)

        "Clash.Signal.Internal.Reset"
          | [dom,rstKind] <- args
          -> do (nm,rate)   <- domain m dom
                synchronous <- resetKind m rstKind
                return (Reset (pack nm) rate synchronous)

        "Clash.Sized.Internal.BitVector.Bit" -> return Bit

        "Clash.Sized.Internal.BitVector.BitVector" -> do
          n <- mapExceptT (Just . coerce) (tyNatSize m (head args))
          case n of
            0 -> return (Void (Just (BitVector (fromInteger n))))
            _ -> return (BitVector (fromInteger n))

        "Clash.Sized.Internal.Index.Index" -> do
          n <- mapExceptT (Just . coerce) (tyNatSize m (head args))
          if n < 2
             then return (Void (Just (Index (fromInteger n))))
             else return (Index (fromInteger n))

        "Clash.Sized.Internal.Signed.Signed" -> do
          n <- mapExceptT (Just . coerce) (tyNatSize m (head args))
          if n == 0
             then return (Void (Just (Signed (fromInteger n))))
             else return (Signed (fromInteger n))

        "Clash.Sized.Internal.Unsigned.Unsigned" -> do
          n <- mapExceptT (Just .coerce) (tyNatSize m (head args))
          if n == 0
             then return (Void (Just (Unsigned (fromInteger n))))
             else return (Unsigned (fromInteger n))

        "Clash.Sized.Vector.Vec" -> do
          let [szTy,elTy] = args
          sz     <- mapExceptT (Just . coerce) (tyNatSize m szTy)
          elHWTy <- ExceptT $ return $ coreTypeToHWType go reprs m keepVoid elTy
          case elHWTy of
            Void {}     -> return (Void (Just (Vector (fromInteger sz) elHWTy)))
            _ | sz == 0 -> return (Void (Just (Vector (fromInteger sz) elHWTy)))
            _           -> return $ Vector (fromInteger sz) elHWTy

        "Clash.Sized.RTree.RTree" -> do
          let [szTy,elTy] = args
          sz     <- mapExceptT (Just . coerce) (tyNatSize m szTy)
          elHWTy <- ExceptT $ return $ coreTypeToHWType go reprs m keepVoid elTy
          case elHWTy of
            Void {} -> return (Void (Just (RTree (fromInteger sz) elHWTy)))
            _       -> return $ RTree (fromInteger sz) elHWTy

        "String" -> return String
        "GHC.Types.[]" -> case tyView (head args) of
          (TyConApp (name2String -> "GHC.Types.Char") []) -> return String
          _ -> throwE $ "Can't translate type: " ++ showDoc ty

        _ -> ExceptT Nothing

    go _ _ _ _ = Nothing

domain
  :: HashMap TyConOccName TyCon
  -> Type
  -> ExceptT String Maybe (String,Integer)
domain m (coreView m -> Just ty') = domain m ty'
domain m (tyView -> TyConApp tcNm [LitTy (SymTy nm),rateTy])
  | name2String tcNm == "Clash.Signal.Internal.Dom"
  = do rate <- mapExceptT (Just . coerce) (tyNatSize m rateTy)
       return (nm,rate)
domain _ ty = throwE $ "Can't translate domain: " ++ showDoc ty

clockKind
  :: HashMap TyConOccName TyCon
  -> Type
  -> ExceptT String Maybe ClockKind
clockKind m (coreView m -> Just ty') = clockKind m ty'
clockKind _ (tyView -> TyConApp tcNm [])
  | name2String tcNm == "Clash.Signal.Internal.Source"
  = return Source
  | name2String tcNm == "Clash.Signal.Internal.Gated"
  = return Gated
clockKind _ ty = throwE $ "Can't translate ClockKind" ++ showDoc ty

resetKind
  :: HashMap TyConOccName TyCon
  -> Type
  -> ExceptT String Maybe ResetKind
resetKind m (coreView m -> Just ty') = resetKind m ty'
resetKind _ (tyView -> TyConApp tcNm [])
  | name2String tcNm == "Clash.Signal.Internal.Synchronous"
  = return Synchronous
  | name2String tcNm == "Clash.Signal.Internal.Asynchronous"
  = return Asynchronous
resetKind _ ty = throwE $ "Can't translate ResetKind" ++ showDoc ty
