{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.GHC.NetlistTypes
  (ghcTypeToHWType)
where

import Data.Coerce                      (coerce)
import Data.Functor.Identity            (Identity (..))
import Data.Text                        (pack)
import Control.Monad.Trans.Except
  (ExceptT (..), mapExceptT, runExceptT, throwE)

import Clash.Core.DataCon               (DataCon (..))
import Clash.Core.Name                  (Name (..))
import Clash.Core.Pretty                (showPpr)
import Clash.Core.TyCon                 (TyConMap, tyConDataCons)
import Clash.Core.Type
  (LitTy (..), Type (..), TypeView (..), coreView1, tyView)
import Clash.Core.Util                  (tyNatSize)
import Clash.Netlist.Util               (coreTypeToHWType, stripFiltered)
import Clash.Netlist.Types
  (HWType(..), FilteredHWType(..), PortDirection (..))
import Clash.Signal.Internal            (ClockKind (..), ResetKind (..))
import Clash.Unique                     (lookupUniqMap')
import Clash.Util                       (curLoc)

import Clash.Annotations.BitRepresentation.Internal
  (CustomReprs)

ghcTypeToHWType
  :: Int
  -- ^ Integer width. The width Clash assumes an Integer to be (instead of it
  -- begin an arbitrarily large, runtime sized construct).
  -> Bool
  -- ^ Float support
  -> CustomReprs
  -- ^ Custom bit representations
  -> TyConMap
  -- ^ Type constructor map
  -> Type
  -- ^ Type to convert to HWType
  -> Maybe (Either String FilteredHWType)
ghcTypeToHWType iw floatSupport = go
  where
    returnN t = return (FilteredHWType t [])

    go reprs m (AnnType attrs typ) = runExceptT $ do
      FilteredHWType typ' areVoids <- ExceptT $ return $ coreTypeToHWType go reprs m typ
      return (FilteredHWType (Annotated attrs typ') areVoids)

    go reprs m ty@(tyView -> TyConApp tc args) = runExceptT $
      case nameOcc tc of
        "GHC.Int.Int8"                  -> returnN (Signed 8)
        "GHC.Int.Int16"                 -> returnN (Signed 16)
        "GHC.Int.Int32"                 -> returnN (Signed 32)
        "GHC.Int.Int64"                 ->
          if iw < 64
             then case tyConDataCons (m `lookupUniqMap'` tc) of
                    [dc] -> case dcArgTys dc of
                      [tyView -> TyConApp nm _]
                        | nameOcc nm == "GHC.Prim.Int#"   ->
                            throwE $ unlines ["Int64 not supported in forced 32-bit mode on a 64-bit machine."
                                             ,"Run Clash with `-fclash-intwidth=64`."
                                             ]
                        | nameOcc nm == "GHC.Prim.Int64#" ->
                            returnN (Signed 64)
                      _  -> throwE $ $(curLoc) ++ "Int64 DC has unexpected amount of arguments"
                    _    -> throwE $ $(curLoc) ++ "Int64 TC has unexpected amount of DCs"
             else returnN (Signed 64)
        "GHC.Word.Word8"                -> returnN (Unsigned 8)
        "GHC.Word.Word16"               -> returnN (Unsigned 16)
        "GHC.Word.Word32"               -> returnN (Unsigned 32)
        "GHC.Word.Word64"               ->
          if iw < 64
             then case tyConDataCons (m `lookupUniqMap'` tc) of
                    [dc] -> case dcArgTys dc of
                      [tyView -> TyConApp nm _]
                        | nameOcc nm == "GHC.Prim.Word#"   ->
                            throwE $ unlines ["Word64 not supported in forced 32-bit mode on a 64-bit machine."
                                             ,"Run Clash with `-fclash-intwidth=64`."
                                             ]
                        | nameOcc nm == "GHC.Prim.Word64#" ->
                            returnN (Unsigned 64)
                      _  -> throwE $ $(curLoc) ++ "Word64 DC has unexpected amount of arguments"
                    _    -> throwE $ $(curLoc) ++ "Word64 TC has unexpected amount of DCs"
             else returnN (Unsigned 64)
        "GHC.Integer.Type.Integer"      -> returnN (Signed iw)
        "GHC.Natural.Natural"           -> returnN (Unsigned iw)
        "GHC.Prim.Char#"                -> returnN (Unsigned 21)
        "GHC.Prim.Int#"                 -> returnN (Signed iw)
        "GHC.Prim.Word#"                -> returnN (Unsigned iw)
        "GHC.Prim.Int64#"               -> returnN (Signed 64)
        "GHC.Prim.Word64#"              -> returnN (Unsigned 64)
        "GHC.Prim.Float#" | floatSupport -> returnN (BitVector 32)
        "GHC.Prim.Double#" | floatSupport -> returnN (BitVector 64)
        "GHC.Prim.ByteArray#"           ->
          throwE $ "Can't translate type: " ++ showPpr ty

        "GHC.Types.Bool"                -> returnN Bool
        "GHC.Types.Float" | floatSupport-> returnN (BitVector 32)
        "GHC.Types.Double" | floatSupport -> returnN (BitVector 64)
        "GHC.Prim.~#"                   -> returnN (Void Nothing)

        "GHC.Prim.Any" -> returnN (Void Nothing)

        "Clash.Signal.Internal.Signal" ->
          ExceptT $ return $ coreTypeToHWType go reprs m (args !! 1)

        "Clash.Signal.BiSignal.BiSignalIn" -> do
          let [_, _, szTy] = args
          let fType ty1 = FilteredHWType ty1 []
          (fType . BiDirectional In . BitVector . fromInteger) <$>
            mapExceptT (Just .coerce) (tyNatSize m szTy)

        "Clash.Signal.BiSignal.BiSignalOut" -> do
          let [_, _, szTy] = args
          let fType ty1 = FilteredHWType ty1 []
          (fType . Void . Just . BiDirectional Out . BitVector . fromInteger) <$>
            mapExceptT (Just .coerce) (tyNatSize m szTy)

        "Clash.Signal.Internal.Clock"
          | [dom,clkKind] <- args
          -> do (nm,rate) <- domain m dom
                gated     <- clockKind m clkKind
                returnN (Clock (pack nm) rate gated)

        "Clash.Signal.Internal.Reset"
          | [dom,rstKind] <- args
          -> do (nm,rate)   <- domain m dom
                synchronous <- resetKind m rstKind
                returnN (Reset (pack nm) rate synchronous)

        "Clash.Sized.Internal.BitVector.Bit" -> returnN Bit

        "Clash.Sized.Internal.BitVector.BitVector" -> do
          n <- mapExceptT (Just . coerce) (tyNatSize m (head args))
          case n of
            0 -> returnN (Void (Just (BitVector (fromInteger n))))
            _ -> returnN (BitVector (fromInteger n))

        "Clash.Sized.Internal.Index.Index" -> do
          n <- mapExceptT (Just . coerce) (tyNatSize m (head args))
          if n < 2
             then returnN (Void (Just (Index (fromInteger n))))
             else returnN (Index (fromInteger n))

        "Clash.Sized.Internal.Signed.Signed" -> do
          n <- mapExceptT (Just . coerce) (tyNatSize m (head args))
          if n == 0
             then returnN (Void (Just (Signed (fromInteger n))))
             else returnN (Signed (fromInteger n))

        "Clash.Sized.Internal.Unsigned.Unsigned" -> do
          n <- mapExceptT (Just .coerce) (tyNatSize m (head args))
          if n == 0
             then returnN (Void (Just (Unsigned (fromInteger n))))
             else returnN (Unsigned (fromInteger n))

        "Clash.Sized.Vector.Vec" -> do
          let [szTy,elTy] = args
          sz0     <- mapExceptT (Just . coerce) (tyNatSize m szTy)
          fElHWTy <- ExceptT $ return $ coreTypeToHWType go reprs m elTy

          -- Treat Vec as a product type with a single constructor and N
          -- constructor fields.
          let sz1    = fromInteger sz0 :: Int
              elHWTy = stripFiltered fElHWTy

          let
            (isVoid, vecHWTy) =
              case elHWTy of
                Void {}      -> (True, Void (Just (Vector sz1 elHWTy)))
                _ | sz1 == 0 -> (True, Void (Just (Vector sz1 elHWTy)))
                _            -> (False, Vector sz1 elHWTy)

          let filtered = [replicate sz1 (isVoid, fElHWTy)]
          return (FilteredHWType vecHWTy filtered)

        "Clash.Sized.RTree.RTree" -> do
          let [szTy,elTy] = args
          sz0     <- mapExceptT (Just . coerce) (tyNatSize m szTy)
          fElHWTy <- ExceptT $ return $ coreTypeToHWType go reprs m elTy

          -- Treat RTree as a product type with a single constructor and 2^N
          -- constructor fields.
          let sz1    = fromInteger sz0 :: Int
              elHWTy = stripFiltered fElHWTy

          let
            (isVoid, vecHWTy) =
              case elHWTy of
                Void {} -> (True, Void (Just (RTree sz1 elHWTy)))
                _       -> (False, RTree sz1 elHWTy)

          let filtered = [replicate (2^sz1) (isVoid, fElHWTy)]
          return (FilteredHWType vecHWTy filtered)

        "String" -> returnN String
        "GHC.Types.[]" -> case tyView (head args) of
          (TyConApp (nameOcc -> "GHC.Types.Char") []) -> returnN String
          _ -> throwE $ "Can't translate type: " ++ showPpr ty

        _ -> ExceptT Nothing

    go _ _ _ = Nothing

domain
  :: TyConMap
  -> Type
  -> ExceptT String Maybe (String,Integer)
domain m (coreView1 m -> Just ty') = domain m ty'
domain m (tyView -> TyConApp tcNm [LitTy (SymTy nm),rateTy])
  | nameOcc tcNm == "Clash.Signal.Internal.Dom"
  = do rate <- mapExceptT (Just . coerce) (tyNatSize m rateTy)
       return (nm,rate)
domain _ ty = throwE $ "Can't translate domain: " ++ showPpr ty

clockKind
  :: TyConMap
  -> Type
  -> ExceptT String Maybe ClockKind
clockKind m (coreView1 m -> Just ty') = clockKind m ty'
clockKind _ (tyView -> TyConApp tcNm [])
  | nameOcc tcNm == "Clash.Signal.Internal.Source"
  = return Source
  | nameOcc tcNm == "Clash.Signal.Internal.Gated"
  = return Gated
clockKind _ ty = throwE $ "Can't translate ClockKind" ++ showPpr ty

resetKind
  :: TyConMap
  -> Type
  -> ExceptT String Maybe ResetKind
resetKind m (coreView1 m -> Just ty') = resetKind m ty'
resetKind _ (tyView -> TyConApp tcNm [])
  | nameOcc tcNm == "Clash.Signal.Internal.Synchronous"
  = return Synchronous
  | nameOcc tcNm == "Clash.Signal.Internal.Asynchronous"
  = return Asynchronous
resetKind _ ty = throwE $ "Can't translate ResetKind" ++ showPpr ty
