{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2023, Myrtle Software Ltd,
                     2021-2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.NetlistTypes
  (ghcTypeToHWType)
where

import Data.Coerce                      (coerce)
import Data.Functor.Identity            (Identity (..))
import Data.Maybe                       (fromMaybe)
import Data.Text                        (pack)
import Data.Text.Extra                  (showt)
import Control.Monad.State.Strict       (State)
import Control.Monad.Trans.Except
  (Except, ExceptT (..), mapExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe        (MaybeT (..))
import GHC.Exts
  ( Addr#, ByteArray#, Char#, Double#, Float#
  , Int#, Int8#, Int16#, Int32#, Int64#
  , List
  , Word#, Word8#, Word16#, Word32#, Word64#
  )
import GHC.Int                          (Int8, Int16, Int32, Int64)
import GHC.Natural                      (Natural)
import qualified GHC.STRef
import qualified GHC.Stack.Types
import GHC.Word                         (Word8, Word16, Word32, Word64)
import Language.Haskell.TH.Syntax       (showName)

import Clash.Core.Basic (LitTy (..))
import Clash.Core.DataCon               (dcSig)
import Clash.Core.Name                  (Name (..))
import Clash.Core.Pretty                (showPpr)
import Clash.Core.TyCon                 (tyConDataCons)
import Clash.Core.DataCon        (DataConSig (..), dataConInstExtTyVars)
import Clash.Core.Syntax         (Distinct, Scope, Type (..))
import Clash.Core.Type
  (TypeView (..), coreView, coreView1, splitTyConAppM, tyNatSize, tyView)
import Clash.Core.TypeEnv        (TypeEnv, findTyCon)
import Clash.Netlist.Util               (coreTypeToHWType, stripFiltered)
import Clash.Netlist.Types
  (HWType(..), HWMap, HWTypeTranslator (..), FilteredHWType(..), PortDirection (..))
import Clash.Signal.Internal
  (ResetPolarity(..), ActiveEdge(..), ResetKind(..)
  ,InitBehavior(..))
import Clash.Util                       (curLoc)

import Clash.Annotations.BitRepresentation.Internal
  (CustomReprs)
import Clash.Signal.Internal (KnownDomain)

-- | The translator of the types that Clash knows about, see 'HWTypeTranslator'
ghcTypeToHWType
  :: Int
  -- ^ Integer width. The width Clash assumes an Integer to be (instead of it
  -- begin an arbitrarily large, runtime sized construct).
  -> HWTypeTranslator
ghcTypeToHWType iw = tt
  where
    tt = HWTypeTranslator go

    -- returnN :: HWType ->
    returnN t = return (FilteredHWType t [])

    -- The type constructors of the fields of a data constructor, for those
    -- that are applications of type constructors
    fieldTyCons dc = case dcSig dc of
      DataConSig _ _ fields -> map (fmap fst . splitTyConAppM) fields

    go
      :: Distinct n
      => CustomReprs
      -> TypeEnv
      -> Scope n
      -> Type n
      -> State HWMap (Maybe (Either String FilteredHWType))
    go reprs m sc (AnnType attrs typ) = fmap Just . runExceptT $ do
      FilteredHWType typ' areVoids <- ExceptT $ coreTypeToHWType tt reprs m sc typ
      return (FilteredHWType (Annotated attrs typ') areVoids)

    go reprs m sc ty@(tyView -> TyConApp tc args) = runMaybeT . runExceptT $
      let occ = nameOcc tc in
      if | occ == showt ''Int8                  -> returnN (Signed 8)
         | occ == showt ''Int16                 -> returnN (Signed 16)
         | occ == showt ''Int32                 -> returnN (Signed 32)
         | occ == showt ''Int64                 ->
             if iw < 64
                then case tyConDataCons (findTyCon m tc) of
                       [dc] -> case fieldTyCons dc of
                         [Just nm]
                           | nameOcc nm == showt ''Int#   ->
                               throwE $ unlines ["Int64 not supported in forced 32-bit mode on a 64-bit machine."
                                                ,"Run Clash with `-fclash-intwidth=64`."
                                                ]
                           | nameOcc nm == showt ''Int64# ->
                               returnN (Signed 64)
                         _  -> throwE $ $(curLoc) ++ "Int64 DC has unexpected amount of arguments"
                       _    -> throwE $ $(curLoc) ++ "Int64 TC has unexpected amount of DCs"
                else returnN (Signed 64)
         | occ == showt ''Word8                 -> returnN (Unsigned 8)
         | occ == showt ''Word16                -> returnN (Unsigned 16)
         | occ == showt ''Word32                -> returnN (Unsigned 32)
         | occ == showt ''Word64                ->
             if iw < 64
                then case tyConDataCons (findTyCon m tc) of
                       [dc] -> case fieldTyCons dc of
                         [Just nm]
                           | nameOcc nm == showt ''Word#   ->
                               throwE $ unlines ["Word64 not supported in forced 32-bit mode on a 64-bit machine."
                                                ,"Run Clash with `-fclash-intwidth=64`."
                                                ]
                           | nameOcc nm == showt ''Word64# ->
                               returnN (Unsigned 64)
                         _  -> throwE $ $(curLoc) ++ "Word64 DC has unexpected amount of arguments"
                       _    -> throwE $ $(curLoc) ++ "Word64 TC has unexpected amount of DCs"
                else returnN (Unsigned 64)
         | occ == showt ''Integer              -> returnN (Signed iw)
         | occ == showt ''Natural              -> returnN (Unsigned iw)
         | occ == showt ''Char#                -> returnN (Unsigned 21)
         | occ == showt ''Int#                 -> returnN (Signed iw)
         | occ == showt ''Word#                -> returnN (Unsigned iw)
         | occ == showt ''Int8#                -> returnN (Signed 8)
         | occ == showt ''Int16#               -> returnN (Signed 16)
         | occ == showt ''Int32#               -> returnN (Signed 32)
         | occ == showt ''Int64#               -> returnN (Signed 64)
         | occ == showt ''Word8#               -> returnN (Unsigned 8)
         | occ == showt ''Word16#              -> returnN (Unsigned 16)
         | occ == showt ''Word32#              -> returnN (Unsigned 32)
         | occ == showt ''Word64#              -> returnN (Unsigned 64)
         | occ == showt ''Float#               -> returnN (BitVector 32)
         | occ == showt ''Double#              -> returnN (BitVector 64)
         | occ == showt ''ByteArray#           ->
             throwE $ "Can't translate type: " ++ showPpr ty

         | occ == showt ''Bool                 -> returnN Bool
         | occ == showt ''Float                -> returnN (BitVector 32)
         | occ == showt ''Double               -> returnN (BitVector 64)
         -- '(~#)' is a wired-in equality primtype; the host module path
         -- changed from 'GHC.Prim' (<= 9.12) to 'GHC.Internal.Prim' (>= 9.14).
         | occ `elem` ["GHC.Prim.~#", "GHC.Internal.Prim.~#"]
                                               -> returnN (Void Nothing)

         | occ == "Clash.Signal.Internal.Signal" ->
             ExceptT $ MaybeT $ Just <$> coreTypeToHWType tt reprs m sc (args !! 1)

         | occ == "Clash.Signal.BiSignal.BiSignalIn" -> do
             szTy <- case args of
               [_, _, szTy] -> pure szTy
               _ -> throwE $ $(curLoc) ++ "BiSignalIn TC has unexpected amount of arguments"
             let fType ty1 = FilteredHWType ty1 []
             (fType . BiDirectional In . BitVector . fromInteger) <$>
               liftE (tyNatSize m sc szTy)

         | occ == "Clash.Signal.BiSignal.BiSignalOut" -> do
             szTy <- case args of
               [_, _, szTy] -> pure szTy
               _ -> throwE $ $(curLoc) ++ "BiSignalOut TC has unexpected amount of arguments"
             let fType ty1 = FilteredHWType ty1 []
             (fType . Void . Just . BiDirectional Out . BitVector . fromInteger) <$>
               liftE (tyNatSize m sc szTy)

         -- XXX: this is a hack to get a KnownDomain from a KnownConfiguration.
         -- Constraint-tuple type constructors are internal names that move
         -- between modules across GHC versions ('GHC.Classes' <= 9.12 vs
         -- 'GHC.Internal.Classes' >= 9.14); both forms appear here.
         | occ `elem` [ "GHC.Classes.(%,%)", "GHC.Internal.Classes.(%,%)"
                      , "GHC.Classes.CTuple2", "GHC.Internal.Classes.CTuple2"
                      ]
         , [arg0@(tyView -> TyConApp kdNm _), arg1] <- args
         , nameOcc kdNm == showt ''KnownDomain
         -> case tyView arg1 of
                TyConApp kdNm1 _
                  | nameOcc kdNm1 == showt ''KnownDomain
                  -> do let stripVoid (Void (Just t)) = t
                            stripVoid t = t
                        k1 <- (stripVoid . stripFiltered) <$> ExceptT (MaybeT (go reprs m sc arg0))
                        k2 <- (stripVoid . stripFiltered) <$> ExceptT (MaybeT (go reprs m sc arg1))
                        returnN (Void (Just (Product occ Nothing [k1,k2])))
                _ -> ExceptT (MaybeT (go reprs m sc arg0))

         | occ == "Clash.Signal.Internal.KnownDomain"
         -> case tyConDataCons (findTyCon m tc) of
               [dc] -> fromMaybe (ExceptT (MaybeT (pure Nothing))) $
                 dataConInstExtTyVars m sc dc args $ \sc1 _exts argTys ->
                 case argTys of
                 [_knownSymbol, _knownNat, tyView -> TyConApp _ [_,dom]] ->
                  case tyView (coreView m sc1 dom) of
                   TyConApp _ [tag0, period0, edge0, rstKind0, init0, polarity0] -> do
                     tag1      <- domTag m sc1 tag0
                     period1   <- domPeriod m sc1 period0
                     edge1     <- domEdge m sc1 edge0
                     rstKind1  <- domResetKind m sc1 rstKind0
                     init1     <- domInitBehavior m sc1 init0
                     polarity1 <- domResetPolarity m sc1 polarity0
                     let kd = KnownDomain (pack tag1) period1 edge1 rstKind1 init1 polarity1
                     returnN (Void (Just kd))
                   _ -> ExceptT (MaybeT (pure Nothing))
                 _ -> ExceptT (MaybeT (pure Nothing))
               _ -> ExceptT (MaybeT (pure Nothing))

         | occ == "Clash.Signal.Internal.Clock"
         , [tag0] <- args
         -> do
             tag1 <- domTag m sc tag0
             returnN (Clock (pack tag1))

         | occ == "Clash.Signal.Internal.ClockN"
         , [tag0] <- args
         -> do
             tag1 <- domTag m sc tag0
             returnN (ClockN (pack tag1))

         | occ == "Clash.Signal.Internal.Reset"
         , [tag0] <- args
         -> do
             tag1 <- domTag m sc tag0
             returnN (Reset (pack tag1))

         | occ == "Clash.Signal.Internal.Enable"
         , [tag0] <- args
         -> do
             tag1 <- domTag m sc tag0
             returnN (Enable (pack tag1))

         | occ == "Clash.Sized.Internal.BitVector.Bit" -> returnN Bit

         | occ == "Clash.Sized.Internal.BitVector.BitVector"
         , n0:_ <- args -> do
             n <- liftE (tyNatSize m sc n0)
             case n of
               0 -> returnN (Void (Just (BitVector (fromInteger n))))
               _ -> returnN (BitVector (fromInteger n))

         | occ == "Clash.Sized.Internal.Index.Index"
         , n0:_ <- args -> do
             n <- liftE (tyNatSize m sc n0)
             if n < 2
                then returnN (Void (Just (Index (fromInteger n))))
                else returnN (Index (fromInteger n))

         | occ == "Clash.Sized.Internal.Signed.Signed"
         , n0:_ <- args -> do
             n <- liftE (tyNatSize m sc n0)
             if n == 0
                then returnN (Void (Just (Signed (fromInteger n))))
                else returnN (Signed (fromInteger n))

         | occ == "Clash.Sized.Internal.Unsigned.Unsigned"
         , n0:_ <- args -> do
             n <- liftE (tyNatSize m sc n0)
             if n == 0
                then returnN (Void (Just (Unsigned (fromInteger n))))
                else returnN (Unsigned (fromInteger n))

         | occ == "Clash.Sized.Vector.Vec" -> case args of
             [szTy,elTy] -> do
               sz0     <- liftE (tyNatSize m sc szTy)
               fElHWTy <- ExceptT $ MaybeT $ Just <$> coreTypeToHWType tt reprs m sc elTy

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
             _ -> throwE $ $(curLoc) ++ "Vec TC has unexpected amount of arguments"

         | occ == "Clash.Explicit.BlockRam.Internal.MemBlob" -> case args of
             [nTy,mTy] -> do
               n0 <- liftE (tyNatSize m sc nTy)
               m0 <- liftE (tyNatSize m sc mTy)
               returnN (MemBlob (fromInteger n0) (fromInteger m0))
             _ -> throwE $ $(curLoc) ++ "MemBlob TC has unexpected amount of arguments"

         | occ == "Clash.Sized.RTree.RTree" -> case args of
             [szTy,elTy] -> do
               sz0     <- liftE (tyNatSize m sc szTy)
               fElHWTy <- ExceptT $ MaybeT $ Just <$> coreTypeToHWType tt reprs m sc elTy

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
             _ -> throwE $ $(curLoc) ++ "RTree TC has unexpected amount of arguments"

         | occ == "String" -> returnN String
         | occ == showt ''Addr# -> returnN String
         | occ `elem` [showt ''[], showt ''List]
         , a0:_ <- args -> case tyView a0 of
             (TyConApp (nameOcc -> nm) [])
               | nm == showt ''Char -> returnN String
             _ -> throwE $ "Can't translate type: " ++ showPpr ty

         -- To ensure that Clash doesn't get stuck working away callstacks that
         -- never end up being used in the generated HDL.
         | occ == showt ''GHC.Stack.Types.CallStack -> returnN (Void Nothing)

         | occ == "Clash.Explicit.SimIO.SimIO"
         , a0:_ <- args ->
             ExceptT $ MaybeT $ Just <$> coreTypeToHWType tt reprs m sc a0

         | occ == "Clash.Explicit.SimIO.File" -> returnN FileType

         | occ == "Clash.Explicit.SimIO.Reg" -> case args of
             [aTy] -> ExceptT (MaybeT (Just <$> coreTypeToHWType tt reprs m sc aTy))
             _ -> throwE $ $(curLoc) ++ "Reg TC has unexpected amount of arguments"

         | occ == showt ''GHC.STRef.STRef -> case args of
             [_,aTy] -> ExceptT (MaybeT (Just <$> coreTypeToHWType tt reprs m sc aTy))
             _ -> throwE $ $(curLoc) ++ "STRef TC has unexpected amount of arguments"

         -- Anything that's wrapped in SimOnly should be elided when we generate HDL
         | occ == "Clash.Magic.SimOnly" -> returnN (Void Nothing)

         | otherwise -> ExceptT (MaybeT (pure Nothing))

    go _ _ _ _ = pure Nothing

liftE
  :: Applicative m
  => Except e a
  -> ExceptT e (MaybeT m) a
liftE = mapExceptT (MaybeT . pure . Just . coerce)

domTag :: (Monad m, Distinct n) => TypeEnv -> Scope n -> Type n -> ExceptT String (MaybeT m) String
domTag m sc (coreView1 m sc -> Just ty) = domTag m sc ty
domTag _ _ (LitTy (SymTy tag)) = pure tag
domTag _ _ ty = throwE $ "Internal error. Cannot translate domain tag:\n" ++ showPpr ty

domPeriod :: (Monad m, Distinct n) => TypeEnv -> Scope n -> Type n -> ExceptT String (MaybeT m) Integer
domPeriod m sc (coreView1 m sc -> Just ty) = domPeriod m sc ty
domPeriod _ _ (LitTy (NumTy period)) = pure period
domPeriod _ _ ty = throwE $ "Internal error. Cannot translate domain period:\n" ++ showPpr ty

fromType
  :: (Monad m, Distinct n)
  => String
  -- ^ Name of type (for error reporting)
  -> [(String, a)]
  -- ^ [(Fully qualified constructor name, constructor value)
  -> TypeEnv
  -- ^ Constructor map (used to look through newtypes)
  -> Scope n
  -> Type n
  -- ^ Type representing some constructor
  -> ExceptT String (MaybeT m) a
fromType tyNm constrs m sc ty =
  case tyView (coreView m sc ty) of
    TyConApp tcNm [] ->
      go constrs (nameOcc tcNm)
    _ ->
      throwE $ "Can't translate " ++ tyNm ++ showPpr ty
 where
  go ((cName,c):cs) tcNm =
    if pack cName == tcNm then
      pure c
    else
      go cs tcNm
  go [] _ =
    throwE $ "Can't translate " ++ tyNm ++ showPpr ty

domEdge
  :: (Monad m, Distinct n)
  => TypeEnv
  -> Scope n
  -> Type n
  -> ExceptT String (MaybeT m) ActiveEdge
domEdge =
  fromType
    (showName ''ActiveEdge)
    [ (showName 'Rising, Rising)
    , (showName 'Falling, Falling) ]

domResetKind
  :: (Monad m, Distinct n)
  => TypeEnv
  -> Scope n
  -> Type n
  -> ExceptT String (MaybeT m) ResetKind
domResetKind =
  fromType
    (showName ''ResetKind)
    [ (showName 'Synchronous, Synchronous)
    , (showName 'Asynchronous, Asynchronous) ]

domInitBehavior
  :: (Monad m, Distinct n)
  => TypeEnv
  -> Scope n
  -> Type n
  -> ExceptT String (MaybeT m) InitBehavior
domInitBehavior =
  fromType
    (showName ''InitBehavior)
    [ (showName 'Defined, Defined)
    , (showName 'Unknown, Unknown) ]

domResetPolarity
  :: (Monad m, Distinct n)
  => TypeEnv
  -> Scope n
  -> Type n
  -> ExceptT String (MaybeT m) ResetPolarity
domResetPolarity =
  fromType
    (showName ''ResetPolarity)
    [ (showName 'ActiveHigh, ActiveHigh)
    , (showName 'ActiveLow, ActiveLow) ]
