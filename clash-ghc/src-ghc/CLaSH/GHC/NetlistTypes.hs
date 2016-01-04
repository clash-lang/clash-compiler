{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module CLaSH.GHC.NetlistTypes
  (ghcTypeToHWType)
where

#include "MachDeps.h"

import Data.HashMap.Strict              (HashMap,(!))
import Control.Monad.Trans.Except       (ExceptT (..), runExceptT)
import Unbound.Generics.LocallyNameless (name2String)

import CLaSH.Core.Pretty                (showDoc)
import CLaSH.Core.TyCon                 (TyCon (..), TyConName)
import CLaSH.Core.Type                  (LitTy (..), Type (..), TypeView (..),
                                         findFunSubst, tyView)
import CLaSH.Netlist.Util               (coreTypeToHWType)
import CLaSH.Netlist.Types              (HWType(..))
import CLaSH.Util

ghcTypeToHWType :: HashMap TyConName TyCon
                -> Type
                -> Maybe (Either String HWType)
ghcTypeToHWType m ty@(tyView -> TyConApp tc args) = runExceptT $
  case name2String tc of
    "Integer"                       -> return Integer
    "Int"                           -> return (Signed WORD_SIZE_IN_BITS)
    "GHC.Int.Int8"                  -> return (Signed 8)
    "GHC.Int.Int16"                 -> return (Signed 16)
    "GHC.Int.Int32"                 -> return (Signed 32)
    "GHC.Int.Int64"                 -> return (Signed 64)
    "GHC.Word.Word8"                -> return (Unsigned 8)
    "GHC.Word.Word16"               -> return (Unsigned 16)
    "GHC.Word.Word32"               -> return (Unsigned 32)
    "GHC.Word.Word64"               -> return (Unsigned 64)
    "GHC.Integer.Type.Integer"      -> return Integer
    "GHC.Prim.Char#"                -> return (Signed 21)
    "GHC.Prim.Int#"                 -> return (Signed WORD_SIZE_IN_BITS)
    "GHC.Prim.Word#"                -> return (Unsigned WORD_SIZE_IN_BITS)
    "GHC.Prim.ByteArray#"           ->
      fail $ "Can't translate type: " ++ showDoc ty

    "GHC.Types.Bool"                -> return Bool
    "GHC.Prim.~#"                   ->
      fail $ "Can't translate type: " ++ showDoc ty

    "CLaSH.Signal.Internal.Signal'" ->
      ExceptT $ return $ coreTypeToHWType ghcTypeToHWType m (args !! 1)

    "CLaSH.Sized.Internal.BitVector.BitVector" ->
      BitVector <$> tyNatSize m (head args)

    "CLaSH.Sized.Internal.Index.Index" ->
      Index <$> tyNatSize m (head args)

    "CLaSH.Sized.Internal.Signed.Signed" ->
      Signed   <$> tyNatSize m (head args)

    "CLaSH.Sized.Internal.Unsigned.Unsigned" ->
      Unsigned <$> tyNatSize m (head args)

    "CLaSH.Sized.Vector.Vec" -> do
      let [szTy,elTy] = args
      sz     <- tyNatSize m szTy
      elHWTy <- ExceptT $ return $ coreTypeToHWType ghcTypeToHWType m elTy
      return $ Vector sz elHWTy

    "String" -> return String
    "GHC.Types.[]" -> case tyView (head args) of
      (TyConApp (name2String -> "GHC.Types.Char") []) -> return String
      _ -> fail $ "Can't translate type: " ++ showDoc ty

    _ -> case m ! tc of
           -- TODO: Remove this conversion
           -- The current problem is that type-functions are not reduced by the GHC -> Core
           -- transformation process, and so end up here. Once a fix has been found for
           -- this problem remove this dirty hack.
           FunTyCon {tyConSubst = tcSubst} -> case findFunSubst tcSubst args of
             Just ty' -> ExceptT $ return $ coreTypeToHWType ghcTypeToHWType m ty'
             _ -> ExceptT Nothing
           _ -> ExceptT Nothing

ghcTypeToHWType _ _ = Nothing

tyNatSize :: HashMap TyConName TyCon
          -> Type
          -> ExceptT String Maybe Int
tyNatSize _ (LitTy (NumTy i)) = return i
tyNatSize m ty@(tyView -> TyConApp tc [ty1,ty2]) = case name2String tc of
  "GHC.TypeLits.+" -> (+) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "GHC.TypeLits.*" -> (*) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "GHC.TypeLits.^" -> (^) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "GHC.TypeLits.-" -> (-) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "CLaSH.Promoted.Ord.Max" -> max <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "CLaSH.Promoted.Ord.Min" -> min <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "GHC.TypeLits.Extra.CLog" -> do
    i1' <- tyNatSize m ty1
    i2' <- tyNatSize m ty2
    if (i1' > 1 && i2' > 0)
       then return (ceiling (logBase (fromIntegral i1' :: Double)
                                     (fromIntegral i2' :: Double)))
       else fail $ $(curLoc) ++ "Can't convert: " ++ show ty
  "GHC.TypeLits.Extra.GCD" -> gcd <$> tyNatSize m ty1 <*> tyNatSize m ty2
  _ -> fail $ $(curLoc) ++ "Can't convert tyNatOp: " ++ show tc
-- TODO: Remove this conversion
-- The current problem is that type-functions are not reduced by the GHC -> Core
-- transformation process, and so end up here. Once a fix has been found for
-- this problem remove this dirty hack.
tyNatSize tcm ty@(tyView -> TyConApp tc tys) = do
  case tcm ! tc of
    FunTyCon {tyConSubst = tcSubst} -> case findFunSubst tcSubst tys of
      Just ty' -> tyNatSize tcm ty'
      _ -> fail $ $(curLoc) ++ "Can't convert tyNat: " ++ show ty
    _ -> fail $ $(curLoc) ++ "Can't convert tyNat: " ++ show ty

tyNatSize _ t = fail $ $(curLoc) ++ "Can't convert tyNat: " ++ show t
