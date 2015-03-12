{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module CLaSH.GHC.NetlistTypes
  (ghcTypeToHWType)
where

import Data.HashMap.Strict       (HashMap,(!))
import Control.Monad.Trans.Error (ErrorT(..))
import Unbound.Generics.LocallyNameless   (name2String)

import CLaSH.Core.Pretty         (showDoc)
import CLaSH.Core.TyCon          (TyCon (..), TyConName)
import CLaSH.Core.Type           (LitTy (..), Type (..), TypeView (..),
                                  findFunSubst, tyView)
import CLaSH.Netlist.Util        (coreTypeToHWType)
import CLaSH.Netlist.Types       (HWType(..))
import CLaSH.Util

ghcTypeToHWType :: HashMap TyConName TyCon
                -> Type
                -> Maybe (Either String HWType)
ghcTypeToHWType m ty@(tyView -> TyConApp tc args) = runErrorT $
  case name2String tc of
    "Int"                           -> return Integer
    "GHC.Integer.Type.Integer"      -> return Integer
    "GHC.Prim.Int#"                 -> return Integer
    "GHC.Types.Int"                 -> return Integer
    "GHC.Prim.ByteArray#"           ->
      fail $ "Can't translate type: " ++ showDoc ty

    "GHC.Types.Bool"                -> return Bool
    "GHC.Prim.~#"                   ->
      fail $ "Can't translate type: " ++ showDoc ty

    "CLaSH.Signal.Internal.Signal'" ->
      ErrorT $ return $ coreTypeToHWType ghcTypeToHWType m (args !! 1)

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
      elHWTy <- ErrorT $ return $ coreTypeToHWType ghcTypeToHWType m elTy
      return $ Vector sz elHWTy
    _ -> case m ! tc of
           -- TODO: Remove this conversion
           -- The current problem is that type-functions are not reduced by the GHC -> Core
           -- transformation process, and so end up here. Once a fix has been found for
           -- this problem remove this dirty hack.
           FunTyCon {tyConSubst = tcSubst} -> case findFunSubst tcSubst args of
             Just ty' -> ErrorT $ return $ coreTypeToHWType ghcTypeToHWType m ty'
             _ -> ErrorT Nothing
           _ -> ErrorT Nothing

ghcTypeToHWType _ _ = Nothing

tyNatSize :: HashMap TyConName TyCon
          -> Type
          -> ErrorT String Maybe Int
tyNatSize _ (LitTy (NumTy i)) = return i
tyNatSize m (tyView -> TyConApp tc [ty1,ty2]) = case name2String tc of
  "GHC.TypeLits.+" -> (+) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "GHC.TypeLits.*" -> (*) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "GHC.TypeLits.^" -> (^) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "GHC.TypeLits.-" -> (-) <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "CLaSH.Promoted.Ord.Max" -> max <$> tyNatSize m ty1 <*> tyNatSize m ty2
  "CLaSH.Promoted.Ord.Min" -> min <$> tyNatSize m ty1 <*> tyNatSize m ty2
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
