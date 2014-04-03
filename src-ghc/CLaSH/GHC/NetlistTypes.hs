{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module CLaSH.GHC.NetlistTypes
  (ghcTypeToHWType)
where

import Data.HashMap.Strict       (HashMap)
import Control.Monad.Trans.Error (ErrorT(..))
import Unbound.LocallyNameless   (name2String)

import CLaSH.Core.Pretty         (showDoc)
import CLaSH.Core.TyCon          (TyCon, TyConName)
import CLaSH.Core.Type           (LitTy (..), Type (..), TypeView (..), tyView)
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
    "GHC.Prim.ByteArray#"           -> fail $ "Can't translate type: " ++ showDoc ty
    "GHC.Types.Bool"                -> return Bool
    "GHC.Prim.~#"                   -> fail $ "Can't translate type: " ++ showDoc ty
    "CLaSH.Bit.Bit"                 -> return Bit
    "CLaSH.Signal.Implicit.Pack"    -> fail $ "Can't translate type: " ++ showDoc ty
    "CLaSH.Signal.Types.Signal"     -> ErrorT $ return $ coreTypeToHWType ghcTypeToHWType m (head args)
    "CLaSH.Signal.Implicit.SignalP" -> ErrorT $ return $ coreTypeToHWType ghcTypeToHWType m (head args)
    "CLaSH.Sized.Signed.Signed"     -> Signed   <$> tyNatSize (head args)
    "CLaSH.Sized.Unsigned.Unsigned" -> Unsigned <$> tyNatSize (head args)
    "CLaSH.Sized.Vector.Vec"        -> do
      let [szTy,elTy] = args
      sz     <- tyNatSize szTy
      elHWTy <- ErrorT $ return $ coreTypeToHWType ghcTypeToHWType m elTy
      return $ Vector sz elHWTy
    _ -> ErrorT Nothing

ghcTypeToHWType _ _ = Nothing

tyNatSize ::
  Type
  -> ErrorT String Maybe Int
tyNatSize (LitTy (NumTy i)) = return i
tyNatSize (tyView -> TyConApp tc [ty1,ty2]) = case name2String tc of
  "GHC.TypeLits.+" -> (+) <$> tyNatSize ty1 <*> tyNatSize ty2
  "GHC.TypeLits.*" -> (*) <$> tyNatSize ty1 <*> tyNatSize ty2
  "GHC.TypeLits.^" -> (^) <$> tyNatSize ty1 <*> tyNatSize ty2
  "GHC.TypeLits.-" -> (-) <$> tyNatSize ty1 <*> tyNatSize ty2
  "CLaSH.Promoted.Ord.Max" -> max <$> tyNatSize ty1 <*> tyNatSize ty2
  "CLaSH.Promoted.Ord.Min" -> min <$> tyNatSize ty1 <*> tyNatSize ty2
  _ -> fail $ $(curLoc) ++ "Can't convert tyNatOp: " ++ show tc
-- TODO: Remove this conversion
-- The current problem is that type-functions are not reduced by the GHC -> Core
-- transformation process, and so end up here. Once a fix has been found for
-- this problem remove this dirty hack.
tyNatSize (tyView -> TyConApp _ [ty1]) = tyNatSize ty1
tyNatSize t = fail $ $(curLoc) ++ "Can't convert tyNat: " ++ show t
