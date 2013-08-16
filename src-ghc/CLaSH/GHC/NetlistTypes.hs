{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module CLaSH.GHC.NetlistTypes
  (ghcTypeToHWType)
where

import Control.Monad.Trans.Error (ErrorT(..))
import Unbound.LocallyNameless   (name2String)

import CLaSH.Core.Pretty         (showDoc)
import CLaSH.Core.TyCon          (tyConName)
import CLaSH.Core.Type           (LitTy(..),Type(..),TypeView(..),tyView)
import CLaSH.Netlist.Util        (coreTypeToHWType)
import CLaSH.Netlist.Types       (HWType(..))
import CLaSH.Util

ghcTypeToHWType ::
  Type
  -> Maybe (Either String HWType)
ghcTypeToHWType ty@(tyView -> TyConApp tc args) = runErrorT $
  case (name2String $ tyConName tc) of
    "GHC.Integer.Type.Integer"      -> return Integer
    "GHC.Prim.Int#"                 -> return Integer
    "GHC.Prim.Int"                  -> return Integer
    "GHC.Prim.ByteArray#"           -> fail $ "Can't translate type: " ++ showDoc ty
    "GHC.Types.Bool"                -> return Bool
    "GHC.TypeLits.Sing"             -> singletonToHWType (head args)
    "GHC.Prim.~#"                   -> fail $ "Can't translate type: " ++ showDoc ty
    "CLaSH.Bit.Bit"                 -> return Bit
    "CLaSH.Signal.Pack"             -> fail $ "Can't translate type: " ++ showDoc ty
    "CLaSH.Signal.Signal"           -> ErrorT $ return $ coreTypeToHWType ghcTypeToHWType (head args)
    "CLaSH.Signal.SignalP"          -> ErrorT $ return $ coreTypeToHWType ghcTypeToHWType (head args)
    "CLaSH.Sized.Signed.Signed"     -> Signed   <$> (tyNatSize $ head args)
    "CLaSH.Sized.Unsigned.Unsigned" -> Unsigned <$> (tyNatSize $ head args)
    "CLaSH.Sized.Vector.Vec"        -> do
      let [szTy,elTy] = args
      sz     <- tyNatSize szTy
      elHWTy <- ErrorT $ return $ coreTypeToHWType ghcTypeToHWType elTy
      return $ Vector sz elHWTy
    _ -> ErrorT $ Nothing

ghcTypeToHWType _ = Nothing

singletonToHWType ::
  Type
  -> ErrorT String Maybe HWType
singletonToHWType (tyView -> TyConApp tc [])
  | (name2String $ tyConName tc) == "GHC.TypeLits.Nat"
  = return Integer

singletonToHWType ty = fail $ "Can't translate singleton type: " ++ showDoc ty

tyNatSize ::
  Type
  -> ErrorT String Maybe Int
tyNatSize (LitTy (NumTy i)) = return i
tyNatSize ((tyView -> TyConApp tc [ty1,ty2]))
                            = case (name2String $ tyConName tc) of
                                "GHC.TypeLits.+" -> (+) <$> tyNatSize ty1 <*> tyNatSize ty2
                                "GHC.TypeLits.*" -> (*) <$> tyNatSize ty1 <*> tyNatSize ty2
                                _ -> fail $ $(curLoc) ++ "Can't convert tyNatOp: " ++ show tc
tyNatSize t                 = fail $ $(curLoc) ++ "Can't convert tyNat: " ++ show t
