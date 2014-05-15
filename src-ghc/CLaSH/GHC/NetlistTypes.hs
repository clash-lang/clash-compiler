{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module CLaSH.GHC.NetlistTypes
  (ghcTypeToHWType)
where

import Data.HashMap.Strict       (HashMap,(!))
import Control.Monad             (zipWithM)
import Control.Monad.Trans.Error (ErrorT(..))
import Unbound.LocallyNameless   (name2String)

import CLaSH.Core.Subst          (substTys)
import CLaSH.Core.Pretty         (showDoc)
import CLaSH.Core.TyCon          (TyCon (..), TyConName)
import CLaSH.Core.Type           (LitTy (..), Type (..), TyName, TypeView (..),
                                  tyView)
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
    "CLaSH.Signal.Implicit.CPack"   -> fail $ "Can't translate type: " ++ showDoc ty
    "CLaSH.Signal.Types.Signal"     -> ErrorT $ return $ coreTypeToHWType ghcTypeToHWType m (head args)
    "CLaSH.Signal.Types.CSignal"    -> ErrorT $ return $ coreTypeToHWType ghcTypeToHWType m (args !! 1)
    "CLaSH.Signal.Implicit.SignalP" -> ErrorT $ return $ coreTypeToHWType ghcTypeToHWType m (head args)
    "CLaSH.Signal.Explicit.CSignalP" -> ErrorT $ return $ coreTypeToHWType ghcTypeToHWType m (args !! 1)
    "CLaSH.Sized.Signed.Signed"     -> Signed   <$> tyNatSize m (head args)
    "CLaSH.Sized.Unsigned.Unsigned" -> Unsigned <$> tyNatSize m (head args)
    "CLaSH.Sized.Vector.Vec"        -> do
      let [szTy,elTy] = args
      sz     <- tyNatSize m szTy
      elHWTy <- ErrorT $ return $ coreTypeToHWType ghcTypeToHWType m elTy
      return $ Vector sz elHWTy
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

-- Given a set of type functions, and list of argument types, get the first
-- type function that matches, and return its substituted RHS type.
findFunSubst :: [([Type],Type)] -> [Type] -> Maybe Type
findFunSubst [] _ = Nothing
findFunSubst (tcSubst:rest) args = case funSubsts tcSubst args of
  Just ty -> Just ty
  Nothing -> findFunSubst rest args

-- Given a ([LHS match type], RHS type) representing a type function, and
-- a set of applied types. Match LHS with args, and when successful, return
-- a substituted RHS
funSubsts :: ([Type],Type) -> [Type] -> Maybe Type
funSubsts (tcSubstLhs,tcSubstRhs) args = do
  tySubts <- concat <$> zipWithM funSubst tcSubstLhs args
  let tyRhs = substTys tySubts tcSubstRhs
  return tyRhs

-- Given a LHS matching type, and a RHS to-match type, check if LHS and RHS
-- are a match. If they do match, and the LHS is a variable, return a
-- substitution
funSubst :: Type -> Type -> Maybe [(TyName,Type)]
funSubst (VarTy _ nmF) ty = Just [(nmF,ty)]
funSubst tyL@(LitTy _) tyR = if tyL == tyR then Just [] else Nothing
funSubst (tyView -> TyConApp tc argTys) (tyView -> TyConApp tc' argTys')
  | tc == tc'
  = do
    tySubts <- zipWithM funSubst argTys argTys'
    return (concat tySubts)
funSubst _ _ = Nothing
