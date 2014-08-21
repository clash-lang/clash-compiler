{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module CLaSH.GHC.Evaluator where

import qualified Data.Bifunctor      as Bifunctor
import           Data.Bits           (shiftL,shiftR)
import qualified Data.Either         as Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import           Unbound.LocallyNameless (bind, embed, string2Name)

import           CLaSH.Core.DataCon  (dcTag)
import           CLaSH.Core.Literal  (Literal (..))
import           CLaSH.Core.Term     (Term (..))
import           CLaSH.Core.Type     (Type (..), ConstTy (..), mkFunTy)
import           CLaSH.Core.TyCon    (TyCon, TyConName, tyConDataCons)
import           CLaSH.Core.TysPrim  (typeNatKind)
import           CLaSH.Core.Util     (collectArgs, mkApps)
import           CLaSH.Core.Var      (Var (..))

reduceConstant :: HashMap.HashMap TyConName TyCon -> Term -> Term
reduceConstant tcm e@(collectArgs -> (Prim nm _, args))
  | nm == "GHC.Prim.==#" || nm == "GHC.Integer.Type.eqInteger#"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i == j    -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == "GHC.Integer.Type.gtInteger#"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i > j     -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == "GHC.Integer.Type.ltInteger#"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i < j     -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == "GHC.Prim.<=#" || nm == "GHC.Integer.Type.leInteger#"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i <= j    -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == "GHC.Prim.>=#" || nm == "GHC.Integer.Type.geInteger#"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i >= j    -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == "GHC.Integer.Type.integerToInt"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i)] -> Literal (IntegerLiteral i)
      _ -> e
  | nm == "GHC.Prim.tagToEnum#"
  = case map (Bifunctor.bimap (reduceConstant tcm) id) args of
      [Right (ConstTy (TyCon tcN)), Left (Literal (IntegerLiteral i))] ->
        let dc = do { tc <- HashMap.lookup tcN tcm
                    ; let dcs = tyConDataCons tc
                    ; List.find ((== (i+1)) . toInteger . dcTag) dcs
                    }
        in maybe e Data dc
      _ -> e
  | nm == "GHC.Prim.*#"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> Literal (IntegerLiteral (i * j))
      _ -> e
  | nm == "GHC.Integer.Type.divInteger"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> Literal (IntegerLiteral (i `div` j))
      _ -> e
  | nm == "GHC.Integer.Type.shiftLInteger"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> Literal (IntegerLiteral (i `shiftL` fromInteger j))
      _ -> e
  | nm == "GHC.Integer.Type.shiftRInteger"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> Literal (IntegerLiteral (i `shiftR` fromInteger j))
      _ -> e
  | nm == "GHC.Prim.negateInt#"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i)] -> Literal (IntegerLiteral (negate i))
      _ -> e
  | nm == "CLaSH.Sized.Internal.Signed.minBound#"
  = case args of
      [litTy,Left (Literal (IntegerLiteral mb))]
        -> let minB = negate (2 ^ (mb - 1))
           in  mkApps signedConPrim [litTy,Left (Literal (IntegerLiteral minB))]
      _ -> e
  | nm == "CLaSH.Sized.Internal.Signed.maxBound#"
  = case args of
      [litTy,Left (Literal (IntegerLiteral mb))]
        -> let maxB = (2 ^ (mb - 1)) - 1
           in  mkApps signedConPrim [litTy,Left (Literal (IntegerLiteral maxB))]
      _ -> e
  | nm == "CLaSH.Sized.Internal.Unsigned.minBound#"
  = case args of
      [litTy]
        -> mkApps unsignedConPrim [litTy,Left (Literal (IntegerLiteral 0))]
      _ -> e
  | nm == "CLaSH.Sized.Internal.Unsigned.maxBound#"
  = case args of
      [litTy,Left (Literal (IntegerLiteral mb))]
        -> let maxB = 2 ^ mb
           in  mkApps unsignedConPrim [litTy,Left (Literal (IntegerLiteral maxB))]
      _ -> e
  | nm == "CLaSH.Sized.Internal.Signed.toInteger#" || nm == "CLaSH.Sized.Internal.Unsigned.toInteger#"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [App _ (Literal (IntegerLiteral i))]
        -> Literal (IntegerLiteral i)
      _ -> e

reduceConstant _ e = e

signedConPrim :: Term
signedConPrim = Prim "CLaSH.Sized.Internal.Signed.S" (ForAllTy (bind nTV funTy))
  where
    funTy    = mkFunTy intTy (AppTy signedTy nVar)
    intTy    = ConstTy (TyCon (string2Name "GHC.Integer.Type.Integer"))
    signedTy = ConstTy (TyCon (string2Name "CLaSH.Sized.Internal.Signed.Signed"))
    nName    = string2Name "n"
    nVar     = VarTy typeNatKind nName
    nTV      = TyVar nName (embed typeNatKind)

unsignedConPrim :: Term
unsignedConPrim = Prim "CLaSH.Sized.Internal.Unsigned.U" (ForAllTy (bind nTV funTy))
  where
    funTy      = mkFunTy intTy (AppTy unsignedTy nVar)
    intTy      = ConstTy (TyCon (string2Name "GHC.Integer.Type.Integer"))
    unsignedTy = ConstTy (TyCon (string2Name "CLaSH.Sized.Internal.Unsigned.Signed"))
    nName      = string2Name "n"
    nVar       = VarTy typeNatKind nName
    nTV        = TyVar nName (embed typeNatKind)
