{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module CLaSH.GHC.Evaluator where

import qualified Data.Bifunctor      as Bifunctor
import           Data.Bits           (shiftL,shiftR)
import qualified Data.Either         as Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import           Unbound.Generics.LocallyNameless (runFreshM, bind, embed,
                                                   string2Name)

import           CLaSH.Core.DataCon  (DataCon (..))
import           CLaSH.Core.Literal  (Literal (..))
import           CLaSH.Core.Term     (Term (..))
import           CLaSH.Core.Type     (Type (..), ConstTy (..), LitTy (..),
                                      TypeView (..), tyView, mkFunTy,
                                      mkTyConApp, splitFunForallTy)
import           CLaSH.Core.TyCon    (TyCon, TyConName, tyConDataCons)
import           CLaSH.Core.TysPrim  (typeNatKind)
import           CLaSH.Core.Util     (collectArgs,mkApps,mkVec,termType)
import           CLaSH.Core.Var      (Var (..))

reduceConstant :: HashMap.HashMap TyConName TyCon -> Bool -> Term -> Term
reduceConstant tcm isSubj e@(collectArgs -> (Prim nm ty, args))
  | nm == "GHC.Prim.==#" || nm == "GHC.Integer.Type.eqInteger#"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i == j    -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == "GHC.Prim.>#" || nm == "GHC.Integer.Type.gtInteger#"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i > j     -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == "GHC.Prim.<#" || nm == "GHC.Integer.Type.ltInteger#"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i < j     -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == "GHC.Prim.<=#" || nm == "GHC.Integer.Type.leInteger#"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i <= j    -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == "GHC.Prim.>=#" || nm == "GHC.Integer.Type.geInteger#"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i >= j    -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == "GHC.Integer.Type.integerToInt"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i)] -> Literal (IntegerLiteral i)
      _ -> e
  | nm == "GHC.Prim.tagToEnum#"
  = case map (Bifunctor.bimap (reduceConstant tcm isSubj) id) args of
      [Right (ConstTy (TyCon tcN)), Left (Literal (IntegerLiteral i))] ->
        let dc = do { tc <- HashMap.lookup tcN tcm
                    ; let dcs = tyConDataCons tc
                    ; List.find ((== (i+1)) . toInteger . dcTag) dcs
                    }
        in maybe e Data dc
      _ -> e
  | nm == "GHC.Prim.*#"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> Literal (IntegerLiteral (i * j))
      _ -> e
  | nm == "GHC.Integer.Type.eqInteger"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> let (_,tyView -> TyConApp boolTcNm []) = splitFunForallTy ty
               (Just boolTc) = HashMap.lookup boolTcNm tcm
               [falseDc,trueDc] = tyConDataCons boolTc
               retDc = if i == j then trueDc else falseDc
           in  Data retDc
      _ -> e
  | nm == "GHC.Integer.Type.minusInteger"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> Literal (IntegerLiteral (i - j))
      _ -> e
  | nm == "GHC.Integer.Type.divInteger"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> Literal (IntegerLiteral (i `div` j))
      _ -> e
  | nm == "GHC.Integer.Type.quotInteger"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> Literal (IntegerLiteral (i `quot` j))
      _ -> e
  | nm == "GHC.Integer.Type.remInteger"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> Literal (IntegerLiteral (i `rem` j))
      _ -> e
  | nm == "GHC.Prim.quotRemInt#"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
               (Just tupTc) = HashMap.lookup tupTcNm tcm
               [tupDc] = tyConDataCons tupTc
               (q,r)   = quotRem i j
               ret     = mkApps (Data tupDc) (map Right tyArgs ++ [Left (Literal (IntegerLiteral q)), Left (Literal (IntegerLiteral r))])
            in ret
      _ -> e
  | nm == "GHC.Integer.Type.shiftLInteger"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> Literal (IntegerLiteral (i `shiftL` fromInteger j))
      _ -> e
  | nm == "GHC.Integer.Type.shiftRInteger"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        -> Literal (IntegerLiteral (i `shiftR` fromInteger j))
      _ -> e
  | nm == "GHC.Prim.negateInt#"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i)] -> Literal (IntegerLiteral (negate i))
      _ -> e
  | nm == "CLaSH.Sized.Internal.Signed.minBound#"
  = case args of
      [litTy,kn@(Left (Literal (IntegerLiteral mb)))]
        -> let minB = negate (2 ^ (mb - 1))
           in  mkApps signedConPrim [litTy,kn,Left (Literal (IntegerLiteral minB))]
      _ -> e
  | nm == "CLaSH.Sized.Internal.Signed.maxBound#"
  = case args of
      [litTy,kn@(Left (Literal (IntegerLiteral mb)))]
        -> let maxB = (2 ^ (mb - 1)) - 1
           in  mkApps signedConPrim [litTy,kn,Left (Literal (IntegerLiteral maxB))]
      _ -> e
  | nm == "CLaSH.Sized.Internal.Unsigned.minBound#"
  = case args of
      [litTy,kn@(Left (Literal (IntegerLiteral _)))]
        -> mkApps unsignedConPrim [litTy,kn,Left (Literal (IntegerLiteral 0))]
      _ -> e
  | nm == "CLaSH.Sized.Internal.Unsigned.maxBound#"
  = case args of
      [litTy,kn@(Left (Literal (IntegerLiteral mb)))]
        -> let maxB = (2 ^ mb) - 1
           in  mkApps unsignedConPrim [litTy,kn,Left (Literal (IntegerLiteral maxB))]
      _ -> e
  | nm == "CLaSH.Sized.Internal.Signed.toInteger#" || nm == "CLaSH.Sized.Internal.Unsigned.toInteger#"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [collectArgs -> (Prim nm' _,[Right _, Left _, Left (Literal (IntegerLiteral i))])]
        | nm' == "CLaSH.Sized.Internal.Signed.fromInteger#" ||
          nm' == "CLaSH.Sized.Internal.Unsigned.fromInteger#" -> Literal (IntegerLiteral i)
      _ -> e
  | nm == "CLaSH.Sized.Internal.Signed.eq#" || nm == "CLaSH.Sized.Internal.Unsigned.eq#"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [collectArgs -> (Prim _ _,[Right _, Left _, Left (Literal (IntegerLiteral i))]) ,collectArgs -> (Prim _ _,[Right _, Left _, Left (Literal (IntegerLiteral j))])] ->
           let (_,tyView -> TyConApp boolTcNm []) = splitFunForallTy ty
               (Just boolTc) = HashMap.lookup boolTcNm tcm
               [falseDc,trueDc] = tyConDataCons boolTc
               retDc = if i == j then trueDc else falseDc
           in  Data retDc
      _ -> e
  | nm == "GHC.TypeLits.natVal"
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      [Literal (IntegerLiteral i), _] -> Literal (IntegerLiteral i)
      _ -> e
  | nm == "CLaSH.Promoted.Nat.SNat"
  = case (map collectArgs . Either.lefts) args of
      [(Literal (IntegerLiteral _),[]), (Data _,_)] -> mkApps snatCon args
      _ -> e
  | isSubj && nm == "CLaSH.Sized.Vector.replicate"
  = let ty' = runFreshM (termType tcm e)
    in  case tyView ty' of
          (TyConApp vecTcNm [LitTy (NumTy len),argTy]) ->
              let (Just vecTc) = HashMap.lookup vecTcNm tcm
                  [nilCon,consCon] = tyConDataCons vecTc
              in  mkVec nilCon consCon argTy len (replicate len (last $ Either.lefts args))
          _ -> e
  | isSubj && nm == "CLaSH.Sized.Vector.maxIndex"
  = case Either.rights args of
      [LitTy (NumTy n), _] ->
        let ty' = runFreshM (termType tcm e)
            (TyConApp intTcNm _) = tyView ty'
            (Just intTc) = HashMap.lookup intTcNm tcm
            [intCon] = tyConDataCons intTc
        in  mkApps (Data intCon) [Left (Literal (IntegerLiteral (toInteger (n - 1))))]
      _ -> e

reduceConstant _ _ e = e

signedConPrim :: Term
signedConPrim = Prim "CLaSH.Sized.Internal.Signed.fromInteger#" (ForAllTy (bind nTV funTy))
  where
    funTy      = foldr1 mkFunTy [intTy,intTy,mkTyConApp signedTcNm [nVar]]
    intTy      = ConstTy (TyCon (string2Name "GHC.Integer.Type.Integer"))
    signedTcNm = string2Name "CLaSH.Sized.Internal.Signed.Signed"
    nName      = string2Name "n"
    nVar       = VarTy typeNatKind nName
    nTV        = TyVar nName (embed typeNatKind)

unsignedConPrim :: Term
unsignedConPrim = Prim "CLaSH.Sized.Internal.Unsigned.fromInteger#" (ForAllTy (bind nTV funTy))
  where
    funTy        = foldr1 mkFunTy [intTy,intTy,mkTyConApp unsignedTcNm [nVar]]
    intTy        = ConstTy (TyCon (string2Name "GHC.Integer.Type.Integer"))
    unsignedTcNm = string2Name "CLaSH.Sized.Internal.Unsigned.Unsigned"
    nName        = string2Name "n"
    nVar         = VarTy typeNatKind nName
    nTV          = TyVar nName (embed typeNatKind)

snatCon :: Term
snatCon = Data (MkData snanNm 1 snatTy [nName] [] argTys)
  where
    snanNm = string2Name "CLaSH.Promoted.Nat.SNat"
    snatTy = ForAllTy (bind nTV funTy)
    argTys = [ConstTy (TyCon (string2Name "GHC.Integer.Type.Integer"))
             ,AppTy (AppTy (ConstTy (TyCon (string2Name "Data.Proxy.Proxy"))) typeNatKind)
                    nVar
             ]
    funTy  = foldr mkFunTy (ConstTy (TyCon (string2Name "CLaSH.Promoted.Nat.SNat"))) argTys
    nName  = string2Name "n"
    nVar   = VarTy typeNatKind nName
    nTV    = TyVar nName (embed typeNatKind)

