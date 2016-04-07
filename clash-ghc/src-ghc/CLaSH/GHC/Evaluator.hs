{-|
  Copyright   :  (C) 2013-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE ViewPatterns      #-}
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
reduceConstant tcm isSubj e@(collectArgs -> (Prim nm ty, args)) = case nm of
  "GHC.Prim.+#" | Just (i,j) <- intLiterals tcm isSubj args
    -> integerToIntLiteral (i+j)

  "GHC.Prim.-#" | Just (i,j) <- intLiterals tcm isSubj args
    -> integerToIntLiteral (i-j)

  "GHC.Prim.*#" | Just (i,j) <- intLiterals tcm isSubj args
    -> integerToIntLiteral (i*j)

  "GHC.Prim.quotInt#" | Just (i,j) <- intLiterals tcm isSubj args
    -> integerToIntLiteral (i `quot` j)

  "GHC.Prim.remInt#" | Just (i,j) <- intLiterals tcm isSubj args
    -> integerToIntLiteral (i `rem` j)

  "GHC.Prim.quotRemInt#" | Just (i,j) <- intLiterals tcm isSubj args
    -> let (_,tyView -> TyConApp tupTcNm tyArgs) = splitFunForallTy ty
           (Just tupTc) = HashMap.lookup tupTcNm tcm
           [tupDc] = tyConDataCons tupTc
           (q,r)   = quotRem i j
           ret     = mkApps (Data tupDc) (map Right tyArgs ++
                    [Left (integerToIntLiteral q), Left (integerToIntLiteral r)])
       in  ret

  "GHC.Prim.negateInt#"
    | [Literal (IntLiteral i)] <- (map (reduceConstant tcm isSubj) . Either.lefts) args
    -> integerToIntLiteral (negate i)

  "GHC.Prim.>#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i > j)

  "GHC.Prim.>=#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i >= j)

  "GHC.Prim.==#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i == j)

  "GHC.Prim./=#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i /= j)

  "GHC.Prim.<#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i < j)

  "GHC.Prim.<=#" | Just (i,j) <- intLiterals tcm isSubj args
    -> boolToIntLiteral (i <= j)

  "GHC.Prim.tagToEnum#"
    | [Right (ConstTy (TyCon tcN)), Left (Literal (IntLiteral i))] <-
      map (Bifunctor.bimap (reduceConstant tcm isSubj) id) args
    -> let dc = do { tc <- HashMap.lookup tcN tcm
                   ; let dcs = tyConDataCons tc
                   ; List.find ((== (i+1)) . toInteger . dcTag) dcs
                   }
       in maybe e Data dc

  "GHC.Integer.Type.integerToInt"
    | [Literal (IntegerLiteral i)] <- (map (reduceConstant tcm isSubj) . Either.lefts) args
    -> integerToIntLiteral i

  "GHC.Integer.Type.plusInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i+j)

  "GHC.Integer.Type.minusInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i-j)

  "GHC.Integer.Type.timesInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i*j)

  "GHC.Integer.Type.negateInteger#"
    | [Literal (IntegerLiteral i)] <- (map (reduceConstant tcm isSubj) . Either.lefts) args
    -> integerToIntegerLiteral (negate i)

  "GHC.Integer.Type.divInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i `div` j)

  "GHC.Integer.Type.modInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i `mod` j)

  "GHC.Integer.Type.quotInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i `quot` j)

  "GHC.Integer.Type.remInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> integerToIntegerLiteral (i `rem` j)

  "GHC.Integer.Type.gtInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i > j)

  "GHC.Integer.Type.geInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i >= j)

  "GHC.Integer.Type.eqInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i == j)

  "GHC.Integer.Type.neqInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i /= j)

  "GHC.Integer.Type.ltInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i < j)

  "GHC.Integer.Type.leInteger" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i <= j)

  "GHC.Integer.Type.gtInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i > j)

  "GHC.Integer.Type.geInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i >= j)

  "GHC.Integer.Type.eqInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i == j)

  "GHC.Integer.Type.neqInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i /= j)

  "GHC.Integer.Type.ltInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i < j)

  "GHC.Integer.Type.leInteger#" | Just (i,j) <- integerLiterals tcm isSubj args
    -> boolToIntLiteral (i <= j)

  "GHC.Integer.Type.shiftRInteger"
    | [Literal (IntegerLiteral i), Literal (IntLiteral j)] <-
      (map (reduceConstant tcm isSubj) . Either.lefts) args
    -> integerToIntegerLiteral (i `shiftR` fromInteger j)

  "GHC.Integer.Type.shiftLInteger"
    | [Literal (IntegerLiteral i), Literal (IntLiteral j)] <-
      (map (reduceConstant tcm isSubj) . Either.lefts) args
    -> integerToIntegerLiteral (i `shiftL` fromInteger j)

  "GHC.TypeLits.natVal"
    | [Literal (IntegerLiteral i), _] <- (map (reduceConstant tcm isSubj) . Either.lefts) args
    -> integerToIntegerLiteral i

  "GHC.Types.I#"
    | isSubj
    , [Literal (IntLiteral i)] <- (map (reduceConstant tcm isSubj) . Either.lefts) args
    ->  let (_,tyView -> TyConApp intTcNm []) = splitFunForallTy ty
            (Just intTc) = HashMap.lookup intTcNm tcm
            [intDc] = tyConDataCons intTc
        in  mkApps (Data intDc) [Left (Literal (IntLiteral i))]

  "CLaSH.Sized.Internal.Signed.eq#" | Just (i,j) <- signedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i == j)

  "CLaSH.Sized.Internal.Signed.neq#" | Just (i,j) <- signedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i /= j)

  "CLaSH.Sized.Internal.Signed.minBound#"
    | [litTy,kn@(Left (Literal (IntegerLiteral mb)))] <- args
    -> let minB = negate (2 ^ (mb - 1))
       in  mkApps signedConPrim [litTy,kn,Left (Literal (IntegerLiteral minB))]

  "CLaSH.Sized.Internal.Signed.maxBound#"
    | [litTy,kn@(Left (Literal (IntegerLiteral mb)))] <- args
    -> let maxB = (2 ^ (mb - 1)) - 1
       in  mkApps signedConPrim [litTy,kn,Left (Literal (IntegerLiteral maxB))]

  "CLaSH.Sized.Internal.Signed.toInteger#"
    | [collectArgs -> (Prim nm' _,[Right _, Left _, Left (Literal (IntegerLiteral i))])] <-
      (map (reduceConstant tcm isSubj) . Either.lefts) args
    , nm' == "CLaSH.Sized.Internal.Signed.fromInteger#"
    -> integerToIntegerLiteral i

  "CLaSH.Sized.Internal.Unsigned.eq#" | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i == j)

  "CLaSH.Sized.Internal.Unsigned.neq#" | Just (i,j) <- unsignedLiterals tcm isSubj args
    -> boolToBoolLiteral tcm ty (i /= j)

  "CLaSH.Sized.Internal.Unsigned.minBound#"
    | [litTy,kn@(Left (Literal (IntegerLiteral _)))] <- args
    -> mkApps unsignedConPrim [litTy,kn,Left (Literal (IntegerLiteral 0))]

  "CLaSH.Sized.Internal.Unsigned.minBound#"
    | [litTy,kn@(Left (Literal (IntegerLiteral mb)))] <- args
    -> let maxB = (2 ^ mb) - 1
       in  mkApps unsignedConPrim [litTy,kn,Left (Literal (IntegerLiteral maxB))]

  "CLaSH.Sized.Internal.Unsigned.toInteger#"
    | [collectArgs -> (Prim nm' _,[Right _, Left _, Left (Literal (IntegerLiteral i))])] <-
      (map (reduceConstant tcm isSubj) . Either.lefts) args
    , nm' == "CLaSH.Sized.Internal.Unsigned.fromInteger#"
    -> integerToIntegerLiteral i

  "CLaSH.Promoted.Nat.SNat"
    | [(Literal (IntegerLiteral _),[]), (Data _,_)] <- (map collectArgs . Either.lefts) args
    -> mkApps snatCon args

  "CLaSH.Sized.Vector.replicate"
    | isSubj
    , (TyConApp vecTcNm [LitTy (NumTy len),argTy]) <- tyView (runFreshM (termType tcm e))
    -> let (Just vecTc) = HashMap.lookup vecTcNm tcm
           [nilCon,consCon] = tyConDataCons vecTc
       in  mkVec nilCon consCon argTy len (replicate len (last $ Either.lefts args))

  "CLaSH.Sized.Vector.maxIndex"
    | isSubj
    , [LitTy (NumTy n), _] <- Either.rights args
    -> let ty' = runFreshM (termType tcm e)
           (TyConApp intTcNm _) = tyView ty'
           (Just intTc) = HashMap.lookup intTcNm tcm
           [intCon] = tyConDataCons intTc
       in  mkApps (Data intCon) [Left (Literal (IntegerLiteral (toInteger (n - 1))))]

  "CLaSH.Sized.Vector.length"
    | isSubj
    , [LitTy (NumTy n), _] <- Either.rights args
    -> let ty' = runFreshM (termType tcm e)
           (TyConApp intTcNm _) = tyView ty'
           (Just intTc) = HashMap.lookup intTcNm tcm
           [intCon] = tyConDataCons intTc
       in  mkApps (Data intCon) [Left (Literal (IntegerLiteral (toInteger n)))]

  _ -> e

reduceConstant _ _ e = e

integerLiterals :: HashMap.HashMap TyConName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
integerLiterals tcm isSubj args = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
  [Literal (IntegerLiteral i), Literal (IntegerLiteral j)] -> Just (i,j)
  _ -> Nothing

intLiterals :: HashMap.HashMap TyConName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
intLiterals tcm isSubj args = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
  [Literal (IntLiteral i), Literal (IntLiteral j)] -> Just (i,j)
  _ -> Nothing

signedLiterals :: HashMap.HashMap TyConName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
signedLiterals tcm isSubj args
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      ([ collectArgs -> (Prim nm  _,[Right _, Left _, Left (Literal (IntegerLiteral i))])
       , collectArgs -> (Prim nm' _,[Right _, Left _, Left (Literal (IntegerLiteral j))])])
        | nm  == "CLaSH.Sized.Internal.Signed.fromInteger#"
        , nm' == "CLaSH.Sized.Internal.Signed.fromInteger#" -> Just (i,j)
      _ -> Nothing

unsignedLiterals :: HashMap.HashMap TyConName TyCon -> Bool -> [Either Term Type] -> Maybe (Integer,Integer)
unsignedLiterals tcm isSubj args
  = case (map (reduceConstant tcm isSubj) . Either.lefts) args of
      ([ collectArgs -> (Prim nm  _,[Right _, Left _, Left (Literal (IntegerLiteral i))])
       , collectArgs -> (Prim nm' _,[Right _, Left _, Left (Literal (IntegerLiteral j))])])
        | nm  == "CLaSH.Sized.Internal.Unsigned.fromInteger#"
        , nm' == "CLaSH.Sized.Internal.Unsigned.fromInteger#" -> Just (i,j)
      _ -> Nothing

boolToIntLiteral :: Bool -> Term
boolToIntLiteral b = if b then Literal (IntLiteral 1) else Literal (IntLiteral 0)

boolToBoolLiteral :: HashMap.HashMap TyConName TyCon -> Type -> Bool -> Term
boolToBoolLiteral tcm ty b =
 let (_,tyView -> TyConApp boolTcNm []) = splitFunForallTy ty
     (Just boolTc) = HashMap.lookup boolTcNm tcm
     [falseDc,trueDc] = tyConDataCons boolTc
     retDc = if b then trueDc else falseDc
 in  Data retDc

integerToIntLiteral :: Integer -> Term
integerToIntLiteral = Literal . IntLiteral . toInteger . (fromInteger :: Integer -> Int) -- for overflow behaviour

integerToIntegerLiteral :: Integer -> Term
integerToIntegerLiteral = Literal . IntegerLiteral

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
