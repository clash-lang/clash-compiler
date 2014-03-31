{-# LANGUAGE ViewPatterns #-}
module CLaSH.GHC.Evaluator where

import qualified Data.Bifunctor      as Bifunctor
import qualified Data.Either         as Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import qualified Data.Text           as Text
import           CLaSH.Core.DataCon  (dcTag)
import           CLaSH.Core.Literal  (Literal (..))
import           CLaSH.Core.Term     (Term (..))
import           CLaSH.Core.Type     (Type (..), ConstTy (..))
import           CLaSH.Core.TyCon    (TyCon, TyConName, tyConDataCons)
import           CLaSH.Core.Util     (collectArgs)

reduceConstant :: HashMap.HashMap TyConName TyCon -> Term -> Term
reduceConstant tcm e@(collectArgs -> (Prim nm _, args))
  | nm == Text.pack "GHC.Integer.Type.eqInteger#"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i == j    -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == Text.pack "GHC.Prim.<=#" || nm == Text.pack "GHC.Integer.Type.leInteger#"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i), Literal (IntegerLiteral j)]
        | i <= j    -> Literal (IntegerLiteral 1)
        | otherwise -> Literal (IntegerLiteral 0)
      _ -> e
  | nm == Text.pack "GHC.Integer.Type.integerToInt"
  = case (map (reduceConstant tcm) . Either.lefts) args of
      [Literal (IntegerLiteral i)] -> Literal (IntegerLiteral i)
      _ -> e
  | nm == Text.pack "GHC.Prim.tagToEnum#"
  = case map (Bifunctor.bimap (reduceConstant tcm) id) args of
      [Right (ConstTy (TyCon tcN)), Left (Literal (IntegerLiteral i))] ->
        let tc  = tcm HashMap.! tcN
            dcs = tyConDataCons tc
        in maybe e Data (List.find ((== (i+1)) . toInteger . dcTag) dcs)
      _ -> e

reduceConstant _ e = e
