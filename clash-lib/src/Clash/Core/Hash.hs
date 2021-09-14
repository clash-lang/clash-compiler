{-|

Lawful Hashable instance for Type and Term implemented using the technique
described in "Hashing Modulo Alpha-Equivalence" by Maziarz et al.
-}

{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.Core.Hash () where

import Data.Hashable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)

import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal)
import Clash.Core.Term
import Clash.Core.Type
import Clash.Core.Var

-- TODO The perf on this may be not that great. In the paper they start with
-- Structure and PosTree being ADTs but quickly move to just carrying around
-- a hash which is modified while descending to get around this, but it makes
-- the implementation less friendly to read.

data PosTree
  = Here
  -- ^ A free variable occurs here in a structure
  | Children [(Int, PosTree)]
  -- ^ A free variable occurs in the specified children. Children are
  -- zero-indexed according to their position in a constructor. e.g.
  --
  --   App child_0 child_1
  --   Letrec [(_, child_0), ..., (_, child_n)] child_n+1
  --   Case child_0 _ [(_, child_1), ..., (_, child_n)]
  --
  deriving (Generic, Hashable)

type VarMap a = Map (Var a) PosTree

toChild :: Int -> VarMap a -> VarMap a
toChild i = fmap (\p -> Children [(i, p)])

merge :: VarMap a -> VarMap a -> VarMap a
merge =
  Map.merge
    Map.preserveMissing
    Map.preserveMissing
    (Map.zipWithMatched (\_ ~(Children is) ~(Children js) -> Children (is <> js)))

mergeList :: Int -> [VarMap a] -> VarMap a
mergeList _ [] = Map.empty
mergeList i xs = foldr1 merge (zipWith toChild [i..] xs)

mergeApp :: VarMap a -> VarMap a -> VarMap a
mergeApp x y = merge (toChild 0 x) (toChild 1 y)

mergeLetrec :: [VarMap a] -> VarMap a -> VarMap a
mergeLetrec xs x =
  merge (mergeList 0 xs) (toChild (length xs) x)

mergeCase :: VarMap a -> Maybe (VarMap a) -> [VarMap a] -> VarMap a
mergeCase x my xs =
  case my of
    Just y  -> mergeList 0 (x : y : xs)
    Nothing -> merge (toChild 0 x) (mergeList 2 xs)

mergeCast :: VarMap a -> VarMap a -> VarMap a -> VarMap a
mergeCast l m r = mergeList 0 [l, m, r]

data TyStruct
  = SVarTy
  | SConstTy ConstTy
  | SForAllTy (Maybe PosTree) TyStruct
  | SAppTy TyStruct TyStruct
  | SLitTy LitTy
  | SAnnType [Attr'] TyStruct
  deriving (Generic, Hashable)

summarizeType :: Type -> (TyStruct, VarMap Type)
summarizeType = go
 where
  go (VarTy i) =
    (SVarTy, Map.singleton i Here)

  go (ConstTy a) =
    (SConstTy a, Map.empty)

  go (ForAllTy i a) =
    let (st, env) = go a
        mPos = Map.lookup i env
     in (SForAllTy mPos st, Map.delete i env)

  go (AppTy a b) =
    let (stA, envA) = go a
        (stB, envB) = go b
     in (SAppTy stA stB, mergeApp envA envB)

  go (LitTy l) =
    (SLitTy l, Map.empty)

  go (AnnType attrs a) =
    let (stA, envA) = go a
     in (SAnnType attrs stA, envA)

instance Hashable Type where
  hashWithSalt salt =
    hashWithSalt salt . fmap Map.toList . summarizeType

data TmStruct
  = SVar
  | SData DataCon
  | SLiteral Literal
  | SPrim PrimInfo
  | SLam (Maybe PosTree) TmStruct
  | STyLam (Maybe PosTree) TmStruct
  | SApp TmStruct TmStruct
  | STyApp TmStruct TyStruct
  | SLetrec [(Maybe PosTree, TmStruct)] TmStruct
  | SCase TmStruct TyStruct [([Maybe PosTree], [Maybe PosTree], TmStruct)]
  | SCast TmStruct TyStruct TyStruct
  | STick TickInfo TmStruct
  deriving (Generic, Hashable)

summarizeTerm :: Term -> (TmStruct, VarMap Type, VarMap Term)
summarizeTerm = go
 where
  go (Var i) =
    let (_, envA) = summarizeType (varType i)
     in (SVar, envA, Map.singleton i Here)

  go (Data dc) =
    (SData dc, Map.empty, Map.empty)

  go (Literal l) =
    (SLiteral l, Map.empty, Map.empty)

  go (Prim pr) =
    (SPrim pr, Map.empty, Map.empty)

  go (Lam i x) =
    let (stX, envA, envX) = go x
        mPos = Map.lookup i envX
     in (SLam mPos stX, envA, Map.delete i envX)

  go (TyLam i x) =
    let (stX, envA, envX) = go x
        mPos = Map.lookup i envA
     in (STyLam mPos stX, Map.delete i envA, envX)

  go (App x y) =
    let (stX, envF, envX) = go x
        (stY, envA, envY) = go y
     in (SApp stX stY, mergeApp envF envA, mergeApp envX envY)

  go (TyApp x a) =
    let (stX, envF, envX) = go x
        (stA, envA) = summarizeType a
     in (STyApp stX stA, mergeApp envF envA, envX)

  go (Letrec bs x) =
    let (stBs, envAs, envXs) = unzip3 (fmap (go . snd) bs)
        (stX, envA, envX) = go x
        ids = fmap fst bs
        envA' = mergeLetrec envAs envA
        envX' = mergeLetrec envXs envX
        mPosIds = fmap (`Map.lookup` envX') ids
        envX'' = Map.withoutKeys envX' (Set.fromList ids)
     in (SLetrec (zip mPosIds stBs) stX, envA', envX'')

  go (Case x ty alts) =
    let (stX, envA, envX) = go x
        (stTy, envTy) = summarizeType ty
        (stAlts, envAs, envXs) = unzip3 (fmap goAlt alts)
     in (SCase stX stTy stAlts, mergeCase envA (Just envTy) envAs, mergeCase envX Nothing envXs)

  go (Cast x a b) =
    let (stX, envC, envX) = go x
        (stA, envA) = summarizeType a
        (stB, envB) = summarizeType b
     in (SCast stX stA stB, mergeCast envC envA envB, envX)

  go (Tick t x) =
    let (stX, envA, envX) = go x
     in (STick t stX, envA, envX)

  goAlt (p, x) =
    let (stX, envA, envX) = go x
        (tvs, ids) = patIds p
        mPosTvs = fmap (`Map.lookup` envA) tvs
        mPosIds = fmap (`Map.lookup` envX) ids
        envA' = Map.withoutKeys envA (Set.fromList tvs)
        envX' = Map.withoutKeys envX (Set.fromList ids)
     in ((mPosTvs, mPosIds, stX), envA', envX')

instance Hashable Term where
  hashWithSalt salt x =
    let (stX, envA, envX) = summarizeTerm x
        envA' = Map.toList envA
        envX' = Map.toList envX
     in salt `hashWithSalt` stX `hashWithSalt` envA' `hashWithSalt` envX'
