{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Alpha-equivalence of scoped Clash Core, and a total order up to
  alpha-equivalence.

  Both sides are in the same scope, so free variables are compared by their
  name. Bound variables are compared by the position of their binder.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.Scoped.Alpha
  ( aeqTy
  , aeqTm
  , commonAltBody
    -- * Ordering
  , acmpTy
  , acmpTm
  , AlphaTerm (..)
  , AlphaType (..)
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isJust)
import Data.Ord (comparing)
import GHC.Types.SrcLoc (leftmost_smallest)


import Clash.Core.Scoped.FreeVars (HasNameBinder (..))
import Clash.Core.Scoped.Internal.Scope (nameId, nameOf)
import Clash.Core.Scoped.Subst (unsinkAlt)
import Clash.Core.Scoped.Syntax

-- | Alpha-equivalence of types. Like 'Clash.Core.Subst.aeqType', the kinds of
-- binders are compared, but not the kinds of variable occurrences.
aeqTy :: Type n -> Type n -> Bool
aeqTy = aeqTyLevels emptyEnv

-- | Alpha-equivalence of terms. Like 'Clash.Core.Subst.aeqTerm', the types of
-- binders are compared, but not those of variable occurrences, nor the result
-- types of case-expressions.
aeqTm :: Term n -> Term n -> Bool
aeqTm = aeqTmLevels emptyEnv

-- | If the right-hand sides of all alternatives are the same term, which
-- mentions none of the variables bound by the patterns, that term
commonAltBody :: [Alt n] -> Maybe (Term n)
commonAltBody [] = Nothing
commonAltBody (Alt p1 e1 : alts)
  -- Comparing first usually fails fast. Comparing terms in different scopes is
  -- meaningful here, as only a result where all alternatives can be moved out
  -- of the scope of their pattern is used, and then they are in the same
  -- scope.
  | all (\(Alt _ e) -> aeqTmLevels emptyEnv e1 e) alts
  , Just e1' <- unsinkAlt p1 e1
  , all (\(Alt p e) -> isJust (unsinkAlt p e)) alts
  = Just e1'
  | otherwise = Nothing

-- * Implementation
--
-- Bound names are mapped to the De Bruijn level of their binder, separately
-- for both sides. Free names are compared by identity. The functions compare
-- values in scopes @x@ and @y@, which are only meaningful if the free names of
-- both are names of the same scope.

-- | The level of the next binder, and the levels of the bound names of either
-- side
data Env = Env !Int !(IntMap Int) !(IntMap Int)

emptyEnv :: Env
emptyEnv = Env 0 IntMap.empty IntMap.empty

-- | Go under a binder on either side
under :: Int -> Int -> Env -> Env
under k1 k2 (Env lvl envL envR) =
  Env (lvl + 1) (IntMap.insert k1 lvl envL) (IntMap.insert k2 lvl envR)
{-# INLINE under #-}

-- | Go under a binder on either side
underB :: (HasNameBinder b1, HasNameBinder b2) => b1 x x' -> b2 y y' -> Env -> Env
underB b1 b2 = under (binderRaw b1) (binderRaw b2)
 where
  binderRaw :: HasNameBinder b => b n l -> Int
  binderRaw = nameId . nameOf . nameBinderOf
{-# INLINE underB #-}

aeqName :: Env -> Int -> Int -> Bool
aeqName (Env _ envL envR) n1 n2 = case (IntMap.lookup n1 envL, IntMap.lookup n2 envR) of
  (Just l1, Just l2) -> l1 == l2
  (Nothing, Nothing) -> n1 == n2
  _ -> False
{-# INLINE aeqName #-}

aeqTyLevels :: Env -> Type x -> Type y -> Bool
aeqTyLevels env = go
 where
  go :: Type x -> Type y -> Bool
  go (VarTy _ _ n1) (VarTy _ _ n2) = aeqName env (tyNameRaw n1) (tyNameRaw n2)
  go (ConstTy c1) (ConstTy c2) = c1 == c2
  go (ForAllTy b1 t1) (ForAllTy b2 t2) =
    go (tyBinderKind b1) (tyBinderKind b2) && aeqTyLevels (underB b1 b2 env) t1 t2
  go (AppTy s1 t1) (AppTy s2 t2) = go s1 s2 && go t1 t2
  go (LitTy l1) (LitTy l2) = l1 == l2
  go (AnnType a1 t1) (AnnType a2 t2) = a1 == a2 && go t1 t2
  go _ _ = False

aeqTmLevels :: Env -> Term x -> Term y -> Bool
aeqTmLevels env = go
 where
  goTy :: Type x -> Type y -> Bool
  goTy = aeqTyLevels env

  go :: Term x -> Term y -> Bool
  go (Var _ _ n1) (Var _ _ n2) = aeqName env (tmNameRaw n1) (tmNameRaw n2)
  go (GlobalVar g1) (GlobalVar g2) = g1 == g2
  go (Data dc1) (Data dc2) = dc1 == dc2
  go (Literal l1) (Literal l2) = l1 == l2
  go (Prim p1) (Prim p2) = primName p1 == primName p2
  go (Lam b1 e1) (Lam b2 e2) =
    goTy (idBinderType b1) (idBinderType b2) && aeqTmLevels (underB b1 b2 env) e1 e2
  go (TyLam b1 e1) (TyLam b2 e2) =
    goTy (tyBinderKind b1) (tyBinderKind b2) && aeqTmLevels (underB b1 b2 env) e1 e2
  go (App l1 r1) (App l2 r2) = go l1 l2 && go r1 r2
  go (TyApp l1 r1) (TyApp l2 r2) = go l1 l2 && goTy r1 r2
  go (Let (NonRec b1 x1) e1) (Let (NonRec b2 x2) e2) =
    goTy (idBinderType b1) (idBinderType b2) && go x1 x2 &&
      aeqTmLevels (underB b1 b2 env) e1 e2
  go (Let (Rec bs1 xs1) e1) (Let (Rec bs2 xs2) e2) =
    length xs1 == length xs2 &&
      aeqTele env bs1 bs2 (\env' -> and (zipWith (aeqTmLevels env') (e1:xs1) (e2:xs2)))
  go (Case e1 _ as1) (Case e2 _ as2) =
    length as1 == length as2 && go e1 e2 && and (zipWith goAlt as1 as2)
  go (Cast e1 l1 r1) (Cast e2 l2 r2) = go e1 e2 && goTy l1 l2 && goTy r1 r2
  go (Tick t1 e1) (Tick t2 e2) = goTick t1 t2 && go e1 e2
  go _ _ = False

  goAlt :: Alt x -> Alt y -> Bool
  goAlt (Alt p1 e1) (Alt p2 e2) = case (p1, p2) of
    (DataPat dc1 tvs1 ids1, DataPat dc2 tvs2 ids2) ->
      dc1 == dc2 &&
        aeqTele env tvs1 tvs2 (\env1 ->
          aeqTele env1 ids1 ids2 (\env2 ->
            aeqTmLevels env2 e1 e2))
    (LitPat l1, LitPat l2) -> l1 == l2 && aeqTmLevels env e1 e2
    (DefaultPat, DefaultPat) -> aeqTmLevels env e1 e2
    _ -> False

  goTick :: TickInfo x -> TickInfo y -> Bool
  goTick (SrcSpan s1) (SrcSpan s2) = s1 == s2
  goTick (NameMod m1 t1) (NameMod m2 t2) = m1 == m2 && goTy t1 t2
  goTick DeDup DeDup = True
  goTick NoDeDup NoDeDup = True
  goTick (Attributes t1 e1) (Attributes t2 e2) = goTy t1 t2 && go e1 e2
  goTick _ _ = False

-- | Binders whose payload can be compared
class HasNameBinder b => AeqBinder b where
  aeqPayload :: Env -> b x x' -> b y y' -> Bool
  acmpPayload :: Env -> b x x' -> b y y' -> Ordering

instance AeqBinder TyBinder where
  aeqPayload env b1 b2 = aeqTyLevels env (tyBinderKind b1) (tyBinderKind b2)
  acmpPayload env b1 b2 = acmpTyLevels env (tyBinderKind b1) (tyBinderKind b2)

instance AeqBinder IdBinder where
  aeqPayload env b1 b2 = aeqTyLevels env (idBinderType b1) (idBinderType b2)
  acmpPayload env b1 b2 = acmpTyLevels env (idBinderType b1) (idBinderType b2)

-- | Compare two telescopes, and continue in the environment under them
aeqTele :: AeqBinder b => Env -> Tele b x x' -> Tele b y y' -> (Env -> Bool) -> Bool
aeqTele env TNil TNil k = k env
aeqTele env (TCons b1 bs1) (TCons b2 bs2) k =
  aeqPayload env b1 b2 && aeqTele (underB b1 b2 env) bs1 bs2 k
aeqTele _ _ _ _ = False

-- * Ordering

-- | A total order on types, up to alpha-equivalence: @'acmpTy' t1 t2 == EQ@
-- if and only if @'aeqTy' t1 t2@. Like 'Clash.Core.Subst.acmpType'.
acmpTy :: Type n -> Type n -> Ordering
acmpTy = acmpTyLevels emptyEnv

-- | A total order on terms, up to alpha-equivalence, like
-- 'Clash.Core.Subst.acmpTerm'. @'acmpTm' t1 t2 == EQ@ if and only if
-- @'aeqTm' t1 t2@, except that it compares the source spans of ticks with
-- 'leftmost_smallest', which treats all unhelpful spans alike, like
-- 'Clash.Core.Subst.acmpTerm' does.
acmpTm :: Term n -> Term n -> Ordering
acmpTm = acmpTmLevels emptyEnv

-- | A closed term, ordered up to alpha-equivalence ('acmpTm'), e.g. as the key
-- of a map
newtype AlphaTerm = AlphaTerm (Term 'VoidS)
  deriving (Show)

instance Eq AlphaTerm where
  AlphaTerm t1 == AlphaTerm t2 = acmpTm t1 t2 == EQ

instance Ord AlphaTerm where
  compare (AlphaTerm t1) (AlphaTerm t2) = acmpTm t1 t2

-- | A closed type, ordered up to alpha-equivalence ('acmpTy')
newtype AlphaType = AlphaType (Type 'VoidS)
  deriving (Show)

instance Eq AlphaType where
  AlphaType t1 == AlphaType t2 = acmpTy t1 t2 == EQ

instance Ord AlphaType where
  compare (AlphaType t1) (AlphaType t2) = acmpTy t1 t2

-- | Compare names, see 'aeqName'. A bound name is smaller than a free one.
acmpName :: Env -> Int -> Int -> Ordering
acmpName (Env _ envL envR) n1 n2 = case (IntMap.lookup n1 envL, IntMap.lookup n2 envR) of
  (Just l1, Just l2) -> compare l1 l2
  (Nothing, Nothing) -> compare n1 n2
  (Just _, Nothing) -> LT
  (Nothing, Just _) -> GT
{-# INLINE acmpName #-}

acmpTyLevels :: Env -> Type x -> Type y -> Ordering
acmpTyLevels env = go
 where
  go :: Type x -> Type y -> Ordering
  go (VarTy _ _ n1) (VarTy _ _ n2) = acmpName env (tyNameRaw n1) (tyNameRaw n2)
  go (ConstTy c1) (ConstTy c2) = compare c1 c2
  go (ForAllTy b1 t1) (ForAllTy b2 t2) =
    go (tyBinderKind b1) (tyBinderKind b2) <> acmpTyLevels (underB b1 b2 env) t1 t2
  go (AppTy s1 t1) (AppTy s2 t2) = go s1 s2 <> go t1 t2
  go (LitTy l1) (LitTy l2) = compare l1 l2
  go (AnnType a1 t1) (AnnType a2 t2) = compare a1 a2 <> go t1 t2
  go t1 t2 = compare (rank t1) (rank t2)

  rank :: Type x -> Int
  rank = \case
    VarTy {} -> 0
    LitTy {} -> 1
    ConstTy {} -> 2
    AnnType {} -> 3
    AppTy {} -> 4
    ForAllTy {} -> 5

acmpTmLevels :: Env -> Term x -> Term y -> Ordering
acmpTmLevels env = go
 where
  goTy :: Type x -> Type y -> Ordering
  goTy = acmpTyLevels env

  go :: Term x -> Term y -> Ordering
  go (Var _ _ n1) (Var _ _ n2) = acmpName env (tmNameRaw n1) (tmNameRaw n2)
  go (GlobalVar g1) (GlobalVar g2) = compare g1 g2
  go (Data dc1) (Data dc2) = compare dc1 dc2
  go (Literal l1) (Literal l2) = compare l1 l2
  go (Prim p1) (Prim p2) = comparing primName p1 p2
  go (Lam b1 e1) (Lam b2 e2) =
    goTy (idBinderType b1) (idBinderType b2) <> acmpTmLevels (underB b1 b2 env) e1 e2
  go (TyLam b1 e1) (TyLam b2 e2) =
    goTy (tyBinderKind b1) (tyBinderKind b2) <> acmpTmLevels (underB b1 b2 env) e1 e2
  go (App l1 r1) (App l2 r2) = go l1 l2 <> go r1 r2
  go (TyApp l1 r1) (TyApp l2 r2) = go l1 l2 <> goTy r1 r2
  go (Let (NonRec b1 x1) e1) (Let (NonRec b2 x2) e2) =
    goTy (idBinderType b1) (idBinderType b2) <> go x1 x2 <>
      acmpTmLevels (underB b1 b2 env) e1 e2
  go (Let (Rec bs1 xs1) e1) (Let (Rec bs2 xs2) e2) =
    compare (length xs1) (length xs2) <>
      acmpTele env bs1 bs2 (\env' -> mconcat (zipWith (acmpTmLevels env') (e1:xs1) (e2:xs2)))
  go (Case e1 _ as1) (Case e2 _ as2) =
    compare (length as1) (length as2) <> go e1 e2 <> mconcat (zipWith goAlt as1 as2)
  go (Cast e1 l1 r1) (Cast e2 l2 r2) = go e1 e2 <> goTy l1 l2 <> goTy r1 r2
  go (Tick t1 e1) (Tick t2 e2) = goTick t1 t2 <> go e1 e2
  go e1 e2 = compare (rank e1) (rank e2)

  goAlt :: Alt x -> Alt y -> Ordering
  goAlt (Alt p1 e1) (Alt p2 e2) = case (p1, p2) of
    (DataPat dc1 tvs1 ids1, DataPat dc2 tvs2 ids2) ->
      compare dc1 dc2 <>
        acmpTele env tvs1 tvs2 (\env1 ->
          acmpTele env1 ids1 ids2 (\env2 ->
            acmpTmLevels env2 e1 e2))
    (LitPat l1, LitPat l2) -> compare l1 l2 <> acmpTmLevels env e1 e2
    (DefaultPat, DefaultPat) -> acmpTmLevels env e1 e2
    _ -> compare (patRank p1) (patRank p2)

  patRank :: Pat x x' -> Int
  patRank = \case
    DataPat {} -> 0
    LitPat {} -> 1
    DefaultPat -> 2

  goTick :: TickInfo x -> TickInfo y -> Ordering
  goTick (SrcSpan s1) (SrcSpan s2) = leftmost_smallest s1 s2
  goTick (NameMod m1 t1) (NameMod m2 t2) = compare m1 m2 <> goTy t1 t2
  goTick DeDup DeDup = EQ
  goTick NoDeDup NoDeDup = EQ
  goTick (Attributes t1 e1) (Attributes t2 e2) = goTy t1 t2 <> go e1 e2
  goTick t1 t2 = compare (tickRank t1) (tickRank t2)

  tickRank :: TickInfo x -> Int
  tickRank = \case
    SrcSpan {} -> 0
    NameMod {} -> 1
    DeDup -> 2
    NoDeDup -> 3
    Attributes {} -> 4

  rank :: Term x -> Int
  rank = \case
    Var {} -> 0
    GlobalVar {} -> 1
    Data {} -> 2
    Literal {} -> 3
    Prim {} -> 4
    Cast {} -> 5
    App {} -> 6
    TyApp {} -> 7
    Lam {} -> 8
    TyLam {} -> 9
    Let NonRec {} _ -> 10
    Let Rec {} _ -> 11
    Case {} -> 12
    Tick {} -> 13

-- | Compare two telescopes, and continue in the environment under them, see
-- 'aeqTele'. Telescopes of different lengths are ordered by their length.
acmpTele :: AeqBinder b => Env -> Tele b x x' -> Tele b y y' -> (Env -> Ordering) -> Ordering
acmpTele env TNil TNil k = k env
acmpTele env (TCons b1 bs1) (TCons b2 bs2) k =
  acmpPayload env b1 b2 <> acmpTele (underB b1 b2 env) bs1 bs2 k
acmpTele _ TNil (TCons _ _) _ = LT
acmpTele _ (TCons _ _) TNil _ = GT
