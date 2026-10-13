{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Checks that "Clash.GHC.Scoped.GHC2Core" gives the same terms as converting
  the terms of 'Clash.GHC.GHC2Core.coreToTerm' to scoped Core. Temporary:
  Phase 7 of the port to scoped Core deletes 'Clash.GHC.GHC2Core.coreToTerm',
  and this with it.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.GHC.Scoped.Check
  ( Outcome (..)
  , compareTerms
  , compareTypes
  , primAtTypeShape
  ) where

import qualified Control.Lens as Lens

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.FreeVars as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Util as C (undefinedTy)

import Clash.Core.Scoped.Alpha (aeqTm, aeqTy)
import Clash.Core.Scoped.Syntax

-- | How two terms compare
data Outcome
  = Identical
  -- ^ The same, with the same raw names
  | AlphaEquivalent String
  -- ^ Alpha-equivalent, with the first difference
  | Different String
  -- ^ Not alpha-equivalent, with the first difference
  deriving (Show)

-- | The worse of two outcomes
instance Semigroup Outcome where
  o@Different{} <> _ = o
  _ <> o@Different{} = o
  o@AlphaEquivalent{} <> _ = o
  _ <> o = o

-- | Compare two closed terms: structurally, with raw names and the
-- occurrence names, sorts and source locations of names; and if they differ,
-- up to alpha-equivalence.
compareTerms :: Term 'VoidS -> Term 'VoidS -> Outcome
compareTerms t1 t2 = case cmpTm t1 t2 of
  Nothing -> Identical
  Just d
    | aeqTm t1 t2 -> AlphaEquivalent d
    | otherwise -> Different d

-- | Compare two closed types, like 'compareTerms'
compareTypes :: Type 'VoidS -> Type 'VoidS -> Outcome
compareTypes t1 t2 = case cmpTy t1 t2 of
  Nothing -> Identical
  Just d
    | aeqTy t1 t2 -> AlphaEquivalent d
    | otherwise -> Different d

-- | Turn the primitives @_RUBBISH_@ of open types that
-- 'Clash.GHC.GHC2Core.coreToTerm' makes into the shape of
-- 'Clash.Core.Scoped.Util.primAtType', as "Clash.GHC.Scoped.GHC2Core" makes
-- them.
primAtTypeShape :: C.Term -> C.Term
primAtTypeShape = go
 where
  go = \case
    C.Prim p
      | C.primName p == "_RUBBISH_"
      , Lens.notNullOf C.typeFreeVars (C.primType p)
      -> C.TyApp (C.Prim p {C.primType = C.undefinedTy}) (C.primType p)
    C.Lam v e -> C.Lam v (go e)
    C.TyLam tv e -> C.TyLam tv (go e)
    C.App e1 e2 -> C.App (go e1) (go e2)
    C.TyApp e ty -> C.TyApp (go e) ty
    C.Let (C.NonRec v x) e -> C.Let (C.NonRec v (go x)) (go e)
    C.Let (C.Rec bs) e -> C.Let (C.Rec [(v, go x) | (v, x) <- bs]) (go e)
    C.Case e ty alts -> C.Case (go e) ty [(p, go x) | (p, x) <- alts]
    C.Cast e t1 t2 -> C.Cast (go e) t1 t2
    C.Tick t e -> C.Tick (goTick t) (go e)
    e -> e

  goTick = \case
    C.Attributes ty e -> C.Attributes ty (go e)
    t -> t

-- * Structural comparison

-- | The first difference, if any
type Cmp = Maybe String

(<+>) :: Cmp -> Cmp -> Cmp
Nothing <+> c = c
c <+> _ = c
infixr 5 <+>

diff :: (Show a, Show b) => String -> a -> b -> Cmp
diff what a b = Just (what ++ ": " ++ take 400 (show a) ++ "\n  vs " ++ take 400 (show b))

check :: (Show a) => String -> (a -> a -> Bool) -> a -> a -> Cmp
check what eq a b
  | eq a b = Nothing
  | otherwise = diff what a b

cmpName :: C.Name a -> C.Name b -> Cmp
cmpName n1 n2
  | C.nameOcc n1 == C.nameOcc n2
  , C.nameSort n1 == C.nameSort n2
  , C.nameLoc n1 == C.nameLoc n2
  = Nothing
  | otherwise = diff "name" n1 n2

cmpTy :: Type n -> Type m -> Cmp
cmpTy t1 t2 = case (t1, t2) of
  (VarTy o1 _ n1, VarTy o2 _ n2) ->
    check "type variable" (==) (tyNameRaw n1) (tyNameRaw n2) <+> cmpName o1 o2
  (ConstTy c1, ConstTy c2) -> check "type constant" (==) c1 c2
  (ForAllTy b1 t1', ForAllTy b2 t2') -> cmpTyBinder b1 b2 <+> cmpTy t1' t2'
  (AppTy a1 b1, AppTy a2 b2) -> cmpTy a1 a2 <+> cmpTy b1 b2
  (LitTy l1, LitTy l2) -> check "type literal" (==) l1 l2
  (AnnType a1 s1, AnnType a2 s2) -> check "annotation" (==) a1 a2 <+> cmpTy s1 s2
  _ -> diff "type" t1 t2

cmpTyBinder :: TyBinder n l -> TyBinder m k -> Cmp
cmpTyBinder b1 b2 =
  check "type binder" (==) (tyNameRaw (tyNameOf b1)) (tyNameRaw (tyNameOf b2))
    <+> cmpName (tyBinderOcc b1) (tyBinderOcc b2)
    <+> cmpTy (tyBinderKind b1) (tyBinderKind b2)

cmpIdBinder :: IdBinder n l -> IdBinder m k -> Cmp
cmpIdBinder b1 b2 =
  check "binder" (==) (tmNameRaw (tmNameOf b1)) (tmNameRaw (tmNameOf b2))
    <+> cmpName (idBinderOcc b1) (idBinderOcc b2)
    <+> cmpTy (idBinderType b1) (idBinderType b2)

cmpTele :: (forall x y x' y'. b x y -> b x' y' -> Cmp) -> Tele b n l -> Tele b m k -> Cmp
cmpTele cmpB tl1 tl2 = case (tl1, tl2) of
  (TNil, TNil) -> Nothing
  (TCons b1 bs1, TCons b2 bs2) -> cmpB b1 b2 <+> cmpTele cmpB bs1 bs2
  _ -> Just "telescopes of different lengths"

cmpPrim :: PrimInfo -> PrimInfo -> Cmp
cmpPrim p1 p2 =
  check "primitive" (==) (primName p1) (primName p2)
    <+> cmpTy (primType p1) (primType p2)
    <+> check "primitive work info" (==) (primWorkInfo p1) (primWorkInfo p2)
    <+> check "primitive multi result" (==) (primMultiResult p1) (primMultiResult p2)
    <+> case (primUnfolding p1, primUnfolding p2) of
          (NoUnfolding, NoUnfolding) -> Nothing
          (Unfolding g1, Unfolding g2) -> cmpGlobal g1 g2
          _ -> diff "primitive unfolding" (primUnfolding p1) (primUnfolding p2)

cmpGlobal :: GlobalId -> GlobalId -> Cmp
cmpGlobal g1 g2 =
  check "global" (==) g1 g2
    <+> cmpName (globalIdName g1) (globalIdName g2)
    <+> cmpTy (globalIdType g1) (globalIdType g2)

cmpTm :: Term n -> Term m -> Cmp
cmpTm e1 e2 = case (e1, e2) of
  (Var o1 _ n1, Var o2 _ n2) ->
    check "variable" (==) (tmNameRaw n1) (tmNameRaw n2) <+> cmpName o1 o2
  (GlobalVar g1, GlobalVar g2) -> cmpGlobal g1 g2
  (Data dc1, Data dc2) -> check "data constructor" (==) (C.dcUniq dc1) (C.dcUniq dc2)
  (Literal l1, Literal l2) -> check "literal" (==) l1 l2
  (Prim p1, Prim p2) -> cmpPrim p1 p2
  (Lam b1 x1, Lam b2 x2) -> cmpIdBinder b1 b2 <+> cmpTm x1 x2
  (TyLam b1 x1, TyLam b2 x2) -> cmpTyBinder b1 b2 <+> cmpTm x1 x2
  (App f1 a1, App f2 a2) -> cmpTm f1 f2 <+> cmpTm a1 a2
  (TyApp f1 t1, TyApp f2 t2) -> cmpTm f1 f2 <+> cmpTy t1 t2
  (Let (NonRec b1 x1) y1, Let (NonRec b2 x2) y2) ->
    cmpTm x1 x2 <+> cmpIdBinder b1 b2 <+> cmpTm y1 y2
  (Let (Rec bs1 xs1) y1, Let (Rec bs2 xs2) y2) ->
    cmpTele cmpIdBinder bs1 bs2 <+> cmpList cmpTm xs1 xs2 <+> cmpTm y1 y2
  (Case s1 t1 as1, Case s2 t2 as2) ->
    cmpTm s1 s2 <+> cmpTy t1 t2 <+> cmpList cmpAlt as1 as2
  (Cast x1 a1 b1, Cast x2 a2 b2) -> cmpTm x1 x2 <+> cmpTy a1 a2 <+> cmpTy b1 b2
  (Tick t1 x1, Tick t2 x2) -> cmpTick t1 t2 <+> cmpTm x1 x2
  _ -> diff "term" e1 e2

cmpList :: (a -> b -> Cmp) -> [a] -> [b] -> Cmp
cmpList f xs ys
  | length xs == length ys = foldr (<+>) Nothing (zipWith f xs ys)
  | otherwise = Just "lists of different lengths"

cmpAlt :: Alt n -> Alt m -> Cmp
cmpAlt (Alt p1 x1) (Alt p2 x2) = case (p1, p2) of
  (DataPat dc1 tvs1 ids1, DataPat dc2 tvs2 ids2) ->
    check "pattern" (==) (C.dcUniq dc1) (C.dcUniq dc2)
      <+> cmpTele cmpTyBinder tvs1 tvs2
      <+> cmpTele cmpIdBinder ids1 ids2
      <+> cmpTm x1 x2
  (LitPat l1, LitPat l2) -> check "literal pattern" (==) l1 l2 <+> cmpTm x1 x2
  (DefaultPat, DefaultPat) -> cmpTm x1 x2
  _ -> diff "pattern" p1 p2

cmpTick :: TickInfo n -> TickInfo m -> Cmp
cmpTick t1 t2 = case (t1, t2) of
  (SrcSpan s1, SrcSpan s2) -> check "source span" (==) s1 s2
  (NameMod m1 ty1, NameMod m2 ty2) -> check "name modifier" (==) m1 m2 <+> cmpTy ty1 ty2
  (DeDup, DeDup) -> Nothing
  (NoDeDup, NoDeDup) -> Nothing
  (Attributes ty1 x1, Attributes ty2 x2) -> cmpTy ty1 ty2 <+> cmpTm x1 x2
  _ -> diff "tick" t1 t2
