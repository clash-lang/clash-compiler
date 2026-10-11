{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Smart constructors and destructors for scoped Clash Core: ports of the
  helpers of "Clash.Core.Term".

  Lists of binders become telescopes: e.g. 'mkLams' abstracts a term in scope
  @l@ over a @'Tele' t'IdBinder' n l@, giving a term in scope @n@. Collecting
  the binders of a term gives a telescope and a body in its scope, either as
  they are ('collectBndrs'), or opened, i.e. renamed where they clash with the
  scope ('openBndrs'), which is needed to go under them.

  The rewrite contexts ('Clash.Core.Term.CoreContext') are not here: they are
  part of the rewrite system.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Core.Scoped.Term
  ( -- * Applications
    mkApps
  , mkTmApps
  , mkTyApps
  , collectArgs
  , collectArgsTicks
  , primArg
    -- * Abstractions
  , AbsBinder (..)
  , mkAbstraction
  , mkLams
  , mkTyLams
  , collectBndrs
  , openBndrs
    -- * Ticks
  , mkTicks
  , stripTicks
  , stripAllTicks
  , collectTicks
  , partitionTicks
  , eqTickInfo
    -- * Telescopes
  , appendTele
  , splitTele
    -- * Traversals
  , walkTerm
  ) where

import qualified Data.DList as DList
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Text (Text)

import qualified Clash.Core.Term as C

import Clash.Core.Scoped.Alpha (aeqTm, aeqTy)
import Clash.Core.Scoped.FreeVars (HasNameBinder (..))
import Clash.Core.Scoped.Subst (openIdBinder, openTyBinder)
import Clash.Core.Scoped.Syntax

-- * Applications

-- | Apply a term to a list of terms and types
mkApps :: Term n -> [Either (Term n) (Type n)] -> Term n
mkApps = List.foldl' (\e a -> either (App e) (TyApp e) a)

-- | Apply a term to a list of terms
mkTmApps :: Term n -> [Term n] -> Term n
mkTmApps = List.foldl' App

-- | Apply a term to a list of types
mkTyApps :: Term n -> [Type n] -> Term n
mkTyApps = List.foldl' TyApp

-- | Split an application in the applied term and its arguments. Looks
-- through ticks, and drops them.
collectArgs :: Term n -> (Term n, [Either (Term n) (Type n)])
collectArgs = go []
 where
  go args (App e1 e2) = go (Left e2:args) e1
  go args (TyApp e t) = go (Right t:args) e
  go args (Tick _ e) = go args e
  go args e = (e, args)

-- | Like 'collectArgs', but also gives the ticks it looked through,
-- outermost last
collectArgsTicks :: Term n -> (Term n, [Either (Term n) (Type n)], [TickInfo n])
collectArgsTicks = go [] []
 where
  go args ticks (App e1 e2) = go (Left e2:args) ticks e1
  go args ticks (TyApp e t) = go (Right t:args) ticks e
  go args ticks (Tick s e) = go args (s:ticks) e
  go args ticks e = (e, args, ticks)

-- | If the term is an application of a primitive: its name, the number of
-- type arguments, and the number of term arguments
primArg :: Term n -> Maybe (Text, Int, Int)
primArg = go 0 0
 where
  go :: Int -> Int -> Term x -> Maybe (Text, Int, Int)
  go !tys !tms = \case
    App f _ -> go tys (tms + 1) f
    TyApp f _ -> go (tys + 1) tms f
    Tick _ e -> go tys tms e
    Prim p -> Just (C.primName p, tys, tms)
    _ -> Nothing

-- * Abstractions

-- | A binder of a lambda or of a type lambda
data AbsBinder (n :: S) (l :: S)
  = AbsId !(IdBinder n l)
  | AbsTy !(TyBinder n l)
  deriving (Show)

instance HasNameBinder AbsBinder where
  nameBinderOf = \case
    AbsId b -> nameBinderOf b
    AbsTy b -> nameBinderOf b
  {-# INLINE nameBinderOf #-}

-- | Abstract a term over a telescope of term and type variables
mkAbstraction :: Term l -> Tele AbsBinder n l -> Term n
mkAbstraction e = \case
  TNil -> e
  TCons (AbsId b) bs -> Lam b (mkAbstraction e bs)
  TCons (AbsTy b) bs -> TyLam b (mkAbstraction e bs)

-- | Abstract a term over a telescope of term variables
mkLams :: Term l -> Tele IdBinder n l -> Term n
mkLams e = \case
  TNil -> e
  TCons b bs -> Lam b (mkLams e bs)

-- | Abstract a term over a telescope of type variables
mkTyLams :: Term l -> Tele TyBinder n l -> Term n
mkTyLams e = \case
  TNil -> e
  TCons b bs -> TyLam b (mkTyLams e bs)

-- | Split the lambdas and type lambdas at the head of a term in their binders
-- and the body, without going under the binders. To go under them, use
-- 'openBndrs'.
collectBndrs :: Term n -> (forall l. Tele AbsBinder n l -> Term l -> r) -> r
collectBndrs e cont = case e of
  Lam b body -> collectBndrs body (\bs body' -> cont (TCons (AbsId b) bs) body')
  TyLam b body -> collectBndrs body (\bs body' -> cont (TCons (AbsTy b) bs) body')
  _ -> cont TNil e

-- | Split the lambdas and type lambdas at the head of a term in their binders
-- and the body, going under the binders: they are renamed where they clash
-- with the scope.
openBndrs
  :: Distinct n
  => Scope n
  -> Term n
  -> (forall l. DExt n l => Scope l -> Tele AbsBinder n l -> Term l -> r)
  -> r
openBndrs sc e cont = case e of
  Lam b body -> openIdBinder sc b body $ \sc1 b' body' ->
    openBndrs sc1 body' $ \sc2 bs body'' -> cont sc2 (TCons (AbsId b') bs) body''
  TyLam b body -> openTyBinder sc b body $ \sc1 b' body' ->
    openBndrs sc1 body' $ \sc2 bs body'' -> cont sc2 (TCons (AbsTy b') bs) body''
  _ -> cont sc TNil e

-- * Ticks

-- | Wrap a term in ticks, the last one outermost, leaving out duplicates
mkTicks :: Term n -> [TickInfo n] -> Term n
mkTicks tm ticks = List.foldl' (\e s -> Tick s e) tm (List.nubBy eqTickInfo ticks)

-- | Whether two ticks are equal, comparing their types and terms up to
-- alpha-equivalence, like the 'Eq' instance of 'C.TickInfo'
eqTickInfo :: TickInfo n -> TickInfo n -> Bool
eqTickInfo t1 t2 = case (t1, t2) of
  (SrcSpan s1, SrcSpan s2) -> s1 == s2
  (NameMod m1 ty1, NameMod m2 ty2) -> m1 == m2 && aeqTy ty1 ty2
  (DeDup, DeDup) -> True
  (NoDeDup, NoDeDup) -> True
  (Attributes ty1 e1, Attributes ty2 e2) -> aeqTy ty1 ty2 && aeqTm e1 e2
  _ -> False

-- | Remove the ticks at the head of a term
stripTicks :: Term n -> Term n
stripTicks (Tick _ e) = stripTicks e
stripTicks e = e

-- | Remove all ticks, also those of subterms
stripAllTicks :: Term n -> Term n
stripAllTicks = go
 where
  go :: Term x -> Term x
  go = \case
    Lam b e -> Lam b (go e)
    TyLam b e -> TyLam b (go e)
    App e1 e2 -> App (go e1) (go e2)
    TyApp e ty -> TyApp (go e) ty
    Let (NonRec b x) e -> Let (NonRec b (go x)) (go e)
    Let (Rec bs xs) e -> Let (Rec bs (map go xs)) (go e)
    Case e ty alts -> Case (go e) ty [Alt p (go a) | Alt p a <- alts]
    Cast e t1 t2 -> Cast (go e) t1 t2
    Tick _ e -> go e
    e -> e

-- | Split the ticks at the head of a term from it, outermost last
collectTicks :: Term n -> (Term n, [TickInfo n])
collectTicks = go []
 where
  go ticks (Tick s e) = go (s:ticks) e
  go ticks e = (e, ticks)

-- | Partition ticks in source ticks and the others, e.g. name modifiers
partitionTicks :: [TickInfo n] -> ([TickInfo n], [TickInfo n])
partitionTicks = List.partition (\case SrcSpan {} -> True; _ -> False)

-- * Telescopes

-- | Append two telescopes
appendTele :: Tele b n i -> Tele b i l -> Tele b n l
appendTele TNil bs = bs
appendTele (TCons b bs) cs = TCons b (appendTele bs cs)

-- | Split a telescope after the given number of binders (or at its end, if
-- it has fewer)
splitTele :: Int -> Tele b n l -> (forall i. Tele b n i -> Tele b i l -> r) -> r
splitTele k bs cont = case bs of
  TCons b bs'
    | k > 0 -> splitTele (k - 1) bs' (\pre post -> cont (TCons b pre) post)
  _ -> cont TNil bs

-- * Traversals

-- | Apply a function to every subterm of a term (but not the terms in
-- ticks), in pre-order, and collect the results
walkTerm :: forall a n. (forall x. Term x -> Maybe a) -> Term n -> [a]
walkTerm f = catMaybes . DList.toList . go
 where
  go :: Term x -> DList.DList (Maybe a)
  go t = DList.cons (f t) $ case t of
    Var {} -> mempty
    GlobalVar {} -> mempty
    Data {} -> mempty
    Literal {} -> mempty
    Prim {} -> mempty
    Lam _ e -> go e
    TyLam _ e -> go e
    App e1 e2 -> go e1 <> go e2
    TyApp e _ -> go e
    Let (NonRec _ x) e -> go e <> go x
    Let (Rec _ xs) e -> go e <> foldMap go xs
    Case e _ alts -> go e <> foldMap (\(Alt _ a) -> go a) alts
    Cast e _ _ -> go e
    Tick _ e -> go e
