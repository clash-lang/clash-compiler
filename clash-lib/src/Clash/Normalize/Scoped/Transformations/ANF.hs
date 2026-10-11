{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017-2018, Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformations for converting scoped Clash Core to A-normal form: the
  ported counterparts of "Clash.Normalize.Transformations.ANF". See
  Note [ANF in Clash] there for the shape of the result.

  = How 'makeANF' moves bindings

  'makeANF' turns a term into a single recursive let-expression (after the
  lambdas of a function), whose bindings are the let-bindings of the term, the
  bindings it creates for subterms, and bindings for the variables that
  patterns bind. Clash Core does this by first making all binders of the term
  distinct ('Clash.Core.Subst.freshenTm'), so that it can move a binding out
  from under the binders on its path: these binders become let-binders of the
  same let-expression, with the same names.

  Scoped Core can't move a term out from under binders: the binders scope over
  the term, so the term lives in an extension of the scope it moves to. So the
  traversal here lets the scope /grow/ instead: it works in a single scope
  (@q@ below) that extends the scope of the root (@r@) with every binder of the
  result's let-expression, in the order in which it meets them. It goes
  through the original term with a substitution from the scope of the
  original subterm into @q@, and

  * a let-binder, or a variable bound by the pattern of an alternative, is
    renamed into @q@ ('substIdBinder'), extending it, when the traversal gets
    to it;

  * a new binding for a subterm gets a fresh binder in @q@ ('mkTmBinderFor').

  So every binder is created exactly once, in the scope in which all the
  binders before it are visible, and the bindings never have to be moved: at
  the end ('regionLet'), the binders, a telescope from @r@ to @q@, are rebound
  in a recursive let-expression in the order of Clash Core. A
  substitution-carrying traversal renames a binder in O(1) (an entry in the
  substitution), unlike opening binders one by one, which would rename a
  binder by substituting in its scope whenever a new binder took its name.
  That is also why this doesn't use 'Clash.Core.Scoped.Subst.freshenDistinct':
  the names come from the growing scope, so they are distinct by construction.

  = Pattern variables

  Clash Core let-binds a variable that an alternative's pattern binds to a
  projection of the subject ('mkSelectorCase'), but keeps the pattern as it
  is: the pattern variable then shadows the let-binder of the same name, and
  the alternative's right-hand side refers to the pattern variable, while the
  bindings moved out of the alternative refer to the let-binder. In scoped
  Core, these are two binders by construction: the variable that the
  traversal renamed into @q@ becomes the let-binder of the projection, and the
  alternative gets a pattern with fresh binders, for which its right-hand side
  is substituted ('mkAlt'). The result is alpha-equivalent to that of Clash
  Core.

  = Regions

  Bindings created under binders that can't become let-binders stay under
  them, in a let-expression of their own: a /region/, with its own root scope.
  These are

  * the binders of an alternative whose pattern binds existential type
    variables that occur in the types of its fields, like in Clash Core (see
    'Clash.Netlist.Util.bindsExistentials');

  * the binders of an alternative whose existential type variables occur in
    its right-hand side, but not in the types of its fields. Clash Core moves
    the bindings out of their scope;

  * lambdas and type lambdas in the term. Clash Core moves the bindings out of
    them, which goes wrong if they mention the variable the lambda binds; it
    relies on 'nonRepANF' to remove such lambdas first.

  The other existential type variables don't occur in the alternative, and
  are substituted away (see 'anfAlt'). So the variables that the growing
  scope gets are term variables only.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.Normalize.Scoped.Transformations.ANF
  ( makeANF
  , nonRepANF
  , nonRepANFWorker
  ) where

import qualified Control.Lens as Lens
import Data.Functor.Compose (Compose (..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Kind as K
import qualified Data.Text as Text
import qualified Data.Text.Extra as Text (showt)
import GHC.Exts (State#)
import GHC.Stack (HasCallStack)

import Clash.Explicit.SimIO (SimIO)
import Clash.Signal.Internal (Signal (..))

import Clash.Core.DataCon (DataCon (..))
import Clash.Core.Literal (Literal)
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Util (curLoc)

import Clash.Core.Scoped.FreeVars
  (anyBoundOccursInType, freeTmVars, freeTyVars, teleNameBinders)
import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.NameMap
  (NameBinders, memberNameSet, nameMapFromList, nameSetToList)
import Clash.Core.Scoped.Subst
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term
  (appendTele, collectArgs, collectTicks, mkTicks, partitionTicks, stripTicks)
import Clash.Core.Scoped.TermInfo (isCon, isLocalVar, isPrim, isVar)
import Clash.Core.Scoped.Type (TypeView (..), coreView, tyView)
import Clash.Core.Scoped.TypeEnv (TypeEnv)
import Clash.Core.Scoped.Util (internalName, mkSelectorCase, systemName)
import Clash.Normalize.Scoped.Transformations.Specialize (specialize)
import Clash.Normalize.Types (NormalizeSession)
import Clash.Rewrite.Scoped
  (TransformContext, changed, closestLetBinder, tfScope, underLam)
import Clash.Rewrite.Scoped.Util (isUntranslatable, mkTmBinderFor, substVars)
import Clash.Rewrite.Scoped.WorkFree (isConstant, isConstantNotClockReset)
import Clash.Rewrite.StrategyDSL (Transformation, onApp, toScopedTransformation)
import Clash.Rewrite.Types (scopedTypeEnv)

-- * makeANF

-- | Turn an expression into a modified ANF-form. As opposed to standard ANF,
-- constants do not become let-bound. See
-- 'Clash.Normalize.Transformations.ANF.makeANF', and the module header for
-- how this moves bindings.
makeANF
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> NormalizeSession (Term n)
makeANF ctx (Lam b e) = underLam ctx b e $ \ctx' b' e' -> Lam b' <$> makeANF ctx' e'
makeANF _ e@TyLam {} = return e
makeANF ctx e0 = do
  let sc = tfScope ctx
  Grown rg e2 <- anfTerm (Env (closestLetBinder ctx)) (idSubst sc) (emptyRegion sc) e0
  case rgBinds rg of
    -- Like Clash Core, this gives the original term when it created no
    -- let-bindings: even if it changed a case-expression, or created
    -- let-bindings in a region.
    [] -> return e0
    _ -> do
      let (e3, ticks) = collectTicks e2
          (srcTicks, nmTicks) = partitionTicks ticks
          bndrs = teleNameBinders (chainTele (rgChain rg))
      -- Ensure that `AppendName` ticks still scope over the entire expression
      case regionLet sc rg (mkTicks e3 srcTicks) of
        Just e4 -> changed (mkTicks e4 (map (unsinkTick bndrs) nmTicks))
        Nothing -> error ($(curLoc) ++ "makeANF: no let-bindings")
{-# SCC makeANF #-}

-- | The closest let-binder on the path to a subterm, from which the names of
-- new binders are derived, see 'Clash.Rewrite.Scoped.mkDerivedName'
newtype Env = Env (Maybe (C.Name C.Term))

-- | The 'Env' of the right-hand side of a let-binding with the given binder
inLetBinding :: C.Name C.Term -> Env
inLetBinding = Env . Just

-- | A name for a new binder, see 'Clash.Rewrite.Scoped.mkDerivedName'
derivedName :: Env -> C.OccName -> C.Name C.Term
derivedName (Env nm) sf = case nm of
  Just nm' -> C.appendToName nm' ('_' `Text.cons` sf)
  Nothing -> C.mkUnsafeInternalName sf 0

-- ** Regions

-- | The binders from the root scope @r@ of a region to the scope @q@ it grew
-- to, in the order in which they were created. A snoc-list of telescopes.
data Chain (r :: S) (q :: S) where
  CNil :: Chain r r
  CSnoc :: !(Chain r i) -> !(Tele IdBinder i q) -> Chain r q

-- | The binders of a chain, as a telescope
chainTele :: Chain r q -> Tele IdBinder r q
chainTele ch0 = go ch0 TNil
 where
  go :: Chain r i -> Tele IdBinder i q -> Tele IdBinder r q
  go CNil acc = acc
  go (CSnoc ch bs) acc = go ch (appendTele bs acc)

-- | A binding of a region, by the raw name of its binder in the chain
data Told (n :: S) = Told !Int !(Term n)

instance Sinkable Told where
  sinkabilityProof r (Told k e) = Told k (sinkabilityProof r e)

-- | The state of the conversion of a region with root scope @r@, grown to
-- scope @q@. Every binder of the chain is eventually a binding of the
-- region; this is the order of the let-expression of Clash Core, see
-- 'Clash.Normalize.Transformations.ANF.tellBinders'.
data Region (r :: S) (q :: S) = Region
  { rgScope :: !(Scope q)
  , rgChain :: !(Chain r q)
  , rgBinds :: [Told q]
  -- ^ The bindings so far, the most recently created groups first
  }

-- | A region that hasn't grown
emptyRegion :: Scope r -> Region r r
emptyRegion sc = Region sc CNil []

-- | Grow a region by binders
growRegion :: DExt q q' => Scope q' -> Tele IdBinder q q' -> Region r q -> Region r q'
growRegion _ TNil rg = rg
growRegion sc' bs (Region _ ch binds) = Region sc' (CSnoc ch bs) (sink1 binds)

-- | Grow a region by a binder
growRegion1 :: DExt q q' => Scope q' -> IdBinder q q' -> Region r q -> Region r q'
growRegion1 sc' b = growRegion sc' (TCons b TNil)

-- | Add bindings, in front of the bindings so far, like
-- 'Clash.Normalize.Transformations.ANF.tellBinders'
tellBinds :: [Told q] -> Region r q -> Region r q
tellBinds bs rg = rg {rgBinds = bs ++ rgBinds rg}

-- | A value in the scope @q'@ that a region grew to from @q@
data Grown (r :: S) (q :: S) (a :: S -> K.Type) where
  Grown :: DExt q q' => !(Region r q') -> !(a q') -> Grown r q a

-- | Move a substitution into the scope a region grew to
moveSubst :: DExt o o' => Region r o' -> Subst i o -> Subst i o'
moveSubst rg = extendSubstScope (rgScope rg)

-- | Run a step on the elements of a list in turn, each in the scope the
-- region grew to so far
traverseGrown
  :: (Distinct q, Sinkable a)
  => (forall q'. DExt q q' => Region r q' -> x -> NormalizeSession (Grown r q' a))
  -> Region r q
  -> [x]
  -> NormalizeSession (Grown r q (Compose [] a))
traverseGrown _ rg [] = pure (Grown rg (Compose []))
traverseGrown f rg (x:xs) = do
  Grown rg1 a <- f rg x
  Grown rg2 (Compose as) <- traverseGrown (\rg' x' -> f rg' x') rg1 xs
  pure (Grown rg2 (Compose (sink a : as)))

-- | The raw name of a binder
binderRaw :: IdBinder n l -> Int
binderRaw = tmNameRaw . tmNameOf

-- | The raw names and human readable names of the binders of a telescope
teleInfo :: Tele IdBinder n l -> [(Int, C.Name C.Term)]
teleInfo TNil = []
teleInfo (TCons b bs) = (binderRaw b, idBinderOcc b) : teleInfo bs

-- | The let-expression that binds the bindings of a region around a term, in
-- the order of the bindings, see 'Region'. 'Nothing' if the region has no
-- bindings.
--
-- The binders of the chain get fresh names, as their order changes.
regionLet
  :: (HasCallStack, Distinct r, Distinct q)
  => Scope r
  -> Region r q
  -> Term q
  -> Maybe (Term r)
regionLet scR (Region _ ch binds) body
  | null binds = Nothing
  | IntMap.size info /= length binds || IntSet.size bindRawSet /= length binds
  = error ($(curLoc) ++ "regionLet: the binders of the region aren't its bindings")
  | otherwise = Just $
      withFreshIdBinders scR (map binderOf bindRaws) $ \scM tele' vars' ->
        let new = IntMap.fromList (zip bindRaws vars')
            entries = [new IntMap.! k | (k, _) <- teleInfo tele]
        in case extendTmSubstTele (extendSubstScope scM (idSubst scR)) tele entries of
             Just s -> Let (Rec tele' [substTm s e | Told _ e <- binds]) (substTm s body)
             Nothing -> error ($(curLoc) ++ "regionLet: wrong number of variables")
 where
  tele = chainTele ch
  info = IntMap.fromList
    (zipWith (\(k, occ) ty -> (k, (occ, ty))) (teleInfo tele) (teleIdBinderTypes tele))
  bindRaws = [k | Told k _ <- binds]
  bindRawSet = IntSet.fromList bindRaws
  binderOf k = case IntMap.lookup k info of
    Just b -> b
    Nothing -> error ($(curLoc) ++ "regionLet: a binding without a binder")

-- | Convert a region to ANF: the subterm of a binder that doesn't become a
-- let-binder, see the module header
anfRegion :: (HasCallStack, Distinct r) => Env -> Subst i r -> Term i -> NormalizeSession (Term r)
anfRegion env s e = do
  let sc = substScope s
  Grown rg e' <- anfTerm env s (emptyRegion sc) e
  case regionLet sc rg e' of
    Just e'' -> pure e''
    Nothing -> case rgChain rg of
      CNil -> pure e'
      CSnoc {} -> error ($(curLoc) ++ "anfRegion: binders without bindings")

-- | Move a tick out of the scope of binders. Name modifiers only mention
-- type variables, which the binders of a region don't bind.
unsinkTick :: HasCallStack => NameBinders n l -> TickInfo l -> TickInfo n
unsinkTick bs t = case t of
  SrcSpan sp -> SrcSpan sp
  NameMod m ty -> NameMod m (orError (unsinkType bs ty))
  DeDup -> DeDup
  NoDeDup -> NoDeDup
  Attributes ty e -> Attributes (orError (unsinkType bs ty)) (orError (unsinkTerm bs e))
 where
  orError :: Maybe a -> a
  orError = maybe (error ($(curLoc) ++ "unsinkTick: tick mentions a let-binder")) id

-- ** The traversal

-- | Convert a subterm to ANF in a region, given the substitution from its
-- scope into the scope of the region. Like 'Clash.Rewrite.Combinators.allR'
-- followed by 'Clash.Normalize.Transformations.ANF.collectANF', bottom-up,
-- except for regions, see 'Clash.Normalize.Transformations.ANF.anfTraverse'.
anfTerm
  :: (HasCallStack, Distinct q)
  => Env
  -> Subst i q
  -> Region r q
  -> Term i
  -> NormalizeSession (Grown r q Term)
anfTerm env s rg e = case e of
  Var {} -> leaf
  GlobalVar {} -> leaf
  Data {} -> leaf
  Literal {} -> leaf
  Prim {} -> leaf
  Lam b x -> substIdBinder s b $ \s1 b1 -> do
    x1 <- anfRegion env s1 x
    pure (Grown rg (Lam b1 x1))
  TyLam b x -> substTyBinder s b $ \s1 b1 -> do
    x1 <- anfRegion env s1 x
    pure (Grown rg (TyLam b1 x1))
  App f a -> do
    Grown rg1 f1 <- anfTerm env s rg f
    Grown rg2 a1 <- anfTerm env (moveSubst rg1 s) rg1 a
    Grown rg3 e1 <- collectApp env rg2 (App (sink f1) a1)
    pure (Grown rg3 e1)
  TyApp f ty -> do
    Grown rg1 f1 <- anfTerm env s rg f
    pure (Grown rg1 (TyApp f1 (substTy (moveSubst rg1 s) ty)))
  Cast x ty1 ty2 -> do
    Grown rg1 x1 <- anfTerm env s rg x
    let s1 = moveSubst rg1 s
    pure (Grown rg1 (Cast x1 (substTy s1 ty1) (substTy s1 ty2)))
  Tick t x -> do
    Grown rg1 x1 <- anfTerm env s rg x
    pure (Grown rg1 (Tick (substTickInfo (moveSubst rg1 s) t) x1))
  Let (NonRec b x) body -> do
    Grown rg1 x1 <- anfTerm (inLetBinding (idBinderOcc b)) s rg x
    substIdBinder (moveSubst rg1 s) b $ \s2 b2 -> do
      let rg2 = growRegion1 (substScope s2) b2 rg1
      Grown rg3 body3 <- anfTerm env s2 rg2 body
      Grown rg4 e4 <- collectLet rg3 [Told (binderRaw b2) (sink x1)] body3
      pure (Grown rg4 e4)
  Let (Rec bs xs) body -> substTele substIdBinder s bs $ \s1 bs1 -> do
    let rg1 = growRegion (substScope s1) bs1 rg
    Grown rg2 (Compose xs2) <-
      traverseGrown
        (\rg' (occ, x) -> anfTerm (inLetBinding occ) (moveSubst rg' s1) rg' x)
        rg1 (zip (map snd (teleInfo bs1)) xs)
    Grown rg3 body3 <- anfTerm env (moveSubst rg2 s1) rg2 body
    let binds = zipWith Told (map fst (teleInfo bs1)) (sink1 xs2)
    Grown rg4 e4 <- collectLet rg3 binds body3
    pure (Grown rg4 e4)
  Case subj ty alts -> do
    Grown rg1 subj1 <- anfTerm env s rg subj
    Grown rg2 (Compose alts2) <-
      traverseGrown (\rg' alt -> anfAlt env (moveSubst rg' s) rg' alt) rg1 alts
    Grown rg3 e3 <-
      collectCase env rg2 (sink subj1) (substTy (moveSubst rg2 s) ty) alts2
    pure (Grown rg3 e3)
 where
  leaf = pure (Grown rg (substTm s e))

-- | An alternative, converted to ANF up to its right-hand side
data AltRes (n :: S)
  = ARegion !(Alt n)
  -- ^ A region, see the module header: left as it is
  | APat !DataCon !(SomeTyTele n) [Term n] !(Term n)
  -- ^ A constructor whose existential type variables don't occur in the
  -- types of its fields: its existential type variables, the variables its
  -- pattern bound, which are binders of the chain, and the right-hand side
  | ALit !Literal !(Term n)
  | ADefault !(Term n)

instance Sinkable AltRes where
  sinkabilityProof r = \case
    ARegion alt -> ARegion (sinkabilityProof r alt)
    APat dc tvs vs e ->
      APat dc (sinkabilityProof r tvs) (map (sinkabilityProof r) vs) (sinkabilityProof r e)
    ALit l e -> ALit l (sinkabilityProof r e)
    ADefault e -> ADefault (sinkabilityProof r e)

-- | The existential type variables of a pattern, see 'APat'
data SomeTyTele (n :: S) where
  SomeTyTele :: !(Tele TyBinder n l) -> SomeTyTele n

instance Sinkable SomeTyTele where
  sinkabilityProof r (SomeTyTele bs) = sinkTele sinkTyBinder r bs $ \_ bs' -> SomeTyTele bs'

-- | Convert the right-hand side of an alternative to ANF.
--
-- The alternative of a constructor whose existential type variables don't
-- occur in the types of its fields isn't a region, like in Clash Core: e.g.
-- @Cons@ of @Vec@ when its length is known. Its variables become binders of
-- the chain, and its existential type variables are substituted away, as they
-- don't become binders of the let-expression. If its right-hand side
-- mentions them, Clash Core moves bindings out of their scope; here, the
-- alternative is a region then.
anfAlt
  :: (HasCallStack, Distinct q)
  => Env
  -> Subst i q
  -> Region r q
  -> Alt i
  -> NormalizeSession (Grown r q AltRes)
anfAlt env s rg (Alt pat e) = case pat of
  DataPat dc exts ids
    | not (bindsExistentials exts ids)
    , not (anyTyBinderOccursIn exts e)
    -> substTele substTyBinder s exts $ \_ exts1 ->
         case extendTySubstTele s exts (map (const unusedExistential) (teleTyRaws exts)) of
           Nothing -> error ($(curLoc) ++ "anfAlt: wrong number of types")
           Just sE -> substTele substIdBinder sE ids $ \s1 ids1 -> do
             let rg1 = growRegion (substScope s1) ids1 rg
             Grown rg2 e2 <- anfTerm env s1 rg1 e
             pure (Grown rg2 (APat dc (sink (SomeTyTele exts1)) (sink1 (teleIdVars ids1)) e2))
  DataPat {} -> substPat s pat $ \s1 pat1 -> do
    e1 <- anfRegion env s1 e
    pure (Grown rg (ARegion (Alt pat1 e1)))
  LitPat l -> do
    Grown rg1 e1 <- anfTerm env s rg e
    pure (Grown rg1 (ALit l e1))
  DefaultPat -> do
    Grown rg1 e1 <- anfTerm env s rg e
    pure (Grown rg1 (ADefault e1))

-- | Whether existential type variables of a pattern occur in the types of its
-- fields, see 'Clash.Netlist.Util.bindsExistentials'
bindsExistentials :: Tele TyBinder n i -> Tele IdBinder i l -> Bool
bindsExistentials TNil _ = False
bindsExistentials exts ids =
  any (anyBoundOccursInType (teleNameBinders exts)) (teleIdBinderTypes ids)

-- | Whether the type variables of a telescope occur free in a term in its
-- scope, or in the scope of binders after it
anyTyBinderOccursIn :: Tele TyBinder n i -> Term l -> Bool
anyTyBinderOccursIn TNil _ = False
anyTyBinderOccursIn exts e =
  any (`IntSet.member` IntSet.fromList (teleTyRaws exts))
      [tyNameRaw nm | nm <- nameSetToList (freeTyVars e)]

-- | The raw names of the binders of a telescope of type variables
teleTyRaws :: Tele TyBinder n l -> [Int]
teleTyRaws TNil = []
teleTyRaws (TCons b bs) = tyNameRaw (tyNameOf b) : teleTyRaws bs

-- | The type substituted for an existential type variable that doesn't occur
-- in the alternative, see 'anfAlt'. It doesn't occur in the result.
unusedExistential :: Type n
unusedExistential =
  LitTy (C.SymTy "Clash.Normalize.Scoped.Transformations.ANF.unusedExistential")

-- | Rebuild an alternative. The pattern of an 'APat' gets fresh binders, see
-- the module header.
mkAlt :: Distinct n => Scope n -> AltRes n -> Alt n
mkAlt sc = \case
  ARegion alt -> alt
  ALit l e -> Alt (LitPat l) e
  ADefault e -> Alt DefaultPat e
  APat dc (SomeTyTele exts) vs e ->
    substTele substTyBinder (idSubst sc) exts $ \sE exts1 ->
      withFreshIdBinders (substScope sE) [(occ, sink ty) | Var occ ty _ <- vs] $ \sc1 ids vs1 ->
        let m = nameMapFromList [(nm, v1) | (Var _ _ nm, v1) <- zip (sink1 vs) vs1]
        in  Alt (DataPat dc exts1 ids) (substVars sc1 m (sink e))

-- ** Creating let-bindings

-- | Is the given type IO-like, see
-- 'Clash.Normalize.Transformations.ANF.isSimIOTy'
isSimIOTy :: Distinct n => TypeEnv -> Scope n -> Type n -> Bool
isSimIOTy te sc ty = case tyView (coreView te sc ty) of
  TyConApp tcNm args
    | C.nameOcc tcNm == Text.showt ''SimIO
    -> True
    | C.nameOcc tcNm == Text.showt ''(# , #)
    , [_,_,st,_] <- args
    -> isStateTokenTy st
  FunTy _ res -> isSimIOTy te sc res
  _ -> False
 where
  isStateTokenTy st = case tyView (coreView te sc st) of
    TyConApp tcNm _ -> C.nameOcc tcNm == Text.showt ''State#
    _ -> False

-- | Let-bind the argument of an application, see
-- 'Clash.Normalize.Transformations.ANF.collectANF' and Note [ANF no let-bind]
-- there.
--
-- Clash Core also lifts the let-bindings out of an untranslatable argument
-- that is a let-expression, which can't occur: the traversal is bottom-up, and
-- converting a let-expression gives its body or a variable.
collectApp
  :: (HasCallStack, Distinct q)
  => Env
  -> Region r q
  -> Term q
  -> NormalizeSession (Grown r q Term)
collectApp env rg e = case e of
  App appf arg
    | (conVarPrim, _) <- collectArgs e
    , isCon conVarPrim || isPrim conVarPrim || isVar conVarPrim
    -> do
      te <- Lens.view scopedTypeEnv
      let sc = rgScope rg
      untranslatable <- isUntranslatable False sc arg
      let localVar = isLocalVar arg
          constantNoCR = isConstantNotClockReset te sc arg
      if not untranslatable && not (localVar || constantNoCR) && not (isSimBind conVarPrim)
        then mkTmBinderFor te sc (derivedName env "app_arg") arg $ \sc1 b ->
          let rg1 = tellBinds [Told (binderRaw b) (sink arg)] (growRegion1 sc1 b rg)
          in  pure (Grown rg1 (App (sink appf) (idBinderVar b)))
        else pure (Grown rg e)
  _ -> pure (Grown rg e)
 where
  isSimBind (Prim p) = C.primName p == "Clash.Explicit.SimIO.bindSimIO#"
  isSimBind _ = False

-- | Lift the bindings of a let-expression into the region, and let-bind its
-- body unless it doesn't need to be, see
-- 'Clash.Normalize.Transformations.ANF.collectANF'
collectLet
  :: (HasCallStack, Distinct q)
  => Region r q
  -> [Told q]
  -- ^ The bindings
  -> Term q
  -- ^ The body
  -> NormalizeSession (Grown r q Term)
collectLet rg binds body = do
  te <- Lens.view scopedTypeEnv
  let sc = rgScope rg
      isSimIO = isSimIOTy te sc (inferCoreTypeOf te sc body)
  untranslatable <- isUntranslatable False sc body
  -- See Note [ANF no let-bind]
  if isLocalVar body || untranslatable || isSimIO
    then pure (Grown (tellBinds binds rg) body)
    else mkTmBinderFor te sc (systemName "result") body $ \sc1 b ->
      let rg1 = tellBinds [Told (binderRaw b) (sink body)] (growRegion1 sc1 b rg)
      in  pure (Grown (tellBinds (sink1 binds) rg1) (idBinderVar b))

-- | Let-bind the subject and the right-hand sides of a case-expression, and
-- let-bind the variables of its patterns to projections of the subject, see
-- 'Clash.Normalize.Transformations.ANF.collectANF'
collectCase
  :: forall r q
   . (HasCallStack, Distinct q)
  => Env
  -> Region r q
  -> Term q
  -- ^ Subject
  -> Type q
  -> [AltRes q]
  -> NormalizeSession (Grown r q Term)
collectCase env rg subj ty alts = do
  te <- Lens.view scopedTypeEnv
  let sc = rgScope rg
      isSimIOAlt = isSimIOTy te sc ty
  if isLocalVar subj || isConstant subj
    then withSubj te isSimIOAlt rg subj Nothing
    else mkTmBinderFor te sc (derivedName env "case_scrut") subj $ \sc1 b ->
      withSubj te isSimIOAlt (growRegion1 sc1 b rg) (idBinderVar b)
        (Just (Told (binderRaw b) (sink subj)))
 where
  withSubj
    :: DExt q q1
    => TypeEnv -> Bool -> Region r q1 -> Term q1 -> Maybe (Told q1)
    -> NormalizeSession (Grown r q Term)
  withSubj te isSimIOAlt rg1 subj1 subjBind = do
    Grown rg2 (Compose alts2) <-
      traverseGrown (\rg' alt -> doAlt te isSimIOAlt rg' (sink subj1) (sink alt)) rg1 alts
    let rg3 = tellBinds (sink1 (maybe [] pure subjBind)) rg2
    case alts2 of
      [APat _ (SomeTyTele TNil) vs altE]
        | isSimIOAlt || not (any (`memberNameSet` freeTmVars altE) [nm | Var _ _ nm <- vs])
        -> pure (Grown rg3 altE)
      _ -> pure (Grown rg3 (Case (sink subj1) (sink ty) (map (mkAlt (rgScope rg3)) alts2)))

  doAlt
    :: forall q1
     . Distinct q1
    => TypeEnv -> Bool -> Region r q1 -> Term q1 -> AltRes q1
    -> NormalizeSession (Grown r q1 AltRes)
  doAlt te isSimIOAlt rg1 subj1 alt = case alt of
    ARegion _ -> pure (Grown rg1 alt)
    APat dc tvs vs altE -> do
      let usesXs = case altE of
            Var _ _ nm -> tmNameRaw nm `elem` [tmNameRaw x | Var _ _ x <- vs]
            _ -> False
          patSels = zipWith (doPatBndr dc vs) vs [0..]
      -- See Note [ANF no let-bind]
      if isSimIOAlt || (isLocalVar altE && (not usesXs || length alts == 1)) || isConstant altE
        then pure (Grown (tellBinds patSels rg1) alt)
        else letBindAlt (APat dc (sink tvs) (sink1 vs)) altE patSels
    ALit l altE -> other (ALit l) altE
    ADefault altE -> other ADefault altE
   where
    sc1 = rgScope rg1

    other
      :: (forall q2. DExt q1 q2 => Term q2 -> AltRes q2)
      -> Term q1 -> NormalizeSession (Grown r q1 AltRes)
    other mk altE
      | isSimIOAlt || isLocalVar altE || isConstant altE = pure (Grown rg1 alt)
      | otherwise = letBindAlt mk altE []

    letBindAlt
      :: (forall q2. DExt q1 q2 => Term q2 -> AltRes q2)
      -> Term q1 -> [Told q1] -> NormalizeSession (Grown r q1 AltRes)
    letBindAlt mk altE patSels =
      mkTmBinderFor te sc1 (derivedName env "case_alt") altE $ \sc2 b ->
        let binds = sink1 patSels ++ [Told (binderRaw b) (sink altE)]
        in  pure (Grown (tellBinds binds (growRegion1 sc2 b rg1)) (mk (idBinderVar b)))

    doPatBndr dc vs v i = case v of
      Var _ _ nm -> Told (tmNameRaw nm) $
        if C.nameOcc (dcName dc) == Text.showt '(:-)
          then mkSignalConsSelector sc1 subj1 dc [vTy | Var _ vTy _ <- vs] i
          else mkSelectorCase ($(curLoc) ++ "doPatBndr") sc1 te subj1 (dcTag dc) i
      _ -> error ($(curLoc) ++ "doPatBndr: not a variable")

-- | Project the field with the given index out of a 'Signal' constructed with
-- @(:-)@, given the types of its fields, see
-- 'Clash.Normalize.Transformations.ANF.mkSignalConsSelector'
mkSignalConsSelector
  :: Distinct n
  => Scope n
  -> Term n
  -- ^ Subject
  -> DataCon
  -- ^ The @(:-)@ constructor
  -> [Type n]
  -- ^ The types of the fields
  -> Int
  -- ^ Index of the field to project
  -> Term n
mkSignalConsSelector sc subj dc tys i =
  withFreshIdBinders sc [(internalName "sel", ty) | ty <- tys] $ \_ fields vs ->
    case drop i (zip tys vs) of
      (ty, sel) : _ -> Case subj ty [Alt (DataPat dc TNil fields) sel]
      [] -> error ($(curLoc) ++ "mkSignalConsSelector: no field " ++ show i)

-- * nonRepANF

-- | Bring an application of a DataCon or Primitive in ANF, when the argument is
-- is considered non-representable
nonRepANF :: Transformation
nonRepANF = toScopedTransformation "nonRepANF" (onApp 'nonRepANFWorker)

-- | The 'App' handler of 'nonRepANF'. Clash Core deshadows the binders of the
-- let-expression it moves the application under (#1071); here, opening them
-- does that.
nonRepANFWorker
  :: (HasCallStack, Distinct n)
  => TransformContext n
  -> Term n
  -> Term n
  -> Term n
  -> NormalizeSession (Term n)
nonRepANFWorker ctx e appConPrim arg
  | (conPrim, _) <- collectArgs e
  , isCon conPrim || isPrim conPrim
  = do
    untranslatable <- isUntranslatable False (tfScope ctx) arg
    case (untranslatable, stripTicks arg) of
      (True, Let bind body) ->
        openBind (tfScope ctx) bind body $ \_ bind1 body1 ->
          changed (Let bind1 (App (sink appConPrim) body1))
      (True, Case {}) -> specialize ctx e
      (True, Lam {}) -> specialize ctx e
      (True, TyLam {}) -> specialize ctx e
      _ -> return e
nonRepANFWorker _ e _ _ = return e
{-# SCC nonRepANFWorker #-}
