{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Conversion between Clash Core ("Clash.Core.Term") and scoped Clash Core
  ("Clash.Core.Scoped.Syntax").

  Converting /to/ scoped Core numbers the binders in depth-first order, from
  just above the watermark of the scope (see 'Scope'). That deshadows the term,
  like 'Clash.Core.Subst.deShadowTerm', and makes names increase along every
  path from the root, which makes opening binders cheap. The free variables of
  a conversion environment keep their uniques. All occurrences of a variable
  share the converted variable of their binder, so their types are converted
  only once.

  Converting /from/ scoped Core uses the raw names as uniques.

  = Coercions

  The types of primitives are closed in scoped Core (see
  "Clash.Core.Scoped.Syntax"). Clash Core's 'Clash.Core.Util.primCo' however
  makes coercion primitives (@_CO_@) whose type is that of the coercion, which
  may mention type variables in scope. Conversion turns such a coercion with
  an open type @ty@ into @'TyApp' ('Prim' _CO_) ty@, where @_CO_@ has type
  @forall a. a@: the shape in which "Clash.GHC.GHC2Core" makes coercions,
  and 'Clash.Core.Scoped.Util.primCo' too. Converting back keeps that shape,
  which Clash Core handles as it does the coercions of "Clash.GHC.GHC2Core".
  So the round trip of a term with such a coercion is not the same term.

  = Trusted code

  This module is part of the trusted core of "Clash.Core.Scoped". Its unsafe
  operations, the invariants they rely on, and why these hold:

  * __Free variables of an environment__ ('unsafeAssertFresh' in
    @withBinderName@): a free variable keeps its unique if no other variable in
    the scope of the environment has it as its name. That is checked against
    the set of the names in the scope (@_convNames@). Every t'ConvEnv' outside
    of this module records all names in its scope there: 'emptyConvEnv' has
    none, 'extendConvEnv' and 'fromClashTyVars' insert every name they bind.
    The environments that conversion builds under binders don't update it, as
    they are never extended with free variables, nor passed outside of this
    module. Neither are the environments that 'fromClashTypeClosed',
    'fromClashTermClosedIn' and 'fromClashTermWith' build for a scope: they
    don't record its names, and are only used to convert binders, which get
    names above the watermark of the scope.

  * __Binders__ ('UnsafeTyNameBinder', 'UnsafeTmNameBinder'): only applied to
    binders that 'withRefreshed' or 'withFresh' just created, or that the
    previous point justifies.

  * __Candidate names__ ('UnsafeName' in @withBinderName@): the next name of
    the supply is passed to 'withRefreshed' as a candidate, which only uses
    its raw name to decide whether it is fresh.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Clash.Core.Scoped.Convert
  ( -- * Conversion environments
    ConvEnv
  , convScope
  , emptyConvEnv
  , withConvEnv
  , extendConvEnv
    -- * From Clash
  , fromClashType
  , fromClashTypeClosed
  , fromClashTerm
  , fromClashTermClosed
  , fromClashTermClosedIn
  , fromClashTermWith
  , fromClashTyVars
    -- * To Clash
  , toClashType
  , toClashTerm
  , toClashTyBinder
  , toClashIdBinder
    -- * Constructors
    -- $constructors
  , mkGlobalId
  , mkGlobalIdFromClash
  , mkPrimInfo
  , mkPrimInfoFromClash
  , mkDataCon
  , mkDataConFromClash
  , mkAlgTyCon
  , mkAlgTyConFromClash
  , mkPromotedDataCon
  , mkPromotedDataConFromClash
  , mkFunTyCon
  , mkFunTyConFromClash
  , mkPrimTyCon
  , mkPrimTyConFromClash
  , mkNewTyConRhs
  , mkNewTyConRhsFromClash
  ) where

import qualified Control.Lens as Lens
import qualified Data.List as List
import Control.Monad.ST (ST, runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Primitive.ByteArray
  (MutableByteArray, newByteArray, readByteArray, writeByteArray)
import Data.Text (Text)
import GHC.Stack (HasCallStack)

import Clash.Core.DataCon
  (ConTag, DataCon (..), DcName, DcStrictness, LegacyDataCon (..))
import qualified Clash.Core.Name as C
import Clash.Core.TyCon (AlgTyConRhs (..), TyCon (..), TyConName)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Util as C (undefinedTy)
import qualified Clash.Core.Var as C
import qualified Clash.Core.FreeVars as C
import Clash.Unique (Unique)

import Clash.Core.Scoped.Internal.Names
import Clash.Core.Scoped.Internal.Scope
import Clash.Core.Scoped.Sig
import Clash.Core.Scoped.Syntax

-- * Conversion environments

-- | Environment for converting Clash Core into scope @n@. Maps the unique of
-- every Clash variable in scope to its scoped variable.
data ConvEnv (n :: S) = ConvEnv
  { convScope :: !(Scope n)
  -- ^ The scope that the environment converts into
  , _convTys :: !(IntMap (Type n))
  -- ^ Type variables in scope, by Clash unique
  , _convTms :: !(IntMap (Term n))
  -- ^ Term variables in scope, by Clash unique
  , _convNames :: !IntSet
  -- ^ The raw names of the scope, see the module header
  }
type role ConvEnv nominal

-- | The environment of the empty scope, to convert closed types and terms
emptyConvEnv :: ConvEnv 'VoidS
emptyConvEnv = ConvEnv emptyScope IntMap.empty IntMap.empty IntSet.empty

-- | An environment with the given free variables, which keep their uniques
-- unless they clash with one another, e.g. a type variable and a term
-- variable with the same unique.
withConvEnv
  :: [C.TyVar]
  -> [C.Id]
  -> (forall n. Distinct n => ConvEnv n -> r)
  -> r
withConvEnv tvs ids cont = extendConvEnv emptyConvEnv tvs ids cont

-- | Extend an environment with more free variables, which keep their uniques
-- unless they clash with the names in scope.
--
-- A value converted in the original environment can be 'sink'ed into the
-- extended one. Note that its binders may then clash with the new variables:
-- functions that go under them rename them.
extendConvEnv
  :: forall n r
   . Distinct n
  => ConvEnv n
  -> [C.TyVar]
  -> [C.Id]
  -> (forall l. DExt n l => ConvEnv l -> r)
  -> r
extendConvEnv env0 tvs ids cont =
  runST (newSupply (convScope env0) >>= \sup -> goTvs sup env0 tvs)
 where
  goTvs :: DExt n x => Supply s -> ConvEnv x -> [C.TyVar] -> ST s r
  goTvs sup env [] = goIds sup env ids
  goTvs sup env (tv:rest) =
    convTyBinder sup FreeVar env tv $ \env' _ -> goTvs sup env' rest

  goIds :: DExt n x => Supply s -> ConvEnv x -> [C.Id] -> ST s r
  goIds _ env [] = pure (cont env)
  goIds sup env (i:rest) =
    convIdBinder sup FreeVar env i $ \env' _ -> goIds sup env' rest

-- * Naming
--
-- Conversion numbers the binders of a term in depth-first order, from just
-- above the watermark of the scope. So names increase along every path (which
-- is what makes opening them cheap, see 'Scope'), and moreover, the binders of
-- a subterm are above those of the subterms to its left: e.g. those of the
-- argument of an application are above those of the function.

-- | A supply of names: the next name to hand out
newtype Supply s = Supply (MutableByteArray s)

newSupply :: Scope n -> ST s (Supply s)
newSupply sc = do
  a <- newByteArray 8
  writeByteArray a 0 (scopeWatermark sc + 1)
  pure (Supply a)

nextName :: Supply s -> ST s Int
nextName (Supply a) = do
  k <- readByteArray a 0
  writeByteArray a 0 (k + 1 :: Int)
  pure k
{-# INLINE nextName #-}

-- | What a converted binder is
data BinderSort
  = Binder
  -- ^ A binder of the term: gets the next name of the supply
  | FreeVar
  -- ^ A free variable of the conversion environment: keeps its unique

-- | Pick the name of a converted binder, given its Clash unique, and extend
-- the scope with it.
--
-- The name of a binder of the term is the next name of the supply, which is
-- only a candidate: 'withRefreshed' decides whether it is fresh. A free
-- variable keeps its unique, unless that is already a name in scope.
withBinderName
  :: Distinct n
  => Supply s
  -> BinderSort
  -> ConvEnv n
  -> Int
  -> (forall l. DExt n l => NameBinder n l -> Scope l -> IntSet -> ST s r)
  -> ST s r
withBinderName sup sort (ConvEnv sc _ _ names) u cont = case sort of
  Binder -> do
    cand <- nextName sup
    withRefreshed sc (UnsafeName cand) $ \b -> cont b (extendScope b sc) names
  FreeVar
    | IntSet.member u names ->
        withFresh sc $ \b ->
          cont b (extendScope b sc) (IntSet.insert (nameId (nameOf b)) names)
    | otherwise ->
        -- Trusted: u is not a name in scope, see the module header
        unsafeAssertFresh (UnsafeNameBinder (UnsafeName u)) $ \b ->
          cont b (extendScope b sc) (IntSet.insert u names)
{-# INLINE withBinderName #-}

convTyBinder
  :: Distinct n
  => Supply s
  -> BinderSort
  -> ConvEnv n
  -> C.TyVar
  -> (forall l. DExt n l => ConvEnv l -> TyBinder n l -> ST s r)
  -> ST s r
convTyBinder sup sort env@(ConvEnv _ tys tms _) tv cont = do
  k <- goTyS sup env (C.varType tv)
  let u = uniqToRaw (C.varUniq tv)
  withBinderName sup sort env u $ \b sc' names' ->
    -- Trusted: b is fresh, see the module header
    let bndr = TyBinder (C.varName tv) k (UnsafeTyNameBinder b)
        occ = VarTy (C.varName tv) (sink k) (tyNameOf bndr)
    in cont (ConvEnv sc' (IntMap.insert u occ (sink1 tys)) (sink1 tms) names') bndr

convIdBinder
  :: Distinct n
  => Supply s
  -> BinderSort
  -> ConvEnv n
  -> C.Id
  -> (forall l. DExt n l => ConvEnv l -> IdBinder n l -> ST s r)
  -> ST s r
convIdBinder sup sort env@(ConvEnv _ tys tms _) i cont = do
  ty <- goTyS sup env (C.varType i)
  let u = uniqToRaw (C.varUniq i)
  withBinderName sup sort env u $ \b sc' names' ->
    -- Trusted: b is fresh, see the module header
    let bndr = IdBinder (C.varName i) ty (UnsafeTmNameBinder b)
        occ = Var (C.varName i) (sink ty) (tmNameOf bndr)
    in cont (ConvEnv sc' (sink1 tys) (IntMap.insert u occ (sink1 tms)) names') bndr

convTele
  :: forall y v n s r
   . Distinct n
  => (forall x r'. Distinct x => ConvEnv x -> v -> (forall x'. DExt x x' => ConvEnv x' -> y x x' -> ST s r') -> ST s r')
  -> ConvEnv n
  -> [v]
  -> (forall l. DExt n l => ConvEnv l -> Tele y n l -> ST s r)
  -> ST s r
convTele _ env [] cont = cont env TNil
convTele convB env (v:vs) cont =
  convB env v $ \env1 b ->
    convTele convB env1 vs $ \env2 bs ->
      cont env2 (TCons b bs)

-- | Clash uniques are 'Unique's, raw names are 'Int's
uniqToRaw :: Unique -> Int
uniqToRaw = fromIntegral
{-# INLINE uniqToRaw #-}

rawToUniq :: Int -> Unique
rawToUniq = fromIntegral
{-# INLINE rawToUniq #-}

-- * From Clash

-- | Convert a Clash type. Errors on type variables that are not in scope.
fromClashType :: (HasCallStack, Distinct n) => ConvEnv n -> C.Type -> Type n
fromClashType env ty = runST (newSupply (convScope env) >>= \sup -> goTyS sup env ty)

-- | Convert a closed Clash type, such as the type of a global or of a data
-- constructor, into any scope
fromClashTypeClosed :: (HasCallStack, Distinct n) => Scope n -> C.Type -> Type n
fromClashTypeClosed sc =
  fromClashType (ConvEnv sc IntMap.empty IntMap.empty IntSet.empty)

goTyS
  :: (HasCallStack, Distinct n) => Supply s -> ConvEnv n -> C.Type -> ST s (Type n)
goTyS sup env@(ConvEnv _ tys _ _) = go
 where
  go = \case
    C.VarTy tv -> case IntMap.lookup (uniqToRaw (C.varUniq tv)) tys of
      Just ty -> pure ty
      Nothing -> error ("fromClashType: type variable not in scope: " ++ show (C.varName tv))
    C.ConstTy c -> pure (ConstTy c)
    C.ForAllTy tv t -> convTyBinder sup Binder env tv $ \env' b -> ForAllTy b <$> goTyS sup env' t
    C.AppTy t1 t2 -> AppTy <$> go t1 <*> go t2
    C.LitTy l -> pure (LitTy l)
    C.AnnType a t -> AnnType a <$> go t

-- | Convert a Clash term. Errors on local variables that are not in scope.
fromClashTerm :: (HasCallStack, Distinct n) => ConvEnv n -> C.Term -> Term n
fromClashTerm env tm = runST (newSupply (convScope env) >>= \sup -> goTmS sup env tm)

-- | Convert a closed Clash term, such as the body of a global binding
fromClashTermClosed :: HasCallStack => C.Term -> Term 'VoidS
fromClashTermClosed = fromClashTerm emptyConvEnv

-- | Convert a closed Clash term, such as the body of a global binding, into
-- any scope. Unlike @'sinkClosed' . 'fromClashTermClosed'@, this numbers the
-- binders from just above the watermark of the scope, so that opening them
-- doesn't rename them.
fromClashTermClosedIn :: (HasCallStack, Distinct n) => Scope n -> C.Term -> Term n
fromClashTermClosedIn sc =
  fromClashTerm (ConvEnv sc IntMap.empty IntMap.empty IntSet.empty)

-- | Convert a Clash term into a scope, given the scoped counterparts of its
-- free variables, by their Clash variables. Errors on other free variables.
-- Like 'fromClashTermClosedIn', this numbers the binders from just above the
-- watermark of the scope.
--
-- E.g. to convert back the result of a function of Clash Core on terms that
-- were converted from the scope.
fromClashTermWith
  :: (HasCallStack, Distinct n)
  => Scope n
  -> [(C.TyVar, Type n)]
  -- ^ The free type variables
  -> [(C.Id, Term n)]
  -- ^ The free term variables
  -> C.Term
  -> Term n
fromClashTermWith sc tvs ids =
  fromClashTerm (ConvEnv sc tys tms IntSet.empty)
 where
  tys = IntMap.fromList [(uniqToRaw (C.varUniq tv), ty) | (tv, ty) <- tvs]
  tms = IntMap.fromList [(uniqToRaw (C.varUniq i), tm) | (i, tm) <- ids]

goTmS
  :: (HasCallStack, Distinct n) => Supply s -> ConvEnv n -> C.Term -> ST s (Term n)
goTmS sup env@(ConvEnv _ _ tms _) = go
 where
  goT = goTyS sup env
  go = \case
    C.Var v
      | C.isGlobalId v -> pure (GlobalVar (mkGlobalIdFromClash v))
      | otherwise -> case IntMap.lookup (uniqToRaw (C.varUniq v)) tms of
          Just tm -> pure tm
          Nothing -> error ("fromClashTerm: variable not in scope: " ++ show (C.varName v))
    C.Data dc -> pure (Data dc)
    C.Literal l -> pure (Literal l)
    C.Prim p
      -- A coercion with an open type, see the module header
      | C.primName p == coercionPrimName
      , Lens.notNullOf C.typeFreeVars (C.primType p)
      -> TyApp (Prim (mkPrimInfoFromClash p {C.primType = C.undefinedTy})) <$> goT (C.primType p)
      | otherwise -> pure (Prim (mkPrimInfoFromClash p))
    C.Lam v e -> convIdBinder sup Binder env v $ \env' b -> Lam b <$> goTmS sup env' e
    C.TyLam tv e -> convTyBinder sup Binder env tv $ \env' b -> TyLam b <$> goTmS sup env' e
    C.App e1 e2 -> App <$> go e1 <*> go e2
    C.TyApp e ty -> TyApp <$> go e <*> goT ty
    C.Let (C.NonRec v x) e -> do
      x' <- go x
      convIdBinder sup Binder env v $ \env' b -> Let (NonRec b x') <$> goTmS sup env' e
    C.Let (C.Rec bs) e ->
      convTele (convIdBinder sup Binder) env (map fst bs) $ \env' bndrs -> do
        xs <- mapM (goTmS sup env' . snd) bs
        Let (Rec bndrs xs) <$> goTmS sup env' e
    C.Case e ty alts -> Case <$> go e <*> goT ty <*> mapM goAlt alts
    C.Cast e t1 t2 -> Cast <$> go e <*> goT t1 <*> goT t2
    C.Tick t e -> Tick <$> goTick t <*> go e

  goAlt (pat, e) = case pat of
    C.DataPat dc tvs ids ->
      convTele (convTyBinder sup Binder) env tvs $ \env1 tvs' ->
        convTele (convIdBinder sup Binder) env1 ids $ \env2 ids' ->
          Alt (DataPat dc tvs' ids') <$> goTmS sup env2 e
    C.LitPat l -> Alt (LitPat l) <$> go e
    C.DefaultPat -> Alt DefaultPat <$> go e

  goTick = \case
    C.SrcSpan s -> pure (SrcSpan s)
    C.NameMod m ty -> NameMod m <$> goT ty
    C.DeDup -> pure DeDup
    C.NoDeDup -> pure NoDeDup
    C.Attributes ty e -> Attributes <$> goT ty <*> go e

-- | The name of coercion primitives, see the module header
coercionPrimName :: Text
coercionPrimName = "_CO_"

-- | Convert Clash type variables into a telescope of binders, e.g. those of a
-- data constructor. Their kinds may mention the ones before them.
fromClashTyVars
  :: forall n r
   . (HasCallStack, Distinct n)
  => ConvEnv n
  -> [C.TyVar]
  -> (forall l. DExt n l => ConvEnv l -> Tele TyBinder n l -> r)
  -> r
fromClashTyVars env0 tvs cont =
  runST (newSupply (convScope env0) >>= \sup ->
    convTele (convTyBinder sup Binder) env0 tvs $ \env1 bs ->
      -- Record the binders' names, see '_convNames'
      pure (cont (addNames bs env1) bs))
 where
  addNames :: Tele TyBinder x y -> ConvEnv l -> ConvEnv l
  addNames bs (ConvEnv sc tys tms names) =
    ConvEnv sc tys tms (teleNames bs names)

  teleNames :: Tele TyBinder x y -> IntSet -> IntSet
  teleNames TNil !acc = acc
  teleNames (TCons b bs) !acc = teleNames bs (IntSet.insert (tyNameRaw (tyNameOf b)) acc)

-- * To Clash

setUniq :: C.Name a -> Int -> C.Name a
setUniq nm u
  | C.nameUniq nm == rawToUniq u = nm
  | otherwise = nm {C.nameUniq = rawToUniq u}
{-# INLINE setUniq #-}

-- | Environment for converting to Clash: the converted binders, by raw name,
-- so that all occurrences share them.
data ToEnv = ToEnv !(IntMap C.TyVar) !(IntMap C.Id)

-- | Convert a type to Clash
toClashType :: Type n -> C.Type
toClashType = toClashTypeEnv (ToEnv IntMap.empty IntMap.empty)

toClashTypeEnv :: ToEnv -> Type n -> C.Type
toClashTypeEnv env@(ToEnv tvs _) = go
 where
  go :: Type x -> C.Type
  go = \case
    VarTy occ k n -> case IntMap.lookup (tyNameRaw n) tvs of
      Just tv -> C.VarTy tv
      Nothing ->
        let u = tyNameRaw n
        in C.VarTy (C.TyVar (setUniq occ u) (rawToUniq u) (go k))
    ConstTy c -> C.ConstTy c
    ForAllTy b t -> toTyBinder env b $ \env' tv -> C.ForAllTy tv (toClashTypeEnv env' t)
    AppTy t1 t2 -> C.AppTy (go t1) (go t2)
    LitTy l -> C.LitTy l
    AnnType a t -> C.AnnType a (go t)

toTyBinder :: ToEnv -> TyBinder n l -> (ToEnv -> C.TyVar -> r) -> r
toTyBinder env@(ToEnv tvs ids) bndr@(TyBinder occ k _) cont =
  let u = tyNameRaw (tyNameOf bndr)
      tv = C.TyVar (setUniq occ u) (rawToUniq u) (toClashTypeEnv env k)
  in cont (ToEnv (IntMap.insert u tv tvs) ids) tv

toIdBinder :: ToEnv -> IdBinder n l -> (ToEnv -> C.Id -> r) -> r
toIdBinder env@(ToEnv tvs ids) bndr@(IdBinder occ ty _) cont =
  let u = tmNameRaw (tmNameOf bndr)
      v = C.Id (setUniq occ u) (rawToUniq u) (toClashTypeEnv env ty) C.LocalId
  in cont (ToEnv tvs (IntMap.insert u v ids)) v

-- | Convert a type variable binder to Clash
toClashTyBinder :: TyBinder n l -> C.TyVar
toClashTyBinder b = toTyBinder (ToEnv IntMap.empty IntMap.empty) b (\_ tv -> tv)

-- | Convert a term variable binder to Clash
toClashIdBinder :: IdBinder n l -> C.Id
toClashIdBinder b = toIdBinder (ToEnv IntMap.empty IntMap.empty) b (\_ v -> v)

toTele
  :: (forall x y r'. ToEnv -> b x y -> (ToEnv -> v -> r') -> r')
  -> ToEnv -> Tele b n l -> (ToEnv -> [v] -> r) -> r
toTele _ env TNil cont = cont env []
toTele toB env (TCons b bs) cont =
  toB env b $ \env1 v -> toTele toB env1 bs $ \env2 vs -> cont env2 (v:vs)

-- | Convert a term to Clash. Errors on a recursive let-binding with more or
-- fewer right-hand sides than binders.
toClashTerm :: HasCallStack => Term n -> C.Term
toClashTerm = toClashTermEnv (ToEnv IntMap.empty IntMap.empty)

toClashTermEnv :: HasCallStack => ToEnv -> Term n -> C.Term
toClashTermEnv env@(ToEnv _ ids) = go
 where
  goTy :: Type x -> C.Type
  goTy = toClashTypeEnv env

  go :: Term x -> C.Term
  go = \case
    Var occ ty n -> case IntMap.lookup (tmNameRaw n) ids of
      Just v -> C.Var v
      Nothing ->
        let u = tmNameRaw n
        in C.Var (C.Id (setUniq occ u) (rawToUniq u) (goTy ty) C.LocalId)
    GlobalVar v -> C.Var (globalIdLegacy v)
    Data dc -> C.Data dc
    Literal l -> C.Literal l
    Prim p -> C.Prim (primLegacy p)
    Lam b e -> toIdBinder env b $ \env' v -> C.Lam v (toClashTermEnv env' e)
    TyLam b e -> toTyBinder env b $ \env' tv -> C.TyLam tv (toClashTermEnv env' e)
    App e1 e2 -> C.App (go e1) (go e2)
    TyApp e ty -> C.TyApp (go e) (goTy ty)
    Let (NonRec b x) e ->
      toIdBinder env b $ \env' v -> C.Let (C.NonRec v (go x)) (toClashTermEnv env' e)
    Let (Rec bs xs) e
      | teleLength bs /= length xs ->
          error ("toClashTerm: " <> show (teleLength bs) <> " binders, but "
            <> show (length xs) <> " right-hand sides")
      | otherwise ->
          toTele toIdBinder env bs $ \env' vs ->
            C.Let (C.Rec (zip vs (map (toClashTermEnv env') xs))) (toClashTermEnv env' e)
    Case e ty alts -> C.Case (go e) (goTy ty) (map goAlt alts)
    Cast e t1 t2 -> C.Cast (go e) (goTy t1) (goTy t2)
    Tick t e -> C.Tick (goTick t) (go e)

  goAlt :: Alt x -> C.Alt
  goAlt (Alt p e) = case p of
    DataPat dc tvs ids' ->
      toTele toTyBinder env tvs $ \env1 tvs' ->
        toTele toIdBinder env1 ids' $ \env2 ids'' ->
          (C.DataPat dc tvs' ids'', toClashTermEnv env2 e)
    LitPat l -> (C.LitPat l, go e)
    DefaultPat -> (C.DefaultPat, go e)

  goTick :: TickInfo x -> C.TickInfo
  goTick = \case
    SrcSpan s -> C.SrcSpan s
    NameMod m ty -> C.NameMod m (goTy ty)
    DeDup -> C.DeDup
    NoDeDup -> C.NoDeDup
    Attributes ty e -> C.Attributes (goTy ty) (go e)

-- * Constructors

{- $constructors
The types of scoped Core that Clash Core shares, such as 'GlobalId', hold
both their scoped parts, which are the truth, and the parts that Clash Core
needs. The constructors below take one side and derive the other one lazily,
by conversion. They are transitional: once Clash Core is gone, so are the
legacy parts.
-}

-- | A global variable with the given name and closed type
mkGlobalId :: C.Name C.Term -> Type 'VoidS -> GlobalId
mkGlobalId nm ty = GlobalId
  { globalIdName = nm
  , globalIdType = ty
  , globalIdLegacy = C.Id nm (C.nameUniq nm) (toClashType ty) C.GlobalId
  }

-- | A global variable of Clash Core. Errors on a local 'C.Id', or on a type
-- variable. Its type is converted lazily, and errors if it isn't closed.
mkGlobalIdFromClash :: HasCallStack => C.Id -> GlobalId
mkGlobalIdFromClash v
  | C.isGlobalId v = globalIdFromClashUnchecked v
  | otherwise =
      error ("mkGlobalIdFromClash: not a global variable: " <> show (C.varName v))

globalIdFromClashUnchecked :: HasCallStack => C.Id -> GlobalId
globalIdFromClashUnchecked v = GlobalId
  { globalIdName = (C.varName v) {C.nameUniq = C.varUniq v}
  , globalIdType = fromClashTypeClosed emptyScope (C.varType v)
  , globalIdLegacy = v
  }

-- | A primitive with the given name, closed type, work info, whether it has
-- multiple results, and unfolding
mkPrimInfo :: Text -> Type 'VoidS -> C.WorkInfo -> C.IsMultiPrim -> PrimUnfolding -> PrimInfo
mkPrimInfo nm ty wi mr unf = PrimInfo
  { primName = nm
  , primType = ty
  , primWorkInfo = wi
  , primMultiResult = mr
  , primUnfolding = unf
  , primLegacy = C.PrimInfo nm (toClashType ty) wi mr unfLegacy
  }
 where
  unfLegacy = case unf of
    NoUnfolding -> C.NoUnfolding
    Unfolding g -> C.Unfolding (globalIdLegacy g)

-- | A primitive of Clash Core. Its type is converted lazily, and errors if it
-- isn't closed. See the module header for the coercions of Clash Core, whose
-- types need not be closed.
mkPrimInfoFromClash :: HasCallStack => C.PrimInfo -> PrimInfo
mkPrimInfoFromClash p = PrimInfo
  { primName = C.primName p
  , primType = fromClashTypeClosed emptyScope (C.primType p)
  , primWorkInfo = C.primWorkInfo p
  , primMultiResult = C.primMultiResult p
  , primUnfolding = case C.primUnfolding p of
      C.NoUnfolding -> NoUnfolding
      -- Not checked with 'C.isGlobalId': the binder of the unfolding is a
      -- top-level binding, but GHC2Core gives it the scope of GHC's
      -- variable, which need not be global
      C.Unfolding v -> Unfolding (globalIdFromClashUnchecked v)
  , primLegacy = p
  }

-- | A data constructor with the given name, tag, closed type, signature (see
-- "Clash.Core.Scoped.Sig"), strictness of its fields, and labels of its
-- fields. The types of Clash Core use the raw names of the binders as
-- uniques.
mkDataCon
  :: DcName -> ConTag -> Type 'VoidS -> DataConSig -> [DcStrictness] -> [Text]
  -> DataCon
mkDataCon nm tag ty sig strict labels = MkData
  { dcName = nm
  , dcUniq = C.nameUniq nm
  , dcTag = tag
  , dcType = ty
  , dcSig = sig
  , dcArgStrict = strict
  , dcFieldLabels = labels
  , dcLegacy = legacy
  }
 where
  legacy = case sig of
    DataConSig univ ext args ->
      toTele toTyBinder (ToEnv IntMap.empty IntMap.empty) univ $ \env1 univ' ->
        toTele toTyBinder env1 ext $ \env2 ext' ->
          LegacyDataCon (toClashType ty) univ' ext' (map (toClashTypeEnv env2) args)

-- | A data constructor of Clash Core: its name, unique, tag, type,
-- universally and existentially quantified type variables, the types and
-- strictness of its fields, and the labels of its fields. Its scoped type and
-- signature are converted lazily, and error if the type isn't closed, or the
-- types of the fields mention other type variables than the quantified ones.
mkDataConFromClash
  :: HasCallStack
  => DcName -> Unique -> ConTag -> C.Type -> [C.TyVar] -> [C.TyVar] -> [C.Type]
  -> [DcStrictness] -> [Text] -> DataCon
mkDataConFromClash nm u tag ty univ ext args strict labels = MkData
  { dcName = nm
  , dcUniq = u
  , dcTag = tag
  , dcType = fromClashTypeClosed emptyScope ty
  , dcSig =
      fromClashTyVars emptyConvEnv univ $ \env1 univ' ->
        fromClashTyVars env1 ext $ \env2 ext' ->
          DataConSig univ' ext' (map (fromClashType env2) args)
  , dcArgStrict = strict
  , dcFieldLabels = labels
  , dcLegacy = LegacyDataCon ty univ ext args
  }

-- | An algebraic data type: name, closed kind, arity, data constructors, and
-- whether it is a class dictionary
mkAlgTyCon :: TyConName -> Kind 'VoidS -> Int -> AlgTyConRhs -> Bool -> TyCon
mkAlgTyCon nm k ar rhs cls = AlgTyCon (C.nameUniq nm) nm k ar rhs cls (toClashType k)

-- | An algebraic data type of Clash Core: unique, name, kind, arity, data
-- constructors, and whether it is a class dictionary. Its kind is converted
-- lazily, and errors if it isn't closed.
mkAlgTyConFromClash
  :: HasCallStack => Unique -> TyConName -> C.Kind -> Int -> AlgTyConRhs -> Bool -> TyCon
mkAlgTyConFromClash u nm k ar rhs cls =
  AlgTyCon u nm (fromClashTypeClosed emptyScope k) ar rhs cls k

-- | A promoted data constructor: name, closed kind, arity, and the data
-- constructor
mkPromotedDataCon :: TyConName -> Kind 'VoidS -> Int -> DataCon -> TyCon
mkPromotedDataCon nm k ar dc = PromotedDataCon (C.nameUniq nm) nm k ar dc (toClashType k)

-- | A promoted data constructor of Clash Core, see 'mkAlgTyConFromClash'
mkPromotedDataConFromClash
  :: HasCallStack => Unique -> TyConName -> C.Kind -> Int -> DataCon -> TyCon
mkPromotedDataConFromClash u nm k ar dc =
  PromotedDataCon u nm (fromClashTypeClosed emptyScope k) ar dc k

-- | A type family: name, closed kind, arity, and equations. The types of
-- Clash Core use the raw names of the pattern variables as uniques.
mkFunTyCon :: TyConName -> Kind 'VoidS -> Int -> [TyFamEqn] -> TyCon
mkFunTyCon nm k ar eqns =
  FunTyCon (C.nameUniq nm) nm k ar eqns (toClashType k) (map toEqn eqns)
 where
  toEqn (TyFamEqn tvs lhs rhs) =
    toTele toTyBinder (ToEnv IntMap.empty IntMap.empty) tvs $ \env _ ->
      (map (toClashTypeEnv env) lhs, toClashTypeEnv env rhs)

-- | A type family of Clash Core: unique, name, kind, arity, and equations, as
-- a list of the types of the left-hand side and the right-hand side. Its kind
-- and equations are converted lazily. Clash Core stores the equations without
-- their pattern variables, so these are the free type variables of the
-- equation, ordered so that every variable comes after the variables in its
-- kind. Errors if the kind isn't closed.
mkFunTyConFromClash
  :: HasCallStack => Unique -> TyConName -> C.Kind -> Int -> [([C.Type], C.Type)] -> TyCon
mkFunTyConFromClash u nm k ar eqns =
  FunTyCon u nm (fromClashTypeClosed emptyScope k) ar (map tyFamEqn eqns) k eqns
 where
  tyFamEqn (lhs, rhs) =
    fromClashTyVars emptyConvEnv (eqnTyVars lhs rhs) $ \env tele ->
      TyFamEqn tele (map (fromClashType env) lhs) (fromClashType env rhs)

  -- The free type variables, closed over kinds
  eqnTyVars lhs rhs =
    C.dependencySortTyVars
      (List.nubBy sameUniq (concatMap (Lens.toListOf C.typeFreeVars) (lhs ++ [rhs])))

  sameUniq :: C.TyVar -> C.TyVar -> Bool
  sameUniq v1 v2 = C.varUniq v1 == C.varUniq v2

-- | A primitive type constructor: name, closed kind, and arity
mkPrimTyCon :: TyConName -> Kind 'VoidS -> Int -> TyCon
mkPrimTyCon nm k ar = PrimTyCon (C.nameUniq nm) nm k ar (toClashType k)

-- | A primitive type constructor of Clash Core, see 'mkAlgTyConFromClash'
mkPrimTyConFromClash :: HasCallStack => Unique -> TyConName -> C.Kind -> Int -> TyCon
mkPrimTyConFromClash u nm k ar = PrimTyCon u nm (fromClashTypeClosed emptyScope k) ar k

-- | The right-hand side of a newtype: its data constructor and signature. The
-- types of Clash Core use the raw names of the type variables as uniques.
mkNewTyConRhs :: DataCon -> NewTyConSig -> AlgTyConRhs
mkNewTyConRhs dc sig = NewTyCon dc sig legacy
 where
  legacy = case sig of
    NewTyConSig tvs rhs ->
      toTele toTyBinder (ToEnv IntMap.empty IntMap.empty) tvs $ \env tvs' ->
        (tvs', toClashTypeEnv env rhs)

-- | The right-hand side of a newtype of Clash Core: its data constructor, and
-- the type variables of the newtype and its eta-reduced representation type.
-- Its signature is converted lazily, and errors if the representation type
-- mentions type variables other than those of the newtype.
mkNewTyConRhsFromClash :: HasCallStack => DataCon -> ([C.TyVar], C.Type) -> AlgTyConRhs
mkNewTyConRhsFromClash dc (tvs, rhs) = NewTyCon dc sig (tvs, rhs)
 where
  sig = fromClashTyVars emptyConvEnv tvs $ \env tele ->
    NewTyConSig tele (fromClashType env rhs)
