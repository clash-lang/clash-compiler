{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Builders of closed terms and types of scoped Clash Core
  ("Clash.Core.Scoped.Syntax"). They build scoped Core from a representation
  that refers to variables by name, such as GHC Core
  ("Clash.GHC.Scoped.GHC2Core"), and add nothing to the trusted core.

  = Two phases

  E.g. "Clash.GHC.Scoped.GHC2Core" converts GHC Core in two phases:

  1. In the 'Clash.GHC.GHC2Core.C2C' monad, it converts a GHC term into a
     builder ('TmB'), and a GHC type into a 'TyB'. This phase does all the
     work that needs GHC: names, data constructors, type families, primitives.
     The builders refer to variables by their 'Key's.

  2. 'runClosedTerm' builds the scoped term. It gives the binders fresh names
     in depth-first order, from a supply, like
     'Clash.Core.Scoped.Convert.fromClashTermClosed' numbers the binders of a
     term of Clash Core. So the native conversion and the conversion through
     Clash Core give the same raw names. That matters: some transformations,
     such as @inlineCleanup@, depend on the order of names.

  The phases are separate because phase 1 can't number the binders itself: it
  converts the body of a lambda before the lambda's binder, as the binder's
  source location comes from the body. Phase 2 also stays lazy per binding,
  like the conversion through Clash Core was: most bindings that GHC2Core
  produces are never looked at.

  Every builder is strict in its parts, so forcing a builder to weak head
  normal form forces all of it (see its 'NFData' instance). The builders hold
  no GHC values.

  = Types

  A 'TyB' of a closed type without binders holds the scoped type itself
  ('TyClosed'), which every use shares. Other types are built per use, as the
  names of their binders depend on where they occur. Types are built natively
  ('varTyB', 'forAllTyB', ...), or from types of Clash Core ('clashTyB').

  = Variables

  A 'Key' is any 'Int' that identifies a variable, chosen by the user of the
  builders: GHC2Core uses the uniques of GHC's variables ('uniqueKey'), and
  'clashTyB' the uniques of Clash's type variables. The environment of phase 2
  maps the keys of the variables in scope to their occurrences, where an inner
  binder shadows an outer one with the same key, as in GHC Core. So the keys
  of different variables need only differ where one is in the scope of the
  other. The builders of "Clash.GHC.Scoped.GHC2Core" that don't come from GHC
  Core, such as those of the signal primitives, are closed: their bodies only
  refer to their own binders, so any keys work for them.

  Building errors on a variable whose key is not in scope.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clash.Core.Scoped.Builder
  ( -- * Keys
    Key
  , uniqueKey
    -- * Types
  , TyB
  , varTyB
  , constTyB
  , forAllTyB
  , appTyB
  , litTyB
  , annTyB
  , clashTyB
    -- * Terms
  , TmB
  , tmbHead
  , varB
  , globalB
  , dataB
  , literalB
  , primB
  , appB
  , tyAppB
  , lamB
  , tyLamB
  , tyLamClashB
  , letB
  , letrecB
  , caseB
  , castB
  , tickB
    -- * Alternatives
  , AltB
  , defaultAltB
  , litAltB
  , dataAltB
    -- * Ticks
  , TickB
  , srcSpanTickB
  , nameModTickB
  , deDupTickB
  , noDeDupTickB
  , attributesTickB
    -- * Building
  , runClosedTerm
  , runClosedType
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad.ST (ST, runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Primitive.ByteArray
  (MutableByteArray, newByteArray, readByteArray, writeByteArray)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import GHC.Types.SrcLoc (SrcSpan)

import Clash.Annotations.SynthesisAttributes (Attr)

import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal)
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import Clash.Unique (Unique)

import Clash.Core.Scoped.Subst (withFreshIdBinder, withFreshTyBinder)
import Clash.Core.Scoped.Syntax

-- * Keys

-- | The key of a variable, see the module header
type Key = Int

-- | The key of a variable with the given unique, such as a Clash variable.
-- Clash's uniques of the variables that GHC2Core converts are those of GHC.
uniqueKey :: Unique -> Key
uniqueKey = fromIntegral
{-# INLINE uniqueKey #-}

-- * Phase 2

-- | The supply of names: the next name to hand out
newtype Supply s = Supply (MutableByteArray s)

-- | The environment of phase 2, in scope @n@
data BEnv s n = BEnv
  { _beScope :: !(Scope n)
  , _beTys :: !(IntMap (Type n))
  -- ^ The type variables in scope, by key
  , _beTms :: !(IntMap (Term n))
  -- ^ The term variables in scope, by key
  , _beSupply :: !(Supply s)
  }

-- | A computation of phase 2
newtype Run f = Run (forall n s. Distinct n => BEnv s n -> ST s (f n))

run :: Distinct n => Run f -> BEnv s n -> ST s (f n)
run (Run f) = f
{-# INLINE run #-}

nextName :: Supply s -> ST s Int
nextName (Supply a) = do
  k <- readByteArray a 0
  writeByteArray a 0 (k + 1 :: Int)
  pure k
{-# INLINE nextName #-}

-- | Bind a type variable, with the next name of the supply. Like
-- 'Clash.Core.Scoped.Convert.fromClashTerm', the caller builds the kind
-- before calling this, so the binders in the kind come first.
withTyBinder
  :: Distinct n
  => BEnv s n
  -> Key
  -> C.Name C.Type
  -> Kind n
  -> (forall l. DExt n l => BEnv s l -> TyBinder n l -> ST s r)
  -> ST s r
withTyBinder (BEnv sc tys tms sup) key occ k cont = do
  nm <- nextName sup
  -- The names in scope are below the next name of the supply, so this binder
  -- gets that name.
  withFreshTyBinder (raiseWatermark (nm - 1) sc) occ k $ \sc' b ->
    cont (BEnv sc' (IntMap.insert key (tyBinderVar b) (sink1 tys)) (sink1 tms) sup) b
{-# INLINE withTyBinder #-}

-- | Bind a term variable, with the next name of the supply, see
-- 'withTyBinder'
withIdBinder
  :: Distinct n
  => BEnv s n
  -> Key
  -> C.Name C.Term
  -> Type n
  -> (forall l. DExt n l => BEnv s l -> IdBinder n l -> ST s r)
  -> ST s r
withIdBinder (BEnv sc tys tms sup) key occ ty cont = do
  nm <- nextName sup
  withFreshIdBinder (raiseWatermark (nm - 1) sc) occ ty $ \sc' b ->
    cont (BEnv sc' (sink1 tys) (IntMap.insert key (idBinderVar b) (sink1 tms)) sup) b
{-# INLINE withIdBinder #-}

-- | Build a closed term. Errors on variables that are not in scope.
runClosedTerm :: HasCallStack => TmB -> Term 'VoidS
runClosedTerm tm = runST $ do
  a <- newByteArray 8
  writeByteArray a 0 (1 :: Int)
  run (tmbRun tm) (BEnv emptyScope IntMap.empty IntMap.empty (Supply a))

-- | Build a closed type. Errors on type variables that are not in scope.
runClosedType :: HasCallStack => TyB -> Type 'VoidS
runClosedType (TyClosed t) = t
runClosedType (TyOpen r) = runST $ do
  a <- newByteArray 8
  writeByteArray a 0 (1 :: Int)
  run r (BEnv emptyScope IntMap.empty IntMap.empty (Supply a))

-- * Types

-- | A type under construction, see the module header
data TyB
  = TyClosed !(Type 'VoidS)
  -- ^ Closed, and without binders
  | TyOpen !(Run Type)

instance NFData TyB where
  rnf t = t `seq` ()

runTyB :: Distinct n => TyB -> BEnv s n -> ST s (Type n)
runTyB (TyClosed t) _ = pure (sinkClosed t)
runTyB (TyOpen r) env = run r env
{-# INLINE runTyB #-}

-- | A type variable occurrence, by key
varTyB :: HasCallStack => Key -> TyB
varTyB !key = TyOpen $ Run $ \(BEnv _ tys _ _) -> case IntMap.lookup key tys of
  Just ty -> pure ty
  Nothing -> error ("Clash.Core.Scoped.Builder: type variable not in scope: " ++ show key)

constTyB :: C.ConstTy -> TyB
constTyB !c = TyClosed (ConstTy c)

-- | A forall-type: key, name and kind of the type variable, and the body
forAllTyB :: Key -> C.Name C.Type -> TyB -> TyB -> TyB
forAllTyB !key !occ !k !t = TyOpen $ Run $ \env -> do
  k' <- runTyB k env
  withTyBinder env key occ k' $ \env' b -> ForAllTy b <$> runTyB t env'

appTyB :: TyB -> TyB -> TyB
appTyB (TyClosed t1) (TyClosed t2) = TyClosed (AppTy t1 t2)
appTyB !t1 !t2 = TyOpen $ Run $ \env -> AppTy <$> runTyB t1 env <*> runTyB t2 env

litTyB :: C.LitTy -> TyB
litTyB !l = TyClosed (LitTy l)

annTyB :: [Attr Text] -> TyB -> TyB
annTyB attrs t0 = rnf attrs `seq` case t0 of
  TyClosed t -> TyClosed (AnnType attrs t)
  TyOpen _ -> TyOpen $ Run $ \env -> AnnType attrs <$> runTyB t0 env

-- | A type of Clash Core, whose type variables are bound by the builders
-- around it, by their uniques. Unless it is closed and without binders, it
-- is converted each time it is built, like
-- 'Clash.Core.Scoped.Convert.fromClashType' does.
clashTyB :: HasCallStack => C.Type -> TyB
clashTyB ty0
  | closed ty0 = TyClosed (goClosed ty0)
  | otherwise = rnf ty0 `seq` TyOpen (Run (\env -> go env ty0))
 where
  -- Closed, and without binders
  closed = \case
    C.VarTy _ -> False
    C.ConstTy _ -> True
    C.ForAllTy _ _ -> False
    C.AppTy t1 t2 -> closed t1 && closed t2
    C.LitTy _ -> True
    C.AnnType _ t -> closed t

  goClosed :: C.Type -> Type 'VoidS
  goClosed = \case
    C.ConstTy c -> ConstTy c
    C.AppTy t1 t2 -> AppTy (goClosed t1) (goClosed t2)
    C.LitTy l -> LitTy l
    C.AnnType attrs t -> AnnType attrs (goClosed t)
    ty -> error ("Clash.Core.Scoped.Builder.clashTyB: not closed: " ++ show ty)

  go :: Distinct n => BEnv s n -> C.Type -> ST s (Type n)
  go env@(BEnv _ tys _ _) = \case
    C.VarTy tv -> case IntMap.lookup (uniqueKey (C.varUniq tv)) tys of
      Just ty -> pure ty
      Nothing -> error ("Clash.Core.Scoped.Builder: type variable not in scope: " ++ show (C.varName tv))
    C.ConstTy c -> pure (ConstTy c)
    C.ForAllTy tv t -> do
      k <- go env (C.varType tv)
      withTyBinder env (uniqueKey (C.varUniq tv)) (C.varName tv) k $ \env' b ->
        ForAllTy b <$> go env' t
    C.AppTy t1 t2 -> AppTy <$> go env t1 <*> go env t2
    C.LitTy l -> pure (LitTy l)
    C.AnnType attrs t -> AnnType attrs <$> go env t

-- * Terms

-- | A term under construction, see the module header
data TmB = TmB
  { tmbHead :: !(Maybe C.PrimInfo)
  -- ^ The primitive at the head of the term, under applications and ticks,
  -- like 'C.collectArgs'
  , tmbRun :: !(Run Term)
  }

instance NFData TmB where
  rnf t = t `seq` ()

runTmB :: Distinct n => TmB -> BEnv s n -> ST s (Term n)
runTmB tm = run (tmbRun tm)
{-# INLINE runTmB #-}

term :: Run Term -> TmB
term = TmB Nothing
{-# INLINE term #-}

-- | A local variable occurrence
varB :: HasCallStack => Key -> TmB
varB !key = term $ Run $ \(BEnv _ _ tms _) -> case IntMap.lookup key tms of
  Just tm -> pure tm
  Nothing -> error ("Clash.Core.Scoped.Builder: variable not in scope: " ++ show key)

globalB :: GlobalId -> TmB
globalB !g = term (Run (\_ -> pure (GlobalVar g)))

dataB :: DataCon -> TmB
dataB !dc = term (Run (\_ -> pure (Data dc)))

literalB :: Literal -> TmB
literalB !l = term (Run (\_ -> pure (Literal l)))

-- | A primitive, whose type must be closed
primB :: C.PrimInfo -> TmB
primB !p = TmB (Just p) (Run (\_ -> pure (Prim p)))

appB :: TmB -> TmB -> TmB
appB !f !a = TmB (tmbHead f) $ Run $ \env -> App <$> runTmB f env <*> runTmB a env

tyAppB :: TmB -> TyB -> TmB
tyAppB !f !t = TmB (tmbHead f) $ Run $ \env -> TyApp <$> runTmB f env <*> runTyB t env

-- | A lambda: key, name and type of the variable, and the body
lamB :: Key -> C.Name C.Term -> TyB -> TmB -> TmB
lamB !key !occ !ty !e = term $ Run $ \env -> do
  ty' <- runTyB ty env
  withIdBinder env key occ ty' $ \env' b -> Lam b <$> runTmB e env'

-- | A type lambda: key, name and kind of the variable, and the body
tyLamB :: Key -> C.Name C.Type -> TyB -> TmB -> TmB
tyLamB !key !occ !k !e = term $ Run $ \env -> do
  k' <- runTyB k env
  withTyBinder env key occ k' $ \env' b -> TyLam b <$> runTmB e env'

-- | A type lambda binding a type variable of Clash Core, which the body
-- refers to by its unique, see 'clashTyB'
tyLamClashB :: C.TyVar -> TmB -> TmB
tyLamClashB tv =
  tyLamB (uniqueKey (C.varUniq tv)) (C.varName tv) (clashTyB (C.varType tv))

-- | A non-recursive let-binding: key, name and type of the variable, its
-- right-hand side, and the body
letB :: Key -> C.Name C.Term -> TyB -> TmB -> TmB -> TmB
letB !key !occ !ty !x !e = term $ Run $ \env -> do
  x' <- runTmB x env
  ty' <- runTyB ty env
  withIdBinder env key occ ty' $ \env' b -> Let (NonRec b x') <$> runTmB e env'

-- | A recursive let-binding: keys, names, types and right-hand sides of the
-- variables, and the body
letrecB :: [(Key, C.Name C.Term, TyB, TmB)] -> TmB -> TmB
letrecB !bs0 !e = forceList bs0 `seq` term (Run go0)
 where
  go0 :: Distinct n => BEnv s n -> ST s (Term n)
  go0 env = binders env bs0 $ \env' tele -> do
    xs <- mapM (\(_, _, _, x) -> runTmB x env') bs0
    Let (Rec tele xs) <$> runTmB e env'

  binders
    :: Distinct x
    => BEnv s x
    -> [(Key, C.Name C.Term, TyB, TmB)]
    -> (forall l. DExt x l => BEnv s l -> Tele IdBinder x l -> ST s r)
    -> ST s r
  binders env [] k = k env TNil
  binders env ((key, occ, ty, _):bs) k = do
    ty' <- runTyB ty env
    withIdBinder env key occ ty' $ \env1 b ->
      binders env1 bs $ \env2 bs' -> k env2 (TCons b bs')

  forceList = foldr (\(key, occ, ty, x) r -> key `seq` occ `seq` ty `seq` x `seq` r) ()

-- | A case-expression: subject, type of the alternatives, alternatives
caseB :: TmB -> TyB -> [AltB] -> TmB
caseB !s !ty !alts = foldr seq () alts `seq` term (Run go)
 where
  go :: Distinct n => BEnv s n -> ST s (Term n)
  go env = Case <$> runTmB s env <*> runTyB ty env <*> mapM (\(AltB a) -> run a env) alts

castB :: TmB -> TyB -> TyB -> TmB
castB !e !t1 !t2 = term $ Run $ \env ->
  Cast <$> runTmB e env <*> runTyB t1 env <*> runTyB t2 env

tickB :: TickB -> TmB -> TmB
tickB (TickB !t) !e = TmB (tmbHead e) $ Run $ \env -> Tick <$> run t env <*> runTmB e env

-- * Alternatives

-- | A case alternative under construction
newtype AltB = AltB (Run Alt)

defaultAltB :: TmB -> AltB
defaultAltB !e = AltB $ Run $ \env -> Alt DefaultPat <$> runTmB e env

litAltB :: Literal -> TmB -> AltB
litAltB !l !e = AltB $ Run $ \env -> Alt (LitPat l) <$> runTmB e env

-- | An alternative of a data constructor: the data constructor, the keys,
-- names and kinds of the existential type variables, the keys, names and
-- types of the fields, and the right-hand side
dataAltB
  :: DataCon
  -> [(Key, C.Name C.Type, TyB)]
  -> [(Key, C.Name C.Term, TyB)]
  -> TmB
  -> AltB
dataAltB !dc !tvs0 !ids0 !e =
  forceList tvs0 `seq` forceList ids0 `seq` AltB (Run go0)
 where
  go0 :: Distinct n => BEnv s n -> ST s (Alt n)
  go0 env =
    tyBinders env tvs0 $ \env1 tvs ->
      idBinders env1 ids0 $ \env2 ids ->
        Alt (DataPat dc tvs ids) <$> runTmB e env2

  tyBinders
    :: Distinct x
    => BEnv s x
    -> [(Key, C.Name C.Type, TyB)]
    -> (forall l. DExt x l => BEnv s l -> Tele TyBinder x l -> ST s r)
    -> ST s r
  tyBinders env [] k = k env TNil
  tyBinders env ((key, occ, ki):bs) k = do
    ki' <- runTyB ki env
    withTyBinder env key occ ki' $ \env1 b ->
      tyBinders env1 bs $ \env2 bs' -> k env2 (TCons b bs')

  idBinders
    :: Distinct x
    => BEnv s x
    -> [(Key, C.Name C.Term, TyB)]
    -> (forall l. DExt x l => BEnv s l -> Tele IdBinder x l -> ST s r)
    -> ST s r
  idBinders env [] k = k env TNil
  idBinders env ((key, occ, ty):bs) k = do
    ty' <- runTyB ty env
    withIdBinder env key occ ty' $ \env1 b ->
      idBinders env1 bs $ \env2 bs' -> k env2 (TCons b bs')

  forceList :: [(Key, a, TyB)] -> ()
  forceList = foldr (\(key, occ, ty) r -> key `seq` occ `seq` ty `seq` r) ()

-- * Ticks

-- | A tick under construction
newtype TickB = TickB (Run TickInfo)

srcSpanTickB :: SrcSpan -> TickB
srcSpanTickB !sp = TickB (Run (\_ -> pure (SrcSpan sp)))

nameModTickB :: C.NameMod -> TyB -> TickB
nameModTickB !m !ty = TickB $ Run $ \env -> NameMod m <$> runTyB ty env

deDupTickB :: TickB
deDupTickB = TickB (Run (\_ -> pure DeDup))

noDeDupTickB :: TickB
noDeDupTickB = TickB (Run (\_ -> pure NoDeDup))

attributesTickB :: TyB -> TmB -> TickB
attributesTickB !ty !e = TickB $ Run $ \env -> Attributes <$> runTyB ty env <*> runTmB e env
