{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Type and instance definitions for Rewrite modules

  The rewrite context and the transformation type of scoped Clash Core
  ("Clash.Core.Scoped.Syntax"). See "Clash.Rewrite.Scoped" for how to write a
  transformation.

  = The context

  A @'TransformContext' n@ is the context of a subterm in scope @n@: the
  'Scope' itself, to create fresh binders, the 'Path' from the root of the
  term to the subterm, and the let-bindings of the path, for the evaluator
  ('tfHeap'). Frames that bind variables are scoped: the path is a
  snoc-list in which every frame lives in the scope of the subterms below it,
  and a frame that goes under binders ('Under') extends the scope. So the
  binders on the path never shadow one another, and a let-binding on the path
  is valid at the subterm as it is (after sinking).

  Only traversals extend the context, and they do so while opening the binder
  they go under: see 'underLam', 'underTyLam', 'underAlt', 'underLet' and
  'underLetRec', which all go through 'pushUnder'. The constructor of the
  context is not exported.

  = Let-bindings

  'Clash.Rewrite.Scoped.Whnf.whnfRW' evaluates a subterm with the let-bindings
  of its context on the heap of the evaluator: the context carries them. The
  context keeps that heap, 'tfHeap', as it is extended: 'pushUnder' adds the
  bindings of a 'LetBody', and sinks it under every other binder. Its keys are
  the names of the scope @n@ of the subterm, which is t'Distinct'. So a binder
  between a let-binding and the subterm can't shadow the let-binder, nor a
  free variable of its right-hand side (see #2571): it has a different name.
  And the heap is built as the context is, instead of from all frames at every
  evaluation. It is lazy, so it is only built when it is used.

  = Inspecting the context

  Transformations inspect the context through 'ctxFrames', which lists the
  kinds of its frames from the subterm outwards. E.g. whether a subterm is the function of an
  application:

  @
  case 'ctxFrames' ctx of
    'KAppFun' : _ -> ...
  @
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Rewrite.Types
  ( -- * Rewrite history
    RewriteStep (..)
    -- * Rewriting sessions
  , RewriteState (..)
  , transformCounter
  , transformAppliedCounters
  , transformTriedCounters
  , uniqSupply
  , curFun
  , nameCounter
  , globalHeap
  , bindings
  , workFreeBinders
  , hwTypeCache
  , extra
  , RewriteEnv (..)
  , clashEnv
  , typeTranslator
  , topEntities
  , typeEnv
  , clashOpts
  , debugOpts
  , aggressiveXOpt
  , tcCache
  , tupleTcCache
  , customReprs
  , fuelLimit
  , primitives
  , inlineLimit
  , inlineFunctionLimit
  , inlineConstantLimit
  , inlineWFCacheLimit
  , newInlineStrategy
  , specializationLimit
  , normalizeUltra
  , RewriteMonad (..)
#if __GLASGOW_HASKELL__ >= 914
  , data R
#else
  , pattern R
#endif
  , runR
  , censor
    -- * Transformations
  , Transform (..)
  , Rewrite
    -- * The context
  , TransformContext
  , tfScope
  , tfPath
  , tfHeap
  , Path (..)
  , Frame (..)
  , Under (..)
  , rootContext
  , pushFrame
  , pushUnder
  , appArgFrame
    -- * Going under binders
  , underLam
  , underTyLam
  , underAlt
  , underLet
  , underLetRec
    -- * Inspecting the context
  , FrameKind (..)
  , ctxFrames
  , isLambdaBodyKind
  , isTickKind
  , closestLetBinder
  , mkDerivedName
  ) where

import Control.DeepSeq                       (NFData)
import Control.Lens                          (use, (.=))
import qualified Control.Lens as Lens
import Control.Monad.Fix                     (MonadFix (..))
import Control.Monad.IO.Class                (MonadIO (..))
import Control.Monad.Reader                  (MonadReader (..))
import Control.Monad.State                   (MonadState (..))
import Control.Monad.Writer                  (MonadWriter (..))
import Data.Binary                           (Binary)
import Data.HashMap.Strict                   (HashMap)
import Data.IntMap.Strict                    (IntMap)
import qualified Data.List                   as List
import Data.Monoid                           (Any)
import Data.Text                             (Text)
import qualified Data.Text                   as Text
import GHC.Exts                              (oneShot)
import GHC.Generics                          (Generic)
import GHC.Stack                             (HasCallStack)

import Clash.Core.Scoped.TypeEnv (TypeEnv)

import Clash.Core.TyCon          (TyConMap, TyConName)
import Clash.Driver.Types
  ( Binding, ClashEnv(..), ClashOpts(..), DebugOpts, HasClashOpts(..) )
import qualified Clash.Core.Name           as C
import Clash.Core.Scoped.NameMap
  (GlobalEnv, GlobalSet, NameMap, emptyNameMap, insertNameMap, sinkNameMap)
import Clash.Core.Scoped.Evaluator.Types (PrimHeap)
import Clash.Core.Scoped.Subst
  (openAlt, openBind, openIdBinder, openTyBinder, teleIdVars)
import Clash.Core.Scoped.Syntax
import qualified Clash.Core.Scoped.Syntax as S
import Clash.Core.Scoped.Term   (primArg)
import Clash.Driver.Warning      (CanWarn)
import Clash.Netlist.Types       (HWMap, HWTypeTranslator)
import Clash.Primitives.Types    (CompiledPrimMap)
import Clash.Util
import Clash.Util.Supply         (Supply, freshId)

import Clash.Annotations.BitRepresentation.Internal (CustomReprs)

-- | A step of the rewrite history of @-fclash-debug-history@, see
-- "Clash.Rewrite.Scoped.History"
data RewriteStep
  = RewriteStep
  { t_name   :: String
  -- ^ Name of the transformation
  , t_bndrS  :: String
  -- ^ Name of the current binder
  , t_free   :: Int
  -- ^ The number of free variables of the rewritten term that 't_before' and
  -- 't_after' are abstracted over: their outermost type lambdas and lambdas
  , t_before :: S.Term 'S.VoidS
  -- ^ Term before `apply`, abstracted over the free variables
  , t_after  :: S.Term 'S.VoidS
  -- ^ Term after `apply`, abstracted over the free variables
  } deriving (Show, Generic, NFData, Binary)

-- | State of a rewriting session
data RewriteState extra
  = RewriteState
    -- TODO Given we now keep transformCounter, this should just be 'fold'
    -- over that map, otherwise the two counts could fall out of sync.
  { _transformCounter :: {-# UNPACK #-} !Word
  -- ^ Total number of applied transformations
  , _transformAppliedCounters :: HashMap Text Word
  -- ^ Map that tracks how many times each transformation is applied
  , _transformTriedCounters :: HashMap Text Word
  -- ^ Map that tracks how many times each transformation has been tried
  , _uniqSupply       :: !Supply
  -- ^ Supply of unique numbers
  , _curFun           :: (S.GlobalId,SrcSpan) -- Initially set to undefined: no strictness annotation
  -- ^ Function which is currently normalized
  , _nameCounter      :: {-# UNPACK #-} !Int
  -- ^ Used for 'Fresh'
  , _globalHeap       :: PrimHeap
  -- ^ Used as a heap for compile-time evaluation of primitives that live in I/O
  , _bindings   :: GlobalEnv (Binding (S.Term 'S.VoidS))
  -- ^ Global binders, as closed terms of scoped Clash Core
  , _workFreeBinders :: GlobalEnv Bool
  -- ^ Map telling whether the definition of a global binder in
  -- '_bindings' is work-free
  , _hwTypeCache      :: HWMap
  -- ^ Cache for the Core-type to HWType translation. The translation only
  -- depends on environment that is constant for the whole rewrite session
  -- (the type translator, custom representations, and the TyConMap), so the
  -- cache never has to be invalidated.
  , _extra            :: !extra
  -- ^ Additional state
  }

Lens.makeLenses ''RewriteState

-- | Read-only environment of a rewriting session
data RewriteEnv
  = RewriteEnv
  { _clashEnv       :: ClashEnv
  -- ^ The global environment of the compiler
  , _typeTranslator :: HWTypeTranslator
  -- ^ Hardcode Type -> FilteredHWType translator
  , _topEntities    :: GlobalSet
  -- ^ Functions that are considered TopEntities
  , _typeEnv  :: TypeEnv
  -- ^ The type constructors of 'tcCache', and the globals of the bindings at
  -- the start of the session, for scoped Clash Core. It caches their
  -- signatures as scoped types, see "Clash.Core.Scoped.TypeEnv": make it once
  -- per session.
  }

Lens.makeLenses ''RewriteEnv

clashOpts :: Lens.Getter RewriteEnv ClashOpts
clashOpts = clashEnv . Lens.to envOpts

debugOpts :: Lens.Getter RewriteEnv DebugOpts
debugOpts = clashEnv . Lens.to (opt_debug . envOpts)

aggressiveXOpt :: Lens.Getter RewriteEnv Bool
aggressiveXOpt = clashEnv . Lens.to (opt_aggressiveXOpt . envOpts)

tcCache :: Lens.Getter RewriteEnv TyConMap
tcCache = clashEnv . Lens.to envTyConMap

tupleTcCache :: Lens.Getter RewriteEnv (IntMap TyConName)
tupleTcCache = clashEnv . Lens.to envTupleTyCons

customReprs :: Lens.Getter RewriteEnv CustomReprs
customReprs = clashEnv . Lens.to envCustomReprs

fuelLimit :: Lens.Getter RewriteEnv Word
fuelLimit = clashEnv . Lens.to (opt_evaluatorFuelLimit . envOpts)

primitives :: Lens.Getter RewriteEnv CompiledPrimMap
primitives = clashEnv . Lens.to envPrimitives

inlineLimit :: Lens.Getter RewriteEnv Int
inlineLimit = clashEnv . Lens.to (opt_inlineLimit . envOpts)

inlineFunctionLimit :: Lens.Getter RewriteEnv Word
inlineFunctionLimit = clashEnv . Lens.to (opt_inlineFunctionLimit . envOpts)

inlineConstantLimit :: Lens.Getter RewriteEnv Word
inlineConstantLimit = clashEnv . Lens.to (opt_inlineConstantLimit . envOpts)

inlineWFCacheLimit :: Lens.Getter RewriteEnv Word
inlineWFCacheLimit = clashEnv . Lens.to (opt_inlineWFCacheLimit . envOpts)

newInlineStrategy :: Lens.Getter RewriteEnv Bool
newInlineStrategy = clashEnv . Lens.to (opt_newInlineStrat . envOpts)

specializationLimit :: Lens.Getter RewriteEnv Int
specializationLimit = clashEnv . Lens.to (opt_specLimit . envOpts)

normalizeUltra :: Lens.Getter RewriteEnv Bool
normalizeUltra = clashEnv . Lens.to (opt_ultra . envOpts)

-- | Monad that keeps track how many transformations have been applied and can
-- generate fresh variables and unique identifiers. In addition, it keeps track
-- if a transformation/rewrite has been successfully applied.
--
-- A reader, writer and state monad over 'IO', whose functions are one-shot:
-- see Note [one-shot rewrite monad].
newtype RewriteMonad extra a = MkR
  { unR :: RewriteEnv -> RewriteState extra -> Any -> IO (a, RewriteState extra, Any) }

-- | Make a 'RewriteMonad' action, marking its function one-shot, see Note
-- [one-shot rewrite monad]
pattern R
  :: (RewriteEnv -> RewriteState extra -> Any -> IO (a, RewriteState extra, Any))
  -> RewriteMonad extra a
pattern R m <- MkR m
  where
    R m = MkR (oneShot (\r -> oneShot (\s -> oneShot (\w -> m r s w))))
{-# COMPLETE R #-}

{-
Note [one-shot rewrite monad]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An action of the rewrite monad is a function of the environment, the state and
the change flag. When GHC sees an action built inside such a function from
values bound outside of it, full laziness floats the building out of the
function, so that it is shared between runs of the function. But an action
almost always runs once: the floating only allocates the action, and the
closures it captures, earlier and unconditionally. In the traversals of
"Clash.Rewrite.Scoped.Combinators" and "Clash.Rewrite.StrategyDSL.TH", that
happened for every node: GHC built the actions for all constructors before the
node was matched.

'R' marks the functions one-shot ('oneShot'), which tells GHC that they run at
most once, so it leaves the building where it is. When an action does run
more than once, work inside it is redone rather than shared, which is what
the code says anyway. GHC's own monads do the same, see Note [The one-shot
state monad trick] in GHC.Utils.Monad.

The instances are those of
@'Control.Monad.Trans.RWS.CPS.RWST' 'RewriteEnv' 'Any' ('RewriteState' extra) 'IO'@,
which this monad was before.
-}

instance Functor (RewriteMonad extra) where
  fmap f (R m) = R $ \r s w -> do
    (a, s', w') <- m r s w
    pure (f a, s', w')
  {-# INLINE fmap #-}

instance Applicative (RewriteMonad extra) where
  pure a = R $ \_ s w -> pure (a, s, w)
  {-# INLINE pure #-}
  R mf <*> R mx = R $ \r s w -> do
    (f, s', w') <- mf r s w
    (x, s'', w'') <- mx r s' w'
    pure (f x, s'', w'')
  {-# INLINE (<*>) #-}

instance Monad (RewriteMonad extra) where
  R m >>= k = R $ \r s w -> do
    (a, s', w') <- m r s w
    unR (k a) r s' w'
  {-# INLINE (>>=) #-}

instance MonadFix (RewriteMonad extra) where
  mfix f = R $ \r s w -> mfix $ \ ~(a, _, _) -> unR (f a) r s w

instance MonadIO (RewriteMonad extra) where
  liftIO io = R $ \_ s w -> do
    a <- io
    pure (a, s, w)
  {-# INLINE liftIO #-}

instance MonadState (RewriteState extra) (RewriteMonad extra) where
  get = R $ \_ s w -> pure (s, s, w)
  {-# INLINE get #-}
  put s = R $ \_ _ w -> pure ((), s, w)
  {-# INLINE put #-}
  state f = R $ \_ s w -> let (a, s') = f s in pure (a, s', w)
  {-# INLINE state #-}

instance MonadReader RewriteEnv (RewriteMonad extra) where
  ask = R $ \r s w -> pure (r, s, w)
  {-# INLINE ask #-}
  local f (R m) = R $ \r s w -> m (f r) s w
  {-# INLINE local #-}
  reader f = R $ \r s w -> pure (f r, s, w)
  {-# INLINE reader #-}

instance MonadWriter Any (RewriteMonad extra) where
  tell w' = R $ \_ s w -> let wt = w <> w' in wt `seq` pure ((), s, wt)
  {-# INLINE tell #-}
  listen (R m) = R $ \r s w -> do
    (a, s', w') <- m r s mempty
    let wt = w <> w'
    wt `seq` pure ((a, w'), s', wt)
  {-# INLINE listen #-}
  pass (R m) = R $ \r s w -> do
    ((a, f), s', w') <- m r s mempty
    let wt = w <> f w'
    wt `seq` pure (a, s', wt)
  {-# INLINE pass #-}

instance HasClashOpts (RewriteMonad extra) where
  askClashOpts = Lens.view clashOpts

instance CanWarn (RewriteMonad extra)

-- | Run the computation in the RewriteMonad
runR
  :: RewriteMonad extra a
  -> RewriteEnv
  -> RewriteState extra
  -> IO (a, RewriteState extra, Any)
runR m r s = unR m r s mempty

instance MonadUnique (RewriteMonad extra) where
  getUniqueM = do
    sup <- use uniqSupply
    let (a,sup') = freshId sup
    uniqSupply .= sup'
    a `seq` return a

censor :: (Any -> Any) -> RewriteMonad extra a -> RewriteMonad extra a
censor f (R m) = R $ \r s w -> do
  (a, s', w') <- m r s mempty
  let wt = w <> f w'
  wt `seq` pure (a, s', wt)
{-# INLINE censor #-}

-- * Transformations

-- | A transformation of a term in any scope, in its context. A newtype rather
-- than a rank-2 type synonym, so that transformations can be stored, passed to
-- combinators and defined recursively without type signatures.
newtype Transform m = Transform
  { runTransform
      :: forall n. Distinct n => TransformContext n -> Term n -> m (Term n)
  }

-- | A 'Transform' in the 'RewriteMonad'
type Rewrite extra = Transform (RewriteMonad extra)

-- * The context

-- | The context of a subterm in scope @n@, see the module header
--
-- The fields are not record fields, so that they can't be updated: only the
-- functions below extend a context, keeping its heap consistent with its path.
data TransformContext (n :: S) = TransformContext
  !(Scope n)
  !(Path n)
  (NameMap TmName n (Term n))
type role TransformContext nominal

-- | The scope of the subterm
tfScope :: TransformContext n -> Scope n
tfScope (TransformContext sc _ _) = sc
{-# INLINE tfScope #-}

-- | The path from the root to the subterm
tfPath :: TransformContext n -> Path n
tfPath (TransformContext _ p _) = p
{-# INLINE tfPath #-}

-- | The let-bindings of the path, valid at the subterm: right-hand sides by
-- the names of their binders. See the module header. Lazy: only built when
-- it is used.
tfHeap :: TransformContext n -> NameMap TmName n (Term n)
tfHeap (TransformContext _ _ h) = h
{-# INLINE tfHeap #-}

-- | The path from the root of a term to a subterm in scope @n@, innermost
-- frame first
data Path (n :: S) where
  -- | The root of a term
  PRoot :: Path n
  -- | A frame that doesn't bind variables
  PFrame :: !(Frame n) -> !(Path n) -> Path n
  -- | A frame that goes under binders, extending scope @i@ to @n@
  PUnder :: (Distinct i, DExt i n) => !(Under i n) -> !(Path i) -> Path n
type role Path nominal

-- | A frame of the context that doesn't bind variables
data Frame (n :: S) where
  -- | Function position of an application
  AppFun :: Frame n
  -- | Argument position of an application. If the function is an applied
  -- primitive: the name of the primitive, and the numbers of type and term
  -- arguments it is applied to. See 'appArgFrame'.
  AppArg :: !(Maybe (Text, Int, Int)) -> Frame n
  -- | Function position of a type application
  TyAppC :: Frame n
  -- | Right-hand side of a non-recursive let-binding. Its binder is in scope
  -- of the body only, not of the right-hand side.
  LetBinding :: !(IdBinder n l) -> Frame n
  -- | Subject of a case-expression
  CaseScrut :: Frame n
  -- | Body of a cast
  CastBody :: Frame n
  -- | Body of a tick
  TickC :: !(TickInfo n) -> Frame n
type role Frame nominal

-- | A frame of the context that goes under binders, extending scope @i@ to
-- @n@
data Under (i :: S) (n :: S) where
  -- | Body of a lambda
  LamBody :: !(IdBinder i n) -> Under i n
  -- | Body of a type lambda
  TyLamBody :: !(TyBinder i n) -> Under i n
  -- | Right-hand side of a case alternative
  CaseAlt :: !(Pat i n) -> Under i n
  -- | Body of a let-expression, with its bindings (as they were before the
  -- right-hand sides were transformed)
  LetBody :: !(Bind i n) -> Under i n
  -- | The right-hand side with the given index of a recursive let-binding
  LetBindingRec :: !(Tele IdBinder i n) -> !Int -> Under i n
type role Under nominal nominal

-- | The context of the root of a term in scope @n@
rootContext :: Scope n -> TransformContext n
rootContext sc = TransformContext sc PRoot emptyNameMap

-- | Push a frame that doesn't bind variables
pushFrame :: Frame n -> TransformContext n -> TransformContext n
pushFrame f (TransformContext sc p h) = TransformContext sc (PFrame f p) h
{-# INLINE pushFrame #-}

-- | Push a frame that goes under binders. The scope is the one that opening
-- the binders gave, e.g. by 'Clash.Core.Scoped.Subst.openIdBinder'. This is
-- the only way to extend the scope of a context.
--
-- The let-bindings of a 'LetBody' are added to the heap ('tfHeap'), as they
-- were before their right-hand sides were transformed. The other bindings of
-- a recursive let-binding are not, while transforming one of its right-hand
-- sides ('LetBindingRec').
pushUnder
  :: (Distinct i, DExt i n)
  => Scope n -> Under i n -> TransformContext i -> TransformContext n
pushUnder sc u (TransformContext _ p h) = TransformContext sc (PUnder u p) h'
 where
  h0 = sink1 (sinkNameMap h)
  h' = case u of
    LetBody (NonRec b x) -> insertNameMap (tmNameOf b) (sink x) h0
    LetBody (Rec bs xs) ->
      List.foldl' (\acc (v, x) -> case v of
                      Var _ _ nm -> insertNameMap nm x acc
                      _ -> acc)
                  h0 (zip (teleIdVars bs) xs)
    LetBindingRec {} -> h0
    LamBody {} -> h0
    TyLamBody {} -> h0
    CaseAlt {} -> h0
{-# INLINE pushUnder #-}

-- | The frame of the argument of an application, given the (already
-- transformed) function. See 'primArg'.
appArgFrame :: Term n -> Frame n
appArgFrame = AppArg . primArg

-- * Going under binders

-- | Go under the binder of a lambda: open it (see
-- 'Clash.Core.Scoped.Subst.openIdBinder'), and give the context of the body.
underLam
  :: Distinct n
  => TransformContext n
  -> IdBinder n l
  -> Term l
  -> (forall l'. DExt n l' => TransformContext l' -> IdBinder n l' -> Term l' -> r)
  -> r
underLam ctx b e k =
  openIdBinder (tfScope ctx) b e $ \sc b' e' ->
    k (pushUnder sc (LamBody b') ctx) b' e'
{-# INLINE underLam #-}

-- | Go under the binder of a type lambda, see 'underLam'
underTyLam
  :: Distinct n
  => TransformContext n
  -> TyBinder n l
  -> Term l
  -> (forall l'. DExt n l' => TransformContext l' -> TyBinder n l' -> Term l' -> r)
  -> r
underTyLam ctx b e k =
  openTyBinder (tfScope ctx) b e $ \sc b' e' ->
    k (pushUnder sc (TyLamBody b') ctx) b' e'
{-# INLINE underTyLam #-}

-- | Go under the pattern of a case alternative, see 'underLam'
underAlt
  :: Distinct n
  => TransformContext n
  -> Alt n
  -> (forall l. DExt n l => TransformContext l -> Pat n l -> Term l -> r)
  -> r
underAlt ctx alt k =
  openAlt (tfScope ctx) alt $ \sc p e ->
    k (pushUnder sc (CaseAlt p) ctx) p e
{-# INLINE underAlt #-}

-- | Go under the binders of a let-expression, see 'underLam'. Gives the
-- context of the body. For the right-hand sides of a recursive let-binding,
-- see 'underLetRec'; that of a non-recursive one lives in the scope of the
-- let-expression, in the context @'pushFrame' ('LetBinding' b) ctx@.
underLet
  :: Distinct n
  => TransformContext n
  -> Bind n l
  -> Term l
  -> (forall l'. DExt n l' => TransformContext l' -> Bind n l' -> Term l' -> r)
  -> r
underLet ctx bind e k =
  openBind (tfScope ctx) bind e $ \sc bind' e' ->
    k (pushUnder sc (LetBody bind') ctx) bind' e'
{-# INLINE underLet #-}

-- | Go under the binders of a recursive let-expression. Gives the context of
-- the body, and those of the right-hand sides, by index.
underLetRec
  :: Distinct n
  => TransformContext n
  -> Tele IdBinder n l
  -> [Term l]
  -> Term l
  -> (forall l'. DExt n l'
        => TransformContext l'
        -> (Int -> TransformContext l')
        -> Tele IdBinder n l'
        -> [Term l']
        -> Term l'
        -> r)
  -> r
underLetRec ctx bs xs e k =
  openBind (tfScope ctx) (Rec bs xs) e $ \sc bind' e' -> case bind' of
    Rec bs' xs' ->
      k (pushUnder sc (LetBody bind') ctx)
        (\i -> pushUnder sc (LetBindingRec bs' i) ctx)
        bs' xs' e'
    NonRec {} -> error "underLetRec: opening a Rec gave a NonRec"
{-# INLINE underLetRec #-}

-- * Inspecting the context

-- | The kind of a frame of the context, without its binders. See
-- 'ctxFrames'.
data FrameKind
  = KAppFun
  | KAppArg !(Maybe (Text, Int, Int))
  -- ^ See 'AppArg'
  | KTyAppC
  | KLetBinding !(C.Name Term)
  -- ^ Right-hand side of a let-binding, with the name of its binder
  | KLetBody
  | KLamBody
  | KTyLamBody
  | KCaseAlt
  | KCaseScrut
  | KCastBody
  | KTickC
  deriving (Eq, Show)

-- | The kinds of the frames of the context, from the subterm outwards. Lazy:
-- only the frames that are looked at are computed.
ctxFrames :: TransformContext n -> [FrameKind]
ctxFrames = go . tfPath
 where
  go :: Path x -> [FrameKind]
  go = \case
    PRoot -> []
    PFrame f p -> frameKind f : go p
    PUnder u p -> underKind u : go p

  frameKind :: Frame x -> FrameKind
  frameKind = \case
    AppFun -> KAppFun
    AppArg a -> KAppArg a
    TyAppC -> KTyAppC
    LetBinding b -> KLetBinding (idBinderOcc b)
    CaseScrut -> KCaseScrut
    CastBody -> KCastBody
    TickC _ -> KTickC

  underKind :: Under x y -> FrameKind
  underKind = \case
    LamBody _ -> KLamBody
    TyLamBody _ -> KTyLamBody
    CaseAlt _ -> KCaseAlt
    LetBody _ -> KLetBody
    LetBindingRec bs i -> KLetBinding (teleOcc bs i)

  teleOcc :: Tele IdBinder x y -> Int -> C.Name Term
  teleOcc TNil _ = error "ctxFrames: LetBindingRec index out of range"
  teleOcc (TCons b bs) i
    | i <= 0 = idBinderOcc b
    | otherwise = teleOcc bs (i - 1)

-- | Is the frame the body of a lambda?
isLambdaBodyKind :: FrameKind -> Bool
isLambdaBodyKind KLamBody = True
isLambdaBodyKind _ = False

-- | Is the frame the body of a tick?
isTickKind :: FrameKind -> Bool
isTickKind KTickC = True
isTickKind _ = False

-- | The name of the binder of the closest let-binding whose right-hand side
-- the subterm is in
closestLetBinder :: TransformContext n -> Maybe (C.Name Term)
closestLetBinder ctx = case [nm | KLetBinding nm <- ctxFrames ctx] of
  nm : _ -> Just nm
  [] -> Nothing

-- | A name for a new binder, derived from the closest let-binder: the name
-- of that binder with @_@ and the given suffix appended, or just the suffix
-- if the subterm isn't in the right-hand side of a let-binding. Its unique
-- is not used: scoped binders get fresh names from the scope.
mkDerivedName :: HasCallStack => TransformContext n -> C.OccName -> C.Name Term
mkDerivedName ctx sf = case closestLetBinder ctx of
  Just nm -> C.appendToName nm ('_' `Text.cons` sf)
  Nothing -> C.mkUnsafeInternalName sf 0
