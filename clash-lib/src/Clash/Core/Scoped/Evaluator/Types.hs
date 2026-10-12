{-|
  Copyright   :  (C) 2020-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The types of the whnf evaluator of scoped Clash Core: values, stack frames,
  heaps and the machine. Ports of "Clash.Core.Evaluator.Types". The machine
  itself is "Clash.Core.Scoped.Evaluator".

  = The scope of the machine

  The machine of "Clash.GHC.Evaluator" is /flat/: the term under evaluation,
  every heap binding and every stack frame live in one scope, the machine's
  in-scope set, which only grows. Allocating a let-binding renames its binder
  to a name that is fresh for the whole machine; applying a lambda
  substitutes the heap variable of the argument for its binder. No part of
  the machine is ever under a binder that another part isn't.

  So the scoped machine has a single scope for all of them: a
  @'Machine' o n@ holds a term, heaps and a stack in scope @n@. The other
  index, @o@, is the scope of the /caller/, e.g. that of the rewrite context.
  The machine records the binders it allocated, which extend @o@ to @n@, in
  'mAlloc'. That makes the result well-scoped by construction: the bindings
  to wrap around the result are exactly those binders, which
  'Clash.Core.Scoped.Evaluator.bindAllocated' turns into a @letrec@ binding
  the names of @n@ that are not in @o@.

  Allocating moves the whole machine into the extended scope
  ('extendMachine'). That is 'sink' on every part, a coercion, so it takes
  constant time rather than a traversal of the heaps and the stack.

  = Heaps

  As in "Clash.Core.Evaluator.Types", there are three heaps: one of the
  primitives (byte arrays), one of global bindings, and one of local ones.
  The local heap is keyed by the local names of the machine's scope
  ('TmName'), the global one by 'GlobalId' (decision D5 of the port), so the
  heap of one can't be consulted for the other. As the names of a
  t'Distinct' scope are distinct, a local key identifies one binder: a
  binding can't be found under a variable that merely has the same name as
  its binder.

  The global bindings are closed terms; the machine moves them into its
  scope ('sinkClosed') when it looks them up.
  Evaluated globals ('mHeapGlobalOverlay') can mention heap variables, so
  they are scoped.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Clash.Core.Scoped.Evaluator.Types
  ( -- * Values
    Value (..)
  , valToTerm
  , collectValueTicks
    -- * Heap variables
  , HeapVar (..)
  , heapVarTerm
  , idBinderHeapVar
    -- * Stack
  , StackFrame (..)
    -- * Heaps
  , PrimHeap
  , PureHeap
    -- * Allocated binders
  , Alloc (..)
  , allocTele
  , allocLength
    -- * The machine
  , Machine (..)
  , initMachine
  , extendMachine
  , extendMachineTele
  , forcePrims
    -- * Steps
  , Next (..)
  , stay
  , nextFrom
    -- * Primitives
  , Env (..)
  , PrimReductions (..)
  , PrimReduction (..)
  , PrimContext (..)
  ) where

import qualified Data.List as List
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)

import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal)
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import Clash.Driver.Types (Binding)

import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TypeEnv (TypeEnv)

-- * Values

-- | A value: the result of evaluating a term to WHNF. See
-- 'Clash.Core.Evaluator.Types.Value'.
data Value (n :: S) where
  -- | Function
  Lambda :: !(IdBinder n l) -> !(Term l) -> Value n
  -- | Type abstraction
  TyLambda :: !(TyBinder n l) -> !(Term l) -> Value n
  -- | Data constructor application
  DC :: !DataCon -> [Either (Term n) (Type n)] -> Value n
  -- | Literal
  Lit :: !Literal -> Value n
  -- | Clash's number types are represented by their @fromInteger#@ primitive
  -- function. So some primitives are values.
  PrimVal :: !PrimInfo -> [Type n] -> [Value n] -> Value n
  -- | Used by lazy primitives
  Suspend :: !(Term n) -> Value n
  -- | Preserve ticks from terms in values
  TickValue :: !(TickInfo n) -> !(Value n) -> Value n
  -- | Preserve casts from terms in values
  CastValue :: !(Value n) -> !(Type n) -> !(Type n) -> Value n
type role Value nominal

deriving instance Show (Value n)

instance Sinkable Value where
  sinkabilityProof r = \case
    Lambda b e -> sinkIdBinder r b $ \r' b' -> Lambda b' (sinkabilityProof r' e)
    TyLambda b e -> sinkTyBinder r b $ \r' b' -> TyLambda b' (sinkabilityProof r' e)
    DC dc args ->
      DC dc (map (either (Left . sinkabilityProof r) (Right . sinkabilityProof r)) args)
    Lit l -> Lit l
    PrimVal p tys vs ->
      PrimVal p (map (sinkabilityProof r) tys) (map (sinkabilityProof r) vs)
    Suspend e -> Suspend (sinkabilityProof r e)
    TickValue t v -> TickValue (sinkabilityProof r t) (sinkabilityProof r v)
    CastValue v t1 t2 ->
      CastValue (sinkabilityProof r v) (sinkabilityProof r t1) (sinkabilityProof r t2)

-- | The term of a value
valToTerm :: Value n -> Term n
valToTerm = \case
  Lambda b e -> Lam b e
  TyLambda b e -> TyLam b e
  DC dc args -> List.foldl' (\e a -> either (App e) (TyApp e) a) (Data dc) args
  Lit l -> Literal l
  PrimVal p tys vs ->
    List.foldl' App (List.foldl' TyApp (Prim p) tys) (map valToTerm vs)
  Suspend e -> e
  TickValue t v -> Tick t (valToTerm v)
  CastValue v t1 t2 -> Cast (valToTerm v) t1 t2

-- | Collect the ticks around a value, exposing the ticked value
collectValueTicks :: Value n -> (Value n, [TickInfo n])
collectValueTicks = go []
 where
  go ticks (TickValue t v) = go (t:ticks) v
  go ticks v = (v, ticks)

-- * Heap variables

-- | A variable bound in the local heap: its human readable name, type (lazy,
-- like that of its binder) and name
data HeapVar (n :: S) = HeapVar !(C.Name C.Term) (Type n) !(TmName n)
  deriving (Show)
type role HeapVar nominal

instance Sinkable HeapVar where
  sinkabilityProof r (HeapVar occ ty x) =
    HeapVar occ (sinkabilityProof r ty) (sinkabilityProof r x)

-- | The variable as a term
heapVarTerm :: HeapVar n -> Term n
heapVarTerm (HeapVar occ ty x) = Var occ ty x
{-# INLINE heapVarTerm #-}

-- | The variable a binder binds
idBinderHeapVar :: DExt n l => IdBinder n l -> HeapVar l
idBinderHeapVar b = HeapVar (idBinderOcc b) (sink (idBinderType b)) (tmNameOf b)
{-# INLINE idBinderHeapVar #-}

-- * Stack

-- | A frame of the stack. See 'Clash.Core.Evaluator.Types.StackFrame'. Every
-- frame lives in the scope of the machine: unlike in an environment machine,
-- frames are never under binders of their own.
data StackFrame (n :: S)
  = UpdateLocal !(TmName n)
  -- ^ Update the local heap binding of the variable with the value
  | UpdateGlobal !GlobalId
  -- ^ Update the global heap binding of the variable with the value
  | Apply !(HeapVar n)
  | Instantiate !(Type n)
  | PrimApply !PrimInfo [Type n] [Value n] [Term n]
  | Scrutinise (Type n) [Alt n]
  -- ^ The type of the alternatives is lazy: 'Clash.Core.Scoped.Evaluator.whnf'
  -- pushes a frame without alternatives, whose type is never used
  | Tickish !(TickInfo n)
  deriving (Show)
type role StackFrame nominal

instance Sinkable StackFrame where
  sinkabilityProof r = \case
    UpdateLocal x -> UpdateLocal (sinkabilityProof r x)
    UpdateGlobal g -> UpdateGlobal g
    Apply x -> Apply (sinkabilityProof r x)
    Instantiate ty -> Instantiate (sinkabilityProof r ty)
    PrimApply p tys vs tms ->
      PrimApply p (map (sinkabilityProof r) tys) (map (sinkabilityProof r) vs)
        (map (sinkabilityProof r) tms)
    Scrutinise ty alts ->
      Scrutinise (sinkabilityProof r ty) (map (sinkabilityProof r) alts)
    Tickish t -> Tickish (sinkabilityProof r t)

-- * Heaps

-- | The heap of the primitives: byte arrays, by number, and the next number.
-- It only holds literals, so it is not scoped, and it is shared with the
-- evaluator of Clash Core ('Clash.Core.Evaluator.Types.PrimHeap').
type PrimHeap = (IntMap C.Term, Int)

-- | The local heap: right-hand sides in scope @n@ for local variables of
-- scope @n@. See the module header.
type PureHeap n = NameMap TmName n (Term n)

-- * Allocated binders

-- | The binders a machine allocated, in the order it allocated them (a snoc
-- list), extending the scope @o@ of the caller to the scope @n@ of the
-- machine
data Alloc (o :: S) (n :: S) where
  ANil :: Alloc o o
  ASnoc :: !(Alloc o i) -> !(IdBinder i n) -> Alloc o n
type role Alloc nominal nominal

-- | The allocated binders as a telescope, e.g. for a @letrec@
allocTele :: Alloc o n -> Tele IdBinder o n
allocTele a0 = go a0 TNil
 where
  go :: Alloc o i -> Tele IdBinder i n -> Tele IdBinder o n
  go ANil acc = acc
  go (ASnoc a b) acc = go a (TCons b acc)

-- | The number of allocated binders
allocLength :: Alloc o n -> Int
allocLength = go 0
 where
  go :: Int -> Alloc o i -> Int
  go !acc ANil = acc
  go !acc (ASnoc a _) = go (acc + 1) a

-- * The machine

-- | The state of the machine, see the module header and
-- 'Clash.Core.Evaluator.Types.Machine'
data Machine (o :: S) (n :: S) = Machine
  { mScope :: !(Scope n)
  -- ^ The scope of the machine, to allocate fresh binders in
  , mAlloc :: !(Alloc o n)
  -- ^ The binders the machine allocated
  , mHeapPrim :: !PrimHeap
  , mHeapGlobalBase :: GlobalEnv (Binding (Term 'VoidS))
  -- ^ The global bindings, closed, moved into the machine's scope when they
  -- are looked up
  , mHeapGlobalOverlay :: !(GlobalEnv (Term n))
  -- ^ Global bindings added or updated during evaluation; shadows
  -- 'mHeapGlobalBase'
  , mHeapGlobalDeleted :: !GlobalSet
  -- ^ Global bindings deleted during evaluation (blackholing); shadows both
  -- maps above
  , mHeapLocal :: !(PureHeap n)
  , mStack :: ![StackFrame n]
  , mTerm :: !(Term n)
  }
type role Machine nominal nominal

-- | A machine that evaluates a term in scope @n@, with a local heap of
-- bindings in that scope, e.g. those of the rewrite context
initMachine
  :: Scope n -> PureHeap n -> GlobalEnv (Binding (Term 'VoidS)) -> PrimHeap -> Term n
  -> Machine n n
initMachine sc h bm ph e = Machine
  { mScope = sc
  , mAlloc = ANil
  , mHeapPrim = ph
  , mHeapGlobalBase = bm
  , mHeapGlobalOverlay = emptyGlobalEnv
  , mHeapGlobalDeleted = emptyGlobalSet
  , mHeapLocal = h
  , mStack = []
  , mTerm = e
  }

-- | Move a machine into the scope extended by a binder it allocated. Its
-- heap binding has to be inserted separately.
extendMachine
  :: DExt n l => Scope l -> IdBinder n l -> Machine o n -> Machine o l
extendMachine sc b m = sinkMachine sc (ASnoc (mAlloc m) b) m
{-# INLINE extendMachine #-}

-- | Move a machine into the scope extended by a telescope of binders it
-- allocated, e.g. those of a @letrec@. Their heap bindings have to be
-- inserted separately.
extendMachineTele
  :: DExt n l => Scope l -> Tele IdBinder n l -> Machine o n -> Machine o l
extendMachineTele sc bs m = sinkMachine sc (snocTele (mAlloc m) bs) m
 where
  snocTele :: Alloc o i -> Tele IdBinder i l -> Alloc o l
  snocTele a TNil = a
  snocTele a (TCons b rest) = snocTele (ASnoc a b) rest

-- | Move a machine into an extended scope, given its allocated binders
-- there. Moving the heaps and the stack is a coercion ('sink1').
sinkMachine :: DExt n l => Scope l -> Alloc o l -> Machine o n -> Machine o l
sinkMachine sc alloc m = Machine
  { mScope = sc
  , mAlloc = alloc
  , mHeapPrim = mHeapPrim m
  , mHeapGlobalBase = mHeapGlobalBase m
  , mHeapGlobalOverlay = sink1 (mHeapGlobalOverlay m)
  , mHeapGlobalDeleted = mHeapGlobalDeleted m
  , mHeapLocal = sink1 (sinkNameMap (mHeapLocal m))
  , mStack = sink1 (mStack m)
  , mTerm = sink (mTerm m)
  }
{-# INLINE sinkMachine #-}

-- | Whether special primitives must be forced: when the term is the subject
-- of a case-expression, or an argument of a primitive. See
-- 'Clash.Core.Evaluator.Types.forcePrims'.
forcePrims :: Machine o n -> Bool
forcePrims = go . mStack
 where
  go (Scrutinise {} : _) = True
  go (PrimApply {} : _) = True
  go (Tickish {} : xs) = go xs
  go (UpdateLocal {} : xs) = go xs
  go (UpdateGlobal {} : xs) = go xs
  go _ = False

-- * Steps

-- | The machine after a step: in an extension of the scope of the machine
-- before it, if the step allocated
data Next (o :: S) (n :: S) where
  Next :: DExt n l => !(Machine o l) -> Next o n

-- | A step that didn't allocate
stay :: Distinct n => Machine o n -> Next o n
stay = Next
{-# INLINE stay #-}

-- | The machine after a step, from the machine after a step of a machine in
-- an extension of the current scope, e.g. one that allocated first
nextFrom :: forall o n i. DExt n i => Next o i -> Next o n
nextFrom (Next (m :: Machine o l)) = withExtTrans @n @i @l (Next m)
{-# INLINE nextFrom #-}

-- * Primitives

-- | The environment of the machine
data Env = Env
  { envTypeEnv :: TypeEnv
  , envPrims :: PrimReductions
  }

-- | The reductions of primitives
data PrimReductions = PrimReductions
  { primReductions :: HashMap Text PrimReduction
  -- ^ By the name of the primitive
  , primFallback :: Maybe PrimReduction
  -- ^ For the primitives without an entry
  }

-- | The reduction of a fully applied primitive. 'Nothing' if it doesn't
-- reduce, e.g. because its arguments are not literals.
newtype PrimReduction = PrimReduction
  { runPrimReduction :: forall o n. Distinct n => PrimContext o n -> Maybe (Next o n) }

-- | The arguments of a primitive reduction. See
-- @Clash.GHC.Evaluator.Primitive.Util.PrimStepContext@.
data PrimContext (o :: S) (n :: S) = PrimContext
  { pcEnv :: Env
  , pcIsSubj :: Bool
  -- ^ Whether special primitives must be forced, see 'forcePrims'
  , pcInfo :: PrimInfo
  , pcTys :: [Type n]
  , pcArgs :: [Value n]
  , pcMachine :: Machine o n
  }
