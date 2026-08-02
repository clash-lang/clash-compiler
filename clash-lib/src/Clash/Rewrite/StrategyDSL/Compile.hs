{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Compile a "Clash.Rewrite.StrategyDSL" strategy into executable dispatch
  code, at runtime. 'compile' produces the fused fast path: one constructor
  match per node drives both the dispatch of the node program and the
  descent into the node's children. 'compileReference' produces an obviously
  correct unfused equivalent from the same spec, built from the
  "Clash.Rewrite.Combinators" traversals; the two agree on results,
  transformation counts, and change flags, so the reference serves as the
  test oracle for the compiler.

  See Note [staged compilation], Note [chain semantics], and
  Note [NonRec erasure during descent].
-}

module Clash.Rewrite.StrategyDSL.Compile
  ( compile
  , compileReference
  , dispatch
  , asRewrite
  , validateStrat
  ) where

import qualified Control.Monad.Writer as Writer
import Data.List (tails)
import Data.Maybe (isJust)
import qualified Data.Monoid as Monoid

import Clash.Core.Term
  (Bind (..), CoreContext (..), Term (..), bindToList, patIds, primArg)
import Clash.Core.VarEnv (extendInScopeSet, extendInScopeSetList)
import qualified Clash.Rewrite.Combinators as Comb
import Clash.Rewrite.StrategyDSL
import Clash.Rewrite.Types (Rewrite, RewriteMonad, TransformContext (..))
import Clash.Rewrite.Util (apply, applyWith)

{-
Note [staged compilation]
~~~~~~~~~~~~~~~~~~~~~~~~~
'compile' is an interpreter staged by hand: every case analysis on the
'Strat'/'Step'/'TransformSpec' structure happens while the strategy value is
built — once, since the compiled strategies are top-level CAFs — and the
result is a tree of closures. Per-node execution only enters closures; it
never inspects the DSL structure.

When modifying this module, keep that discipline: anything derived from the
spec (bucket lists, guard compositions, per-shape tables) must be bound
outside the lambda that receives the context and the term. A binding moved
inside such a lambda silently turns compilation into per-node
interpretation; the symptom is an allocation increase on big designs, not a
test failure.

Note [chain semantics]
~~~~~~~~~~~~~~~~~~~~~~
A 'Chain' compiles, per constructor, to the members registered for that
constructor (the constructor's "bucket"), run in list order. Members
registered only for other constructors do not run at this node, even when a
mid-chain rewrite gives the node their constructor; they wait for the
traversal's next dispatch.

Two contracts make the compiled bucket code equal to the sequential
@apply n1 g1 >-> apply n2 g2 >-> …@ chain over the bucket members, where
each @gI@ guards member I on its full entry table:

1. /Unchanged means untouched./ A worker that does not signal change must
   return the node it was given, unmodified. This is a pre-existing engine
   invariant ("Expression changed without notice" in
   'Clash.Rewrite.Util.applyDebug'). It lets the next bucket member run on
   the constructor's original node without a change of shape.

2. /Change re-guards the rest of the bucket./ After a member signals change
   the term may have any constructor, so the remaining bucket members run
   through their full entry tables (the "guarded suffix") — exactly what the
   pattern matches of the sequential chain did.

The fast path and the reference path differ only in which member attempts
are wrapped in 'applyWith': the fast path skips members whose entry table
cannot match the node (they would be no-ops), so with
@-fclash-debug-transformations@ it prints fewer "Trying:" lines. Results,
transformation counters, and change flags are identical.
-}

--------------------------------------------------------------------------------
-- Per-shape node programs
--------------------------------------------------------------------------------

-- | A compiled 'Step': the node program per 'Term' constructor. 'Nothing'
-- means the program is the identity at that constructor, which lets the
-- traversals skip the change-listening machinery there entirely.
data StepProg extra = StepProg
  { spVar     :: !(Maybe (Rewrite extra))
  , spData    :: !(Maybe (Rewrite extra))
  , spLiteral :: !(Maybe (Rewrite extra))
  , spPrim    :: !(Maybe (Rewrite extra))
  , spLam     :: !(Maybe (Rewrite extra))
  , spTyLam   :: !(Maybe (Rewrite extra))
  , spApp     :: !(Maybe (Rewrite extra))
  , spTyApp   :: !(Maybe (Rewrite extra))
  , spLet     :: !(Maybe (Rewrite extra))
  , spCase    :: !(Maybe (Rewrite extra))
  , spCast    :: !(Maybe (Rewrite extra))
  , spTick    :: !(Maybe (Rewrite extra))
  }

-- | Build a program table; the strict fields force the per-shape analysis
-- when the table is built, per Note [staged compilation].
mkProg :: (Shape -> Maybe (Rewrite extra)) -> StepProg extra
mkProg f = StepProg
  { spVar     = f SVar
  , spData    = f SData
  , spLiteral = f SLiteral
  , spPrim    = f SPrim
  , spLam     = f SLam
  , spTyLam   = f STyLam
  , spApp     = f SApp
  , spTyApp   = f STyApp
  , spLet     = f SLet
  , spCase    = f SCase
  , spCast    = f SCast
  , spTick    = f STick
  }

-- | The table's program at a shape; strategy-construction time only.
progAt :: StepProg extra -> Shape -> Maybe (Rewrite extra)
progAt prog shape = case shape of
  SVar     -> spVar prog
  SData    -> spData prog
  SLiteral -> spLiteral prog
  SPrim    -> spPrim prog
  SLam     -> spLam prog
  STyLam   -> spTyLam prog
  SApp     -> spApp prog
  STyApp   -> spTyApp prog
  SLet     -> spLet prog
  SCase    -> spCase prog
  SCast    -> spCast prog
  STick    -> spTick prog

-- | The table's program for a node's constructor; the per-node lookup.
{-# INLINE progFor #-}
progFor :: StepProg extra -> Term -> Maybe (Rewrite extra)
progFor prog tm = case tm of
  Var{}     -> spVar prog
  Data{}    -> spData prog
  Literal{} -> spLiteral prog
  Prim{}    -> spPrim prog
  Lam{}     -> spLam prog
  TyLam{}   -> spTyLam prog
  App{}     -> spApp prog
  TyApp{}   -> spTyApp prog
  Let{}     -> spLet prog
  Case{}    -> spCase prog
  Cast{}    -> spCast prog
  Tick{}    -> spTick prog

-- | Run a program table on a term of unknown constructor.
progDispatch :: StepProg extra -> Rewrite extra
progDispatch prog = \ctx tm -> case progFor prog tm of
  Nothing -> pure tm
  Just p  -> p ctx tm

--------------------------------------------------------------------------------
-- Entries and transformation specs
--------------------------------------------------------------------------------

-- | An entry's invocation on a node: match the entry, extract the node's
-- fields, call the worker. INLINE is load-bearing at the 'mkBucket' call
-- site: it compiles the entry dispatch and field extraction into the bucket
-- closure's own body, so a member attempt crosses only the worker's closure
-- boundary — ticky showed the intermediate wrapper closure at ~160 bytes
-- per attempt across ~157M attempts on wireDemoTest.
applyEntry :: Entry extra -> TransformContext -> Term -> RewriteMonad extra Term
applyEntry entry ctx tm = case entry of
  OnVar f -> case tm of
    Var i -> f ctx tm i
    _ -> pure tm
  OnData f -> case tm of
    Data dc -> f ctx tm dc
    _ -> pure tm
  OnLiteral f -> case tm of
    Literal l -> f ctx tm l
    _ -> pure tm
  OnPrim f -> case tm of
    Prim p -> f ctx tm p
    _ -> pure tm
  OnLam f -> case tm of
    Lam v e -> f ctx tm v e
    _ -> pure tm
  OnTyLam f -> case tm of
    TyLam tv e -> f ctx tm tv e
    _ -> pure tm
  OnApp f -> case tm of
    App fun arg -> f ctx tm fun arg
    _ -> pure tm
  OnTyApp f -> case tm of
    TyApp fun argTy -> f ctx tm fun argTy
    _ -> pure tm
  OnLet f -> case tm of
    Let bnd body -> f ctx tm bnd body
    _ -> pure tm
  OnCase f -> case tm of
    Case subj altsTy alts -> f ctx tm subj altsTy alts
    _ -> pure tm
  OnCast f -> case tm of
    Cast e fromTy toTy -> f ctx tm e fromTy toTy
    _ -> pure tm
  OnTick f -> case tm of
    Tick tick e -> f ctx tm tick e
    _ -> pure tm
  NodeEntry _ r -> r ctx tm
  AnyShapeEntry r -> r ctx tm
{-# INLINE applyEntry #-}

-- | The entry a transformation registers at a shape, if any.
entryAt :: Shape -> [Entry extra] -> Maybe (Entry extra)
entryAt shape = go
 where
  go [] = Nothing
  go (entry : rest) = case entryShape entry of
    Just shape'
      | shape' == shape -> Just entry
      | otherwise -> go rest
    Nothing -> Just entry

-- | A transformation's entry table as a program table: its worker at every
-- shape it registers for, uninstrumented.
specProg :: TransformSpec extra -> StepProg extra
specProg spec = mkProg (\shape -> applyEntry <$> entryAt shape (specEntries spec))

-- | A transformation as a plain uninstrumented rewrite: match the term
-- against the entry table, run the worker on a match, return the term
-- untouched otherwise. Use where a transformation is invoked from inside
-- another transformation (any instrumentation is the caller's); bind the
-- result once rather than rebuilding it per call.
asRewrite :: TransformSpec extra -> Rewrite extra
asRewrite spec = progDispatch (specProg spec)

-- | One member's guard: match the term against the member's full entry
-- table; run the member instrumented on a match, return the term untouched
-- (without instrumentation) otherwise.
memberGuard :: String -> StepProg extra -> Rewrite extra
memberGuard name table = \ctx tm -> case progFor table tm of
  Nothing -> pure tm
  Just invoke -> fst <$> applyWith name ctx tm (invoke ctx tm)

--------------------------------------------------------------------------------
-- Chains
--------------------------------------------------------------------------------

-- | The chain-level validation shared by 'compile' (as an error) and
-- 'validateStrat' (as a result).
chainError :: [TransformSpec extra] -> Maybe String
chainError members
  | null members
  = Just "empty chain"
  | name : _ <- [n | n : rest <- tails (map specName members), n `elem` rest]
  = Just ("duplicate transformation name in one chain: " <> show name
          <> " (alias one occurrence with 'named')")
  | otherwise
  = Nothing

validateChainMembers :: [TransformSpec extra] -> [TransformSpec extra]
validateChainMembers members = case chainError members of
  Just err -> error ("Clash.Rewrite.StrategyDSL.Compile: " <> err)
  Nothing -> members

-- | Compile a chain to its per-shape buckets. See Note [chain semantics].
compileChain :: [TransformSpec extra] -> StepProg extra
compileChain members0 = mkProg bucketProg
 where
  members = validateChainMembers members0
  annotated =
    [ (specName m, specEntries m, memberGuard (specName m) (specProg m))
    | m <- members
    ]

  bucketProg shape = case bucketAt shape of
    [] -> Nothing
    bucket -> Just (mkBucket bucket)

  -- The shape's bucket: each member's entry at this shape, paired with
  -- the composed guards of the bucket members after it (the member's
  -- guarded suffix).
  bucketAt shape =
    [ (name, entry, composeGuards [g | (_, es', g) <- rest, isJust (entryAt shape es')])
    | (name, es, _guard) : rest <- tails annotated
    , Just entry <- [entryAt shape es]
    ]

-- | A bucket: run the members in order on the node; after the first change,
-- hand the term to the member's guarded suffix. All list analysis happens
-- here, before the term arrives; see Note [staged compilation].
mkBucket :: [(String, Entry extra, Rewrite extra)] -> Rewrite extra
mkBucket [] = \_ctx tm -> pure tm
mkBucket ((name, entry, suffix) : rest) =
  let restK = case rest of
        [] -> Nothing
        _ -> Just (mkBucket rest)
  in \ctx tm -> do
       (t, changedFlag) <- applyWith name ctx tm (applyEntry entry ctx tm)
       if changedFlag
         then suffix ctx t
         else case restK of
           Nothing -> pure t
           Just k -> k ctx t

-- | @g1 ctx t >>= g2 ctx >>= …@: offer the term to each remaining bucket
-- member through its full entry table.
composeGuards :: [Rewrite extra] -> Rewrite extra
composeGuards [] = \_ctx tm -> pure tm
composeGuards (g0 : gs0) = go g0 gs0
 where
  go g [] = g
  go g (g' : gs) = let k = go g' gs in \ctx tm -> g ctx tm >>= k ctx

--------------------------------------------------------------------------------
-- Node programs ('Step')
--------------------------------------------------------------------------------

compileStep :: Step extra -> StepProg extra
compileStep step = case step of
  Chain members -> compileChain members

  SeqS a b ->
    let pa = compileStep a
        pb = compileStep b
        dispatchB = progDispatch pb
    in mkProg $ \shape -> case (progAt pa shape, progAt pb shape) of
         (Nothing, kb) -> kb
         (Just ka, Nothing) -> Just $ \ctx tm -> do
           (t, w) <- Writer.listen (ka ctx tm)
           if Monoid.getAny w then dispatchB ctx t else pure t
         (Just ka, Just kb) -> Just $ \ctx tm -> do
           (t, w) <- Writer.listen (ka ctx tm)
           -- When the left side did not signal change the term is untouched
           -- (Note [chain semantics], contract 1), so the right side's
           -- program for this same shape still applies.
           if Monoid.getAny w then dispatchB ctx t else kb ctx t

  OnChangeS a b ->
    let pa = compileStep a
        dispatchB = progDispatch (compileStep b)
    in mkProg $ \shape -> case progAt pa shape of
         Nothing -> Nothing
         Just ka -> Just $ \ctx tm -> do
           (t, w) <- Writer.listen (ka ctx tm)
           if Monoid.getAny w then dispatchB ctx t else pure t

  OnNoChangeS a b ->
    let pa = compileStep a
        pb = compileStep b
    in mkProg $ \shape -> case (progAt pa shape, progAt pb shape) of
         (Nothing, kb) -> kb
         (Just ka, Nothing) -> Just ka
         (Just ka, Just kb) -> Just $ \ctx tm -> do
           (t, w) <- Writer.listen (ka ctx tm)
           if Monoid.getAny w then pure t else kb ctx t

  EmbedS strat ->
    let embedded = compile strat
    in mkProg (\_shape -> Just embedded)

-- | A node program as a plain rewrite: match the constructor once and run
-- that constructor's program, without traversing. For composing a 'Step'
-- with plain combinators, and for testing chains in isolation.
dispatch :: Step extra -> Rewrite extra
dispatch step = progDispatch (compileStep step)

--------------------------------------------------------------------------------
-- Descent (the congruence step of the fused traversals)
--------------------------------------------------------------------------------

{-
Note [NonRec erasure during descent]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'Clash.Rewrite.Combinators.allR' matches let-expressions through the 'Letrec'
pattern synonym and rebuilds them with 'Rec' bindings, so any 'NonRec' let a
traversal descends into comes back as a 'Rec' let. 'descend' replicates that
behavior deliberately: preserving 'NonRec' is a real behavior change
(downstream code branches on 'Rec'/'NonRec', and netlist rendering order can
shift) that must be validated on its own, separately from this dispatch
machinery.
-}

-- | The one-level descent, replicating 'Clash.Rewrite.Combinators.allR'
-- exactly: same 'CoreContext' pushes, same 'InScopeSet' extensions, and the
-- argument's context computed from the already-rewritten function. A copy
-- rather than a call because 'allR' is polymorphic in its monad (it is also
-- used at 'Control.Monad.State.StateT' transformer stacks) while the hot
-- traversals want the statically known 'RewriteMonad' bind.
--
-- INLINE is load-bearing: 'descend' is not self-recursive (it recurses
-- through its @go@ argument), and inlining it into each fused traversal's
-- local @go@ lets GHC's worker\/wrapper pass the 'TransformContext' fields
-- unboxed around the whole traversal loop, as the members' strictness
-- signatures demand. Left out-of-line, every child visit allocates a fresh
-- 'TransformContext' just to hand it to the unknown @go@ — measured as ~11%
-- extra total allocation on wireDemoTest.
{-# INLINE descend #-}
descend :: Rewrite extra -> Rewrite extra
descend go (TransformContext is c) tm = case tm of
  Lam v e ->
    Lam v <$> go (TransformContext (extendInScopeSet is v) (LamBody v : c)) e

  TyLam tv e ->
    TyLam tv <$> go (TransformContext (extendInScopeSet is tv) (TyLamBody tv : c)) e

  App fun arg -> do
    fun' <- go (TransformContext is (AppFun : c)) fun
    -- The argument's context is computed from the already rewritten
    -- function; the order is load-bearing.
    arg' <- go (TransformContext is (AppArg (primArg fun') : c)) arg
    pure (App fun' arg')

  TyApp fun argTy ->
    (`TyApp` argTy) <$> go (TransformContext is (TyAppC : c)) fun

  -- See Note [NonRec erasure during descent]
  Let bnd body -> do
    let bs = bindToList bnd
        bndrs = map fst bs
        is' = extendInScopeSetList is bndrs
    bs' <- traverse
             (\(b, rhs) -> (,) b <$> go (TransformContext is' (LetBinding b bndrs : c)) rhs)
             bs
    body' <- go (TransformContext is' (LetBody bs : c)) body
    pure (Let (Rec bs') body')

  Case subj altsTy alts -> do
    subj' <- go (TransformContext is (CaseScrut : c)) subj
    alts' <- traverse
               (\(p, e) ->
                  let (tvs, ids) = patIds p
                      isAlt = extendInScopeSetList (extendInScopeSetList is tvs) ids
                  in (,) p <$> go (TransformContext isAlt (CaseAlt p : c)) e)
               alts
    pure (Case subj' altsTy alts')

  Cast e fromTy toTy ->
    (\e' -> Cast e' fromTy toTy) <$> go (TransformContext is (CastBody : c)) e

  Tick tick e ->
    Tick tick <$> go (TransformContext is (TickC tick : c)) e

  -- Var, Data, Literal, Prim: no subterms.
  _ -> pure tm

--------------------------------------------------------------------------------
-- Fused traversals
--------------------------------------------------------------------------------

-- The recursion structure of each traversal is pinned to its
-- "Clash.Rewrite.Combinators" counterpart, with the node program as @r@; a
-- shape without a program skips the change-listening machinery and goes
-- straight to the descent (or the leaf return).

-- | 'Comb.topdownR': repeat the program at each node until it no longer
-- fires, then descend. See Note [topdown repeatR] in
-- "Clash.Rewrite.Combinators".
fusedTopDown :: StepProg extra -> Rewrite extra
fusedTopDown prog = go
 where
  go ctx tm = case progFor prog tm of
    Nothing -> descend go ctx tm
    Just p -> do
      (t, w) <- Writer.listen (p ctx tm)
      if Monoid.getAny w then go ctx t else descend go ctx tm

-- | 'Comb.topdownSucR': run the program once; when it fired, stop — neither
-- repeating nor descending below it.
fusedTopDownSuc :: StepProg extra -> Rewrite extra
fusedTopDownSuc prog = go
 where
  go ctx tm = case progFor prog tm of
    Nothing -> descend go ctx tm
    Just p -> do
      (t, w) <- Writer.listen (p ctx tm)
      if Monoid.getAny w then pure t else descend go ctx tm

-- | 'Comb.bottomupR': descend first, run the program once on the rebuilt
-- node.
fusedBottomUp :: StepProg extra -> Rewrite extra
fusedBottomUp prog = go
 where
  go ctx tm = do
    tm' <- descend go ctx tm
    case progFor prog tm' of
      Nothing -> pure tm'
      Just p -> p ctx tm'

-- | 'Comb.innerMost': bottom-up; when the program fires, re-traverse the
-- result until the innermost fixpoint is reached.
fusedInnerMost :: StepProg extra -> Rewrite extra
fusedInnerMost prog = go
 where
  go ctx tm = do
    tm' <- descend go ctx tm
    case progFor prog tm' of
      Nothing -> pure tm'
      Just p -> do
        (t, w) <- Writer.listen (p ctx tm')
        if Monoid.getAny w then go ctx t else pure t

-- | 'Comb.topdownFixR': a 'settle' loop (@'Comb.repeatR' r@) plus the
-- tryParent\/childChanged\/parentChanged loop from Note [topdownFixR] in
-- "Clash.Rewrite.Combinators".
fusedTopDownFix :: StepProg extra -> Rewrite extra
fusedTopDownFix prog = goFix True
 where
  settle ctx tm = case progFor prog tm of
    Nothing -> pure tm
    Just p -> do
      (t, w) <- Writer.listen (p ctx tm)
      if Monoid.getAny w then settle ctx t else pure t

  goFix tryParent ctx tm = do
    t1 <- if tryParent then settle ctx tm else pure tm
    (t2, childChanged) <- Writer.listen (descend (goFix True) ctx t1)
    if Monoid.getAny childChanged
      then do
        (t3, parentChanged) <- Writer.listen (settle ctx t2)
        if Monoid.getAny parentChanged
          then goFix False ctx t3
          else pure t3
      else pure t2

--------------------------------------------------------------------------------
-- Strategies
--------------------------------------------------------------------------------

-- | Compile a strategy into its fused dispatch code. Compilation happens
-- when the result is built, not when it runs; see Note [staged compilation].
compile :: Strat extra -> Rewrite extra
compile strat = case strat of
  Seq a b -> compile a Comb.>-> compile b
  DeepSeqS a b -> compile a Comb.>-!-> compile b
  OnChange a b -> compile a Comb.!-> compile b
  OnNoChange a b -> compile a Comb.>-! compile b
  Repeat a -> Comb.repeatR (compile a)
  Pass name rewrite -> apply name rewrite
  CallStrategy rewrite -> rewrite
  TopDown step -> fusedTopDown (compileStep step)
  BottomUp step -> fusedBottomUp (compileStep step)
  TopDownFix step -> fusedTopDownFix (compileStep step)
  TopDownSuc step -> fusedTopDownSuc (compileStep step)
  InnerMost step -> fusedInnerMost (compileStep step)

-- | Compile a strategy into its unfused reference: the same node programs,
-- run through the "Clash.Rewrite.Combinators" traversals. Agrees with
-- 'compile' on results, transformation counts, and change flags; see
-- Note [chain semantics] for the only observable difference.
compileReference :: Strat extra -> Rewrite extra
compileReference = goStrat
 where
  goStrat strat = case strat of
    Seq a b -> goStrat a Comb.>-> goStrat b
    DeepSeqS a b -> goStrat a Comb.>-!-> goStrat b
    OnChange a b -> goStrat a Comb.!-> goStrat b
    OnNoChange a b -> goStrat a Comb.>-! goStrat b
    Repeat a -> Comb.repeatR (goStrat a)
    Pass name rewrite -> apply name rewrite
    CallStrategy rewrite -> rewrite
    TopDown step -> Comb.topdownR (goStep step)
    BottomUp step -> Comb.bottomupR (goStep step)
    TopDownFix step -> Comb.topdownFixR (goStep step)
    TopDownSuc step -> Comb.topdownSucR (goStep step)
    InnerMost step -> Comb.innerMost (goStep step)

  goStep step = case step of
    SeqS a b -> goStep a Comb.>-> goStep b
    OnChangeS a b -> goStep a Comb.!-> goStep b
    OnNoChangeS a b -> goStep a Comb.>-! goStep b
    EmbedS strat -> goStrat strat
    Chain members0 ->
      -- Per constructor, the sequential apply chain over that constructor's
      -- bucket, each member guarded on its full entry table. See
      -- Note [chain semantics].
      let members = validateChainMembers members0
          applied = [(m, apply (specName m) (asRewrite m)) | m <- members]
          table = mkProg $ \shape ->
            case [r | (m, r) <- applied, isJust (entryAt shape (specEntries m))] of
              [] -> Nothing
              bucket -> Just (foldr1 (Comb.>->) bucket)
      in progDispatch table

-- | Check the properties 'compile' would otherwise fail on while building
-- the closure tree: chain-level rules ('chainError') for every chain in the
-- strategy. Entry-table rules are checked by
-- 'Clash.Rewrite.StrategyDSL.transform' when a spec is forced, which this
-- walk also does. Strategy specs should be covered by a unit test calling
-- this, restoring the build-time validation that Template Haskell
-- compilation used to provide.
validateStrat :: Strat extra -> Either String ()
validateStrat = goStrat
 where
  goStrat strat = case strat of
    Seq a b -> goStrat a >> goStrat b
    DeepSeqS a b -> goStrat a >> goStrat b
    OnChange a b -> goStrat a >> goStrat b
    OnNoChange a b -> goStrat a >> goStrat b
    Repeat a -> goStrat a
    Pass name _ -> name `seq` Right ()
    CallStrategy _ -> Right ()
    TopDown step -> goStep step
    BottomUp step -> goStep step
    TopDownFix step -> goStep step
    TopDownSuc step -> goStep step
    InnerMost step -> goStep step

  goStep step = case step of
    Chain members -> case chainError members of
      Just err -> Left err
      Nothing ->
        -- Force every member's entry table, surfacing 'transform' errors.
        foldr (\m rest -> length (specEntries m) `seq` rest) (Right ()) members
    SeqS a b -> goStep a >> goStep b
    OnChangeS a b -> goStep a >> goStep b
    OnNoChangeS a b -> goStep a >> goStep b
    EmbedS strat -> goStrat strat
