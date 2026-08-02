{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  A pure description language for rewrite strategies. A strategy written in
  this DSL is compiled — once, when the strategy value is built, by
  "Clash.Rewrite.StrategyDSL.Compile" — into a fused traversal that matches
  each node's constructor once and runs only the transformations registered
  for that shape.

  The DSL has two layers:

  * 'Step': what runs at a single node inside a fused traversal — an ordered
    'chain' of transformations, sequencing and success/failure conditionals
    between chain segments, or a whole 'nested' strategy embedded as one node
    action.

  * 'Strat': a complete strategy — fused traversals over a 'Step', whole-term
    passes, and the usual combinators ('>->', '!->', '>-!', '>-!->',
    'repeatR') between them.

  Transformations enter the DSL as 'TransformSpec' values: a canonical debug
  name plus, per 'Term' constructor the transformation can fire on, a worker
  function receiving that constructor's fields. Because each entry
  constructor fixes its worker's field types, declaring a wrong shape is a
  type error at the declaration site — entry shapes cannot silently drift
  from a transformation's implementation.
-}

{-# LANGUAGE FlexibleInstances #-}

module Clash.Rewrite.StrategyDSL
  ( -- * Transformation metadata
    Shape (..)
  , Entry (..)
  , entryShape
  , TransformSpec (..)
  , transform
  , anyShape
  , named
    -- ** Entry builders: workers receiving the constructor's fields
  , onVar
  , onData
  , onLiteral
  , onPrim
  , onLam
  , onTyLam
  , onApp
  , onTyApp
  , onLet
  , onCase
  , onCast
  , onTick
    -- ** Entry builders: workers receiving the node itself
    -- $nodeEntries
  , onVarNode
  , onPrimNode
  , onAppNode
  , onTyAppNode
  , onLetNode
  , onTickNode
    -- * Node layer
  , Step (..)
  , chain
  , one
  , nested
    -- * Strategy layer
  , Strat (..)
  , topdown
  , bottomup
  , topdownFix
  , topdownSuc
  , innerMost
  , pass
  , callStrategy
  , repeatR
    -- * Combinators
  , Strategic (..)
  , (>-!->)
  , ToStep (..)
  ) where

import Clash.Core.DataCon (DataCon)
import Clash.Core.Literal (Literal)
import Clash.Core.Term (Alt, Bind, PrimInfo, Term, TickInfo)
import Clash.Core.Type (Type)
import Clash.Core.Var (Id, TyVar)
import Clash.Rewrite.Types (Rewrite, RewriteMonad, TransformContext)

-- | One 'Clash.Core.Term.Term' constructor, as an entry shape.
data Shape
  = SVar | SData | SLiteral | SPrim | SLam | STyLam
  | SApp | STyApp | SLet | SCase | SCast | STick
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | How a transformation is entered at one constructor. The per-shape
-- constructors carry a worker that receives the matched node (to return on
-- the unchanged path with sharing intact) followed by the constructor's
-- fields; the dispatch code built by "Clash.Rewrite.StrategyDSL.Compile"
-- only invokes a worker on a node of its constructor.
data Entry extra
  = OnVar     !(TransformContext -> Term -> Id -> RewriteMonad extra Term)
  | OnData    !(TransformContext -> Term -> DataCon -> RewriteMonad extra Term)
  | OnLiteral !(TransformContext -> Term -> Literal -> RewriteMonad extra Term)
  | OnPrim    !(TransformContext -> Term -> PrimInfo -> RewriteMonad extra Term)
  | OnLam     !(TransformContext -> Term -> Id -> Term -> RewriteMonad extra Term)
  | OnTyLam   !(TransformContext -> Term -> TyVar -> Term -> RewriteMonad extra Term)
  | OnApp     !(TransformContext -> Term -> Term -> Term -> RewriteMonad extra Term)
  | OnTyApp   !(TransformContext -> Term -> Term -> Type -> RewriteMonad extra Term)
  | OnLet     !(TransformContext -> Term -> Bind Term -> Term -> RewriteMonad extra Term)
  | OnCase    !(TransformContext -> Term -> Term -> Type -> [Alt] -> RewriteMonad extra Term)
  | OnCast    !(TransformContext -> Term -> Term -> Type -> Type -> RewriteMonad extra Term)
  | OnTick    !(TransformContext -> Term -> TickInfo -> Term -> RewriteMonad extra Term)
  | NodeEntry !Shape !(Rewrite extra)
  -- ^ A worker applied to the context and the node only, registered at
  -- exactly this constructor. For transformations that operate on the node
  -- itself, such as application-spine collectors.
  | AnyShapeEntry !(Rewrite extra)
  -- ^ A worker applied to the context and the node only, offered at every
  -- node. Escape hatch for a transformation whose firing condition lives in
  -- the 'TransformContext' rather than the constructor (@topLet@). Mutually
  -- exclusive with the other entries.

-- | The constructor an entry is registered at; 'Nothing' for
-- 'AnyShapeEntry'.
entryShape :: Entry extra -> Maybe Shape
entryShape entry = case entry of
  OnVar{}     -> Just SVar
  OnData{}    -> Just SData
  OnLiteral{} -> Just SLiteral
  OnPrim{}    -> Just SPrim
  OnLam{}     -> Just SLam
  OnTyLam{}   -> Just STyLam
  OnApp{}     -> Just SApp
  OnTyApp{}   -> Just STyApp
  OnLet{}     -> Just SLet
  OnCase{}    -> Just SCase
  OnCast{}    -> Just SCast
  OnTick{}    -> Just STick
  NodeEntry shape _ -> Just shape
  AnyShapeEntry{} -> Nothing

-- | Pure metadata for one transformation: its canonical name (used for debug
-- tracing, transformation counters, and the rewrite history) and its entry
-- table. Construct with 'transform' or 'anyShape', which validate the entry
-- table.
data TransformSpec extra = TransformSpec
  { specName :: !String
  , specEntries :: ![Entry extra]
  }

-- | Declare a transformation's entry shapes. The entry list must be
-- non-empty, may register at most one entry per constructor, and may not mix
-- 'AnyShapeEntry' with per-constructor entries: a transformation is either
-- shape-agnostic or shaped, never both.
transform :: String -> [Entry extra] -> TransformSpec extra
transform name entries
  | null entries
  = error ("transform: transformation " <> name <> " declares no entry shapes")
  | any isAnyShape entries, any (not . isAnyShape) entries
  = error ("transform: " <> name
           <> ": AnyShapeEntry is mutually exclusive with per-constructor entries")
  | length (filter isAnyShape entries) > 1
  = error ("transform: " <> name <> ": duplicate AnyShapeEntry")
  | Just shape <- firstDuplicateShape entries
  = error ("transform: " <> name <> ": duplicate entry for " <> show shape)
  | otherwise
  = TransformSpec name entries
 where
  isAnyShape AnyShapeEntry{} = True
  isAnyShape _ = False

  firstDuplicateShape = go []
   where
    go _ [] = Nothing
    go seen (entry : rest) = case entryShape entry of
      Just shape
        | shape `elem` seen -> Just shape
        | otherwise -> go (shape : seen) rest
      Nothing -> go seen rest

-- | Declare a shape-agnostic transformation. See 'AnyShapeEntry'.
anyShape :: String -> Rewrite extra -> TransformSpec extra
anyShape name worker = TransformSpec name [AnyShapeEntry worker]

-- | Give a transformation a different name; used for deliberate debug
-- aliases of a transformation that runs at several strategy positions (the
-- transformation counters and rewrite history key on the name).
named :: String -> TransformSpec extra -> TransformSpec extra
named alias spec = spec { specName = alias }

onVar :: (TransformContext -> Term -> Id -> RewriteMonad extra Term) -> [Entry extra]
onVar = pure . OnVar

onData :: (TransformContext -> Term -> DataCon -> RewriteMonad extra Term) -> [Entry extra]
onData = pure . OnData

onLiteral :: (TransformContext -> Term -> Literal -> RewriteMonad extra Term) -> [Entry extra]
onLiteral = pure . OnLiteral

onPrim :: (TransformContext -> Term -> PrimInfo -> RewriteMonad extra Term) -> [Entry extra]
onPrim = pure . OnPrim

onLam :: (TransformContext -> Term -> Id -> Term -> RewriteMonad extra Term) -> [Entry extra]
onLam = pure . OnLam

onTyLam :: (TransformContext -> Term -> TyVar -> Term -> RewriteMonad extra Term) -> [Entry extra]
onTyLam = pure . OnTyLam

onApp :: (TransformContext -> Term -> Term -> Term -> RewriteMonad extra Term) -> [Entry extra]
onApp = pure . OnApp

onTyApp :: (TransformContext -> Term -> Term -> Type -> RewriteMonad extra Term) -> [Entry extra]
onTyApp = pure . OnTyApp

onLet :: (TransformContext -> Term -> Bind Term -> Term -> RewriteMonad extra Term) -> [Entry extra]
onLet = pure . OnLet

onCase :: (TransformContext -> Term -> Term -> Type -> [Alt] -> RewriteMonad extra Term) -> [Entry extra]
onCase = pure . OnCase

onCast :: (TransformContext -> Term -> Term -> Type -> Type -> RewriteMonad extra Term) -> [Entry extra]
onCast = pure . OnCast

onTick :: (TransformContext -> Term -> TickInfo -> Term -> RewriteMonad extra Term) -> [Entry extra]
onTick = pure . OnTick

-- $nodeEntries
-- Only the constructors an application spine can start with, or that spine
-- transformations otherwise register at, have @on*Node@ builders; extend the
-- list when a transformation needs another one.

onVarNode, onPrimNode, onAppNode, onTyAppNode, onLetNode,
  onTickNode :: Rewrite extra -> [Entry extra]
onVarNode   = pure . NodeEntry SVar
onPrimNode  = pure . NodeEntry SPrim
onAppNode   = pure . NodeEntry SApp
onTyAppNode = pure . NodeEntry STyApp
onLetNode   = pure . NodeEntry SLet
onTickNode  = pure . NodeEntry STick

-- | What runs at a single node inside a fused traversal.
data Step extra
  = Chain [TransformSpec extra]
  -- ^ One constructor match dispatches the members registered for that
  -- shape, in list order. Equivalent to the sequential
  -- @apply n1 t1 '>->' apply n2 t2 …@ chain over those members: after a
  -- member changes the term, the remaining members re-guard on the changed
  -- term through their full entry tables (the /guarded suffix/); members
  -- registered only for other constructors wait for the traversal's next
  -- dispatch.
  | SeqS (Step extra) (Step extra)
  -- ^ '>->' at the node: the right side re-dispatches on the left side's
  -- result.
  | OnChangeS (Step extra) (Step extra)
  -- ^ '!->' at the node: the right side runs only when the left side
  -- signaled change.
  | OnNoChangeS (Step extra) (Step extra)
  -- ^ '>-!' at the node: the right side runs only when the left side did
  -- not signal change.
  | EmbedS (Strat extra)
  -- ^ A whole strategy as one node action.

-- | A complete rewriting strategy.
data Strat extra
  = TopDown (Step extra)
  -- ^ Fused 'Clash.Rewrite.Combinators.topdownR': repeat the step at each
  -- node until it no longer fires, then descend.
  | BottomUp (Step extra)
  -- ^ Fused 'Clash.Rewrite.Combinators.bottomupR': descend first, run the
  -- step once on the rebuilt node.
  | TopDownFix (Step extra)
  -- ^ Fused 'Clash.Rewrite.Combinators.topdownFixR'. Mind its soundness
  -- precondition (Note [topdownFixR] in "Clash.Rewrite.Combinators").
  | TopDownSuc (Step extra)
  -- ^ Fused 'Clash.Rewrite.Combinators.topdownSucR': stop at the first
  -- success, neither repeating nor descending below it.
  | InnerMost (Step extra)
  -- ^ Fused 'Clash.Rewrite.Combinators.innerMost': bottom-up; when the step
  -- fires, re-traverse the result until the innermost fixpoint is reached.
  | Pass String (Rewrite extra)
  -- ^ A whole-term pass: @apply \<name\> \<rewrite\>@, for transformations
  -- that traverse the term themselves (@makeANF@, @etaExpansionTL@, …).
  | CallStrategy (Rewrite extra)
  -- ^ An already-compiled strategy embedded as-is, typically the compiled
  -- binding of another strategy spec.
  | Seq (Strat extra) (Strat extra)            -- ^ '>->'
  | DeepSeqS (Strat extra) (Strat extra)       -- ^ '>-!->'
  | OnChange (Strat extra) (Strat extra)       -- ^ '!->'
  | OnNoChange (Strat extra) (Strat extra)     -- ^ '>-!'
  | Repeat (Strat extra)                       -- ^ 'repeatR'

-- | Things that can be used where a 'Step' is expected; a bare
-- 'TransformSpec' lifts to a singleton 'chain'.
class ToStep s where
  toStep :: s extra -> Step extra

instance ToStep Step where
  toStep = id

instance ToStep TransformSpec where
  toStep = one

-- | An ordered chain of transformations, dispatched per constructor. See
-- 'Chain'.
chain :: [TransformSpec extra] -> Step extra
chain = Chain

-- | A singleton 'chain', for use as an operand of the node-level operators.
one :: TransformSpec extra -> Step extra
one = Chain . pure

-- | Embed a whole strategy as a single node action.
nested :: Strat extra -> Step extra
nested = EmbedS

topdown, bottomup, topdownFix, topdownSuc, innerMost
  :: ToStep s => s extra -> Strat extra
topdown    = TopDown . toStep
bottomup   = BottomUp . toStep
topdownFix = TopDownFix . toStep
topdownSuc = TopDownSuc . toStep
innerMost  = InnerMost . toStep

-- | A whole-term pass, run as @apply \<name\> \<rewrite\>@.
pass :: String -> Rewrite extra -> Strat extra
pass = Pass

-- | Embed an already-compiled strategy, by its compiled binding.
callStrategy :: Rewrite extra -> Strat extra
callStrategy = CallStrategy

-- | Keep applying a strategy until it no longer fires.
repeatR :: Strat extra -> Strat extra
repeatR = Repeat

infixr 6 >->
infixr 5 !->
infixr 5 >-!

-- | The combinators shared by both DSL layers. Fixities match
-- "Clash.Rewrite.Combinators"; a strategy spec module should import only
-- this module, so the names are unambiguous there.
class Strategic a where
  -- | Apply two strategies in succession.
  (>->) :: a -> a -> a
  -- | Only apply the second strategy if the first one succeeds.
  (!->) :: a -> a -> a
  -- | Only apply the second strategy if the first one fails.
  (>-!) :: a -> a -> a

instance Strategic (Strat extra) where
  (>->) = Seq
  (!->) = OnChange
  (>-!) = OnNoChange

instance Strategic (Step extra) where
  (>->) = SeqS
  (!->) = OnChangeS
  (>-!) = OnNoChangeS

infixr 6 >-!->

-- | Apply two strategies in succession, with a @deepseq@ of the term in
-- between. Strategy layer only.
(>-!->) :: Strat extra -> Strat extra -> Strat extra
(>-!->) = DeepSeqS
