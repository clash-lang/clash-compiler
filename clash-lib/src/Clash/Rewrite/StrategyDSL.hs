{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  A pure description language for rewrite strategies. A strategy written in
  this DSL is compiled by "Clash.Rewrite.StrategyDSL.TH" into a fused
  traversal that matches each node's constructor once and runs only the
  transformations registered for that constructor.

  The DSL has two layers:

  * 'Step': what runs at a single node inside a fused traversal. An ordered
    'chain' of transformations, sequencing and success/failure conditionals
    between chain segments, or a whole 'nested' strategy embedded as one node
    action.

  * 'Strat': a complete strategy. Fused traversals over a 'Step', whole-term
    passes, and the usual combinators ('>->', '!->', '>-!', '>-!->',
    'repeatR') between them.
-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Clash.Rewrite.StrategyDSL
  ( -- * Transformation metadata
    Constructor (..)
  , TransformationWorker (..)
  , Transformation (..)
  , toTransformation
  , anyConstructor
  , named
    -- ** Worker builders: workers receiving the constructor's fields
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
    -- ** Worker builders: workers receiving the node itself
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

import Language.Haskell.TH.Syntax (Name)

-- | One 'Clash.Core.Term.Term' constructor.
data Constructor
  = CVar | CData | CLiteral | CPrim | CLam | CTyLam
  | CApp | CTyApp | CLet | CCase | CCast | CTick
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A single function separated by the constructor it fires on. Note that this
-- isn't a single transformation per se, as one transformation might fire on
-- multiple constructors. 'Transformation' ties together multiple workers into
-- a single transformation.
--
-- The 'Name' must refer to an exported top-level worker so the generated
-- dispatch code can reference it from the splice site.
--
-- A worker that returns a term other than the one it was given must say so
-- with 'Clash.Rewrite.Util.changed': a 'chain' hands the fields it matched to
-- the next member as long as no change was signaled.
data TransformationWorker
  = FieldWorker !Constructor !Name
  -- ^ @worker :: TransformContext -> Term -> \<fields of the constructor\>
  -- -> RewriteMonad extra Term@. The worker receives the matched node (to
  -- return on the unchanged path with sharing intact) followed by the
  -- constructor's fields.
  | Worker !Constructor !Name
  -- ^ @worker :: Rewrite extra@. A worker that fires only at the given
  -- constructor, like the per-constructor worker above, but receives the
  -- whole node instead of its deconstructed fields. For transformations that
  -- decompose the node themselves, e.g. by collecting the entire application
  -- spine rather than one @App@ layer.
  | AnyWorker !Name
  -- ^ @worker :: Rewrite extra@, applied at all constructors. For a
  -- transformation whose firing condition lives in the
  -- 'Clash.Rewrite.Types.TransformContext' rather than the constructor
  -- (@topLet@).
  deriving (Eq, Show)

-- | Single, named transformation. Can consist of multiple workers (see
-- 'TransformationWorker').
data Transformation = Transformation
  { name :: !String
  -- ^ Canonical name, used for debug tracing, transformation counters, and
  -- the rewrite history.
  , workers :: ![TransformationWorker]
  } deriving (Eq, Show)

-- | Declare a transformation's workers: at most one per constructor, and
-- either an 'AnyWorker' or per-constructor workers, never both. Checked when
-- the transformation is compiled.
toTransformation :: String -> [TransformationWorker] -> Transformation
toTransformation = Transformation

-- | Declare a transformation that fires at any constructor. See 'AnyWorker'.
anyConstructor :: String -> Name -> Transformation
anyConstructor name worker = Transformation name [AnyWorker worker]

-- | Give a transformation a different name; used for deliberate debug
-- aliases of a transformation that runs at several strategy positions (the
-- transformation counters and rewrite history key on the name).
named :: String -> Transformation -> Transformation
named alias transformation = transformation { name = alias }

onVar, onData, onLiteral, onPrim, onLam, onTyLam, onApp, onTyApp, onLet,
  onCase, onCast, onTick :: Name -> [TransformationWorker]
onVar     = pure . FieldWorker CVar
onData    = pure . FieldWorker CData
onLiteral = pure . FieldWorker CLiteral
onPrim    = pure . FieldWorker CPrim
onLam     = pure . FieldWorker CLam
onTyLam   = pure . FieldWorker CTyLam
onApp     = pure . FieldWorker CApp
onTyApp   = pure . FieldWorker CTyApp
onLet     = pure . FieldWorker CLet
onCase    = pure . FieldWorker CCase
onCast    = pure . FieldWorker CCast
onTick    = pure . FieldWorker CTick

onVarNode, onPrimNode, onAppNode, onTyAppNode, onLetNode,
  onTickNode :: Name -> [TransformationWorker]
onVarNode   = pure . Worker CVar
onPrimNode  = pure . Worker CPrim
onAppNode   = pure . Worker CApp
onTyAppNode = pure . Worker CTyApp
onLetNode   = pure . Worker CLet
onTickNode  = pure . Worker CTick

-- | What runs at a single node inside a fused traversal.
data Step
  = Chain [Transformation]
  -- ^ One constructor match dispatches the members registered for that
  -- constructor, in list order. Equivalent to the sequential
  -- @apply n1 t1 '>->' apply n2 t2 …@ chain over all members.
  | SeqS Step Step
  -- ^ '>->' at the node: the right side re-dispatches on the left side's
  -- result.
  | OnChangeS Step Step
  -- ^ '!->' at the node: the right side runs only when the left side
  -- signaled change.
  | OnNoChangeS Step Step
  -- ^ '>-!' at the node: the right side runs only when the left side did
  -- not signal change.
  | EmbedS Strat
  -- ^ A whole strategy as one node action.
  deriving (Eq, Show)

-- | A complete rewriting strategy.
data Strat
  = TopDown Step
  -- ^ Fused 'Clash.Rewrite.Combinators.topdownR': repeat the step at each
  -- node until it no longer fires, then descend.
  | BottomUp Step
  -- ^ Fused 'Clash.Rewrite.Combinators.bottomupR': descend first, run the
  -- step once on the rebuilt node.
  | TopDownFix Step
  -- ^ Fused 'Clash.Rewrite.Combinators.topdownFixR'. Mind its soundness
  -- precondition (Note [topdownFixR] in "Clash.Rewrite.Combinators").
  | TopDownSuc Step
  -- ^ Fused 'Clash.Rewrite.Combinators.topdownSucR': stop at the first
  -- success, neither repeating nor descending below it.
  | InnerMost Step
  -- ^ Fused 'Clash.Rewrite.Combinators.innerMost': bottom-up; when the step
  -- fires, re-traverse the result until the innermost fixpoint is reached.
  | Pass String Name
  -- ^ A whole-term pass: @apply \<name\> \<rewrite\>@, for transformations
  -- that traverse the term themselves (@makeANF@, @etaExpansionTL@, …).
  | CallStrategy Name
  -- ^ Reference to another compiled strategy binding.
  | Seq Strat Strat            -- ^ '>->'
  | DeepSeqS Strat Strat       -- ^ '>-!->'
  | OnChange Strat Strat       -- ^ '!->'
  | OnNoChange Strat Strat     -- ^ '>-!'
  | Repeat Strat               -- ^ 'repeatR'
  deriving (Eq, Show)

-- | Things that can be used where a 'Step' is expected; a bare
-- 'Transformation' lifts to a singleton 'chain'.
class ToStep s where
  toStep :: s -> Step

instance ToStep Step where
  toStep = id

instance ToStep Transformation where
  toStep = one

-- | An ordered chain of transformations, dispatched per constructor. See
-- 'Chain'.
chain :: [Transformation] -> Step
chain = Chain

-- | A singleton 'chain', for use as an operand of the node-level operators.
one :: Transformation -> Step
one = Chain . pure

-- | Embed a whole strategy as a single node action.
nested :: Strat -> Step
nested = EmbedS

topdown, bottomup, topdownFix, topdownSuc, innerMost :: ToStep s => s -> Strat
topdown    = TopDown . toStep
bottomup   = BottomUp . toStep
topdownFix = TopDownFix . toStep
topdownSuc = TopDownSuc . toStep
innerMost  = InnerMost . toStep

-- | A whole-term pass, run as @apply \<name\> \<rewrite\>@.
pass :: String -> Name -> Strat
pass = Pass

-- | Reference another compiled strategy by the name of its binding.
callStrategy :: Name -> Strat
callStrategy = CallStrategy

-- | Keep applying a strategy until it no longer fires.
repeatR :: Strat -> Strat
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

instance Strategic Strat where
  (>->) = Seq
  (!->) = OnChange
  (>-!) = OnNoChange

instance Strategic Step where
  (>->) = SeqS
  (!->) = OnChangeS
  (>-!) = OnNoChangeS

infixr 6 >-!->

-- | Apply two strategies in succession, with a @deepseq@ of the term in
-- between. Strategy layer only.
(>-!->) :: Strat -> Strat -> Strat
(>-!->) = DeepSeqS
