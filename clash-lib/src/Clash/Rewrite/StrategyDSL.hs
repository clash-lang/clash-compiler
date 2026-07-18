{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  A pure description language for rewrite strategies. A strategy written in
  this DSL is compiled — at compile time, by "Clash.Rewrite.StrategyDSL.TH" —
  into a fused traversal that matches each node's constructor once and runs
  only the transformations registered for that shape.

  The DSL has two layers:

  * 'Step': what runs at a single node inside a fused traversal — an ordered
    'chain' of transformations, sequencing and success/failure conditionals
    between chain segments, or a whole 'nested' strategy embedded as one node
    action.

  * 'Strat': a complete strategy — fused traversals over a 'Step', whole-term
    passes, and the usual combinators ('>->', '!->', '>-!', '>-!->',
    'repeatR') between them.

  Transformations enter the DSL as 'TransformSpec' values: a canonical debug
  name plus, per 'Term' constructor the transformation can fire on, the
  'Language.Haskell.TH.Syntax.Name' of a worker function. Because the
  generated dispatch code applies a worker to the matched constructor's
  fields, declaring a wrong shape is a type error at the splice site — entry
  shapes cannot silently drift from a transformation's implementation.
-}

module Clash.Rewrite.StrategyDSL
  ( -- * Transformation metadata
    Shape (..)
  , Entry (..)
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

import Language.Haskell.TH.Syntax (Name)

-- | One 'Clash.Core.Term.Term' constructor, as an entry shape.
data Shape
  = SVar | SData | SLiteral | SPrim | SLam | STyLam
  | SApp | STyApp | SLet | SCase | SCast | STick
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | How a transformation is entered at one constructor. The 'Name' must
-- refer to an exported top-level worker so the generated dispatch code can
-- reference it from the splice site.
data Entry
  = FieldEntry !Shape !Name
  -- ^ @worker :: TransformContext -> Term -> \<fields of the constructor\>
  -- -> RewriteMonad extra Term@. The worker receives the matched node (to
  -- return on the unchanged path with sharing intact) followed by the
  -- constructor's fields.
  | NodeEntry !Shape !Name
  -- ^ @worker :: Rewrite extra@, registered at exactly this constructor.
  -- For transformations that operate on the node itself, such as
  -- application-spine collectors.
  | AnyShapeEntry !Name
  -- ^ @worker :: Rewrite extra@, offered at every node. Escape hatch for a
  -- transformation whose firing condition lives in the 'TransformContext'
  -- rather than the constructor (@topLet@). Mutually exclusive with the
  -- other entries.
  deriving (Eq, Show)

entryShape :: Entry -> Maybe Shape
entryShape (FieldEntry shape _) = Just shape
entryShape (NodeEntry shape _) = Just shape
entryShape (AnyShapeEntry _) = Nothing

-- | Pure metadata for one transformation: its canonical name (used for debug
-- tracing, transformation counters, and the rewrite history) and its entry
-- table. Construct with 'transform' or 'anyShape', which validate the entry
-- table.
data TransformSpec = TransformSpec
  { specName :: !String
  , specEntries :: ![Entry]
  } deriving (Eq, Show)

-- | Declare a transformation's entry shapes. The entry list must be
-- non-empty, may register at most one entry per constructor, and may not mix
-- 'AnyShapeEntry' with per-constructor entries: a transformation is either
-- shape-agnostic or shaped, never both.
transform :: String -> [Entry] -> TransformSpec
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
anyShape :: String -> Name -> TransformSpec
anyShape name worker = TransformSpec name [AnyShapeEntry worker]

-- | Give a transformation a different name; used for deliberate debug
-- aliases of a transformation that runs at several strategy positions (the
-- transformation counters and rewrite history key on the name).
named :: String -> TransformSpec -> TransformSpec
named alias spec = spec { specName = alias }

onVar, onData, onLiteral, onPrim, onLam, onTyLam, onApp, onTyApp, onLet,
  onCase, onCast, onTick :: Name -> [Entry]
onVar     = pure . FieldEntry SVar
onData    = pure . FieldEntry SData
onLiteral = pure . FieldEntry SLiteral
onPrim    = pure . FieldEntry SPrim
onLam     = pure . FieldEntry SLam
onTyLam   = pure . FieldEntry STyLam
onApp     = pure . FieldEntry SApp
onTyApp   = pure . FieldEntry STyApp
onLet     = pure . FieldEntry SLet
onCase    = pure . FieldEntry SCase
onCast    = pure . FieldEntry SCast
onTick    = pure . FieldEntry STick

-- $nodeEntries
-- Only the constructors an application spine can start with, or that spine
-- transformations otherwise register at, have @on*Node@ builders; extend the
-- list when a transformation needs another one.

onVarNode, onPrimNode, onAppNode, onTyAppNode, onLetNode,
  onTickNode :: Name -> [Entry]
onVarNode   = pure . NodeEntry SVar
onPrimNode  = pure . NodeEntry SPrim
onAppNode   = pure . NodeEntry SApp
onTyAppNode = pure . NodeEntry STyApp
onLetNode   = pure . NodeEntry SLet
onTickNode  = pure . NodeEntry STick

-- | What runs at a single node inside a fused traversal.
data Step
  = Chain [TransformSpec]
  -- ^ One constructor match dispatches the members registered for that
  -- shape, in list order. Equivalent to the sequential
  -- @apply n1 t1 '>->' apply n2 t2 …@ chain over those members: after a
  -- member changes the term, the remaining members re-guard on the changed
  -- term through their full entry tables (the /guarded suffix/); members
  -- registered only for other constructors wait for the traversal's next
  -- dispatch.
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
-- 'TransformSpec' lifts to a singleton 'chain'.
class ToStep s where
  toStep :: s -> Step

instance ToStep Step where
  toStep = id

instance ToStep TransformSpec where
  toStep = one

-- | An ordered chain of transformations, dispatched per constructor. See
-- 'Chain'.
chain :: [TransformSpec] -> Step
chain = Chain

-- | A singleton 'chain', for use as an operand of the node-level operators.
one :: TransformSpec -> Step
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
