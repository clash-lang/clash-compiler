{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Compile a "Clash.Rewrite.StrategyDSL" strategy into executable dispatch
  code. 'compileStrategy' produces the fused fast path: one constructor match
  per node drives both the dispatch of the node program and the descent into
  the node's children. 'compileStrategyReference' produces an obviously
  correct unfused equivalent from the same spec, built from the
  "Clash.Rewrite.Combinators" traversals; the two agree on results,
  transformation counts, and change flags, so the reference serves as the
  test oracle for the generator — compare the manually unrolled
  'Clash.Core.Type.tyView' and the readable specification it keeps in a
  comment.

  Inspect what a splice generates with @-ddump-splices@ (add
  @-ddump-to-file@ to keep the dump next to the interface files).

  See Note [chain semantics] and Note [NonRec erasure during descent].
-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Rewrite.StrategyDSL.TH
  ( compileStrategy
  , compileStrategyReference
  , compileStrategyReferenceWith
  , dispatchQ
  , asRewriteQ
  ) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Writer as Writer
import Data.Char (isAlphaNum)
import Data.List (tails)
import qualified Data.Monoid as Monoid
import Language.Haskell.TH hiding (Prim)

import Clash.Core.Term
  (Bind (..), CoreContext (..), Term (..), bindToList, patIds, primArg)
import Clash.Core.VarEnv (extendInScopeSet, extendInScopeSetList)
import qualified Clash.Rewrite.Combinators as Comb
import Clash.Rewrite.StrategyDSL
  (Entry (..), Shape (..), Step (..), Strat (..), TransformSpec (..))
import Clash.Rewrite.Types (TransformContext (..))
import Clash.Rewrite.Util (apply, applyWith)

{-
Note [chain semantics]
~~~~~~~~~~~~~~~~~~~~~~
A 'Chain' compiles, per constructor, to the members registered for that
constructor (the constructor's "bucket"), run in list order. Members
registered only for other constructors do not run at this node, even when a
mid-chain rewrite gives the node their constructor; they wait for the
traversal's next dispatch.

Two contracts make the generated bucket code equal to the sequential
@apply n1 g1 >-> apply n2 g2 >-> …@ chain over the bucket members, where
each @gI@ guards member I on its full entry table:

1. /Unchanged means untouched./ A worker that does not signal change must
   return the node it was given, unmodified. This is a pre-existing engine
   invariant ("Expression changed without notice" in
   'Clash.Rewrite.Util.applyDebug'). It lets the next bucket member run on
   the constructor's original fields without re-matching.

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
-- Shapes
--------------------------------------------------------------------------------

-- | The term constructor belonging to a 'Shape'.
shapeCon :: Shape -> Name
shapeCon shape = case shape of
  SVar -> 'Var
  SData -> 'Data
  SLiteral -> 'Literal
  SPrim -> 'Prim
  SLam -> 'Lam
  STyLam -> 'TyLam
  SApp -> 'App
  STyApp -> 'TyApp
  SLet -> 'Let
  SCase -> 'Case
  SCast -> 'Cast
  STick -> 'Tick

-- | Binder hints for a shape's fields. Underscore-prefixed because not
-- every generated arm uses every field.
shapeFieldHints :: Shape -> [String]
shapeFieldHints shape = case shape of
  SVar -> ["_i"]
  SData -> ["_dc"]
  SLiteral -> ["_lit"]
  SPrim -> ["_p"]
  SLam -> ["_v", "_e"]
  STyLam -> ["_tv", "_e"]
  SApp -> ["_fun", "_arg"]
  STyApp -> ["_fun", "_argTy"]
  SLet -> ["_bnd", "_body"]
  SCase -> ["_subj", "_altsTy", "_alts"]
  SCast -> ["_e", "_fromTy", "_toTy"]
  STick -> ["_tick", "_e"]

shapeFieldNames :: Shape -> Q [Name]
shapeFieldNames = traverse newName . shapeFieldHints

allShapes :: [Shape]
allShapes = [minBound .. maxBound]

-- | Shapes whose nodes have no subterms to descend into.
leafShape :: Shape -> Bool
leafShape shape = shape `elem` [SVar, SData, SLiteral, SPrim]

--------------------------------------------------------------------------------
-- Members
--------------------------------------------------------------------------------

-- | How a bucket member is invoked once the constructor has matched.
data Invoke
  = InvokeFields Name -- ^ worker applied to ctx, node, and the node's fields
  | InvokeNode Name   -- ^ worker applied to ctx and the node only

-- | The member's invocation at this shape, if it is in the shape's bucket.
invokeAt :: Shape -> TransformSpec -> Maybe Invoke
invokeAt shape = go . specEntries
 where
  go [] = Nothing
  go (FieldEntry shape' worker : rest)
    | shape' == shape = Just (InvokeFields worker)
    | otherwise = go rest
  go (NodeEntry shape' worker : rest)
    | shape' == shape = Just (InvokeNode worker)
    | otherwise = go rest
  go (AnyShapeEntry worker : _) = Just (InvokeNode worker)

invokeE :: Invoke -> Q Exp -> Q Exp -> [Q Exp] -> Q Exp
invokeE (InvokeFields worker) ctxE nodeE fieldEs =
  foldl appE (varE worker) (ctxE : nodeE : fieldEs)
invokeE (InvokeNode worker) ctxE nodeE _fieldEs =
  [| $(varE worker) $ctxE $nodeE |]

validateChain :: [TransformSpec] -> Q ()
validateChain members = do
  Monad.when (null members) (fail "compileStrategy: empty chain")
  case [name | name : rest <- tails (map specName members), name `elem` rest] of
    name : _ ->
      fail ("compileStrategy: duplicate transformation name in one chain: "
            <> show name <> " (alias one occurrence with 'named')")
    [] -> pure ()

--------------------------------------------------------------------------------
-- Small builders
--------------------------------------------------------------------------------

-- | Bind the result and change flag of an action:
-- @do { (t, w) <- Writer.listen action; \<continue t (getAny w)\> }@
withListen :: Q Exp -> (Q Exp -> Q Exp -> Q Exp) -> Q Exp
withListen action continue = do
  t <- newName "t"
  w <- newName "w"
  doE
    [ bindS (tupP [varP t, varP w]) [| Writer.listen $action |]
    , noBindS (continue (varE t) [| Monoid.getAny $(varE w) |])
    ]

-- | Bind an expression so a continuation can use it more than once.
bindTo :: String -> Q Exp -> (Q Exp -> Q Exp) -> Q Exp
bindTo hint bound continue = do
  x <- newName hint
  letE [valD (varP x) (normalB bound) []] (continue (varE x))

-- | A fresh name for a generated let-binding, derived from a transformation
-- name. Underscore-prefixed: not every generated binding ends up used.
freshBinding :: String -> Q Name
freshBinding hint = newName ('_' : map sanitize hint)
 where
  sanitize c = if isAlphaNum c then c else '_'

-- | @\\ctx tm -> case tm of { \<arm per shape with a program\>; _ -> pure tm }@
-- The wildcard is omitted when every constructor has an arm.
nodeCaseExp :: (Shape -> Maybe (Q Exp -> Q Exp -> [Q Exp] -> Q Exp)) -> Q Exp
nodeCaseExp armFor = do
  ctx <- newName "ctx"
  tm <- newName "tm"
  let arms =
        [ do fieldNames <- shapeFieldNames shape
             match (conP (shapeCon shape) (map varP fieldNames))
                   (normalB (armBody (varE ctx) (varE tm) (map varE fieldNames)))
                   []
        | shape <- allShapes
        , Just armBody <- [armFor shape]
        ]
      fallthrough
        | length arms == length allShapes = []
        | otherwise = [match wildP (normalB [| pure $(varE tm) |]) []]
  lamE [varP ctx, varP tm] (caseE (varE tm) (arms <> fallthrough))

--------------------------------------------------------------------------------
-- Node programs ('Step')
--------------------------------------------------------------------------------

-- | A compiled node program. 'csKnown' is the program entered with the
-- node's constructor statically known and its fields in scope; 'Nothing'
-- means the program is the identity at that constructor. 'csDispatch'
-- references a generated binding running the program on a term of unknown
-- constructor.
data CompiledStep = CompiledStep
  { csDecs :: [Q Dec]
  , csKnown :: Shape -> Maybe (Q Exp -> Q Exp -> [Q Exp] -> Q Exp)
  , csDispatch :: Q Exp
  }

compileStep :: Step -> Q CompiledStep
compileStep step = case step of
  Chain members -> compileChain members
  SeqS a b -> compileStep2 a b seqKnown seqDispatch
  OnChangeS a b -> compileStep2 a b onChangeKnown (combinatorDispatch [| (Comb.!->) |])
  OnNoChangeS a b -> compileStep2 a b onNoChangeKnown (combinatorDispatch [| (Comb.>-!) |])

  EmbedS strat -> do
    embedded <- freshBinding "embedded"
    let dec = valD (varP embedded) (normalB (compileStrategy strat)) []
    pure CompiledStep
      { csDecs = [dec]
      , csKnown = \_shape -> Just (\ctxE nodeE _fieldEs -> [| $(varE embedded) $ctxE $nodeE |])
      , csDispatch = varE embedded
      }
 where
  seqKnown ca cb shape = case (csKnown ca shape, csKnown cb shape) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just kb) -> Just kb
    (Just ka, Nothing) -> Just $ \ctxE nodeE fieldEs ->
      withListen (ka ctxE nodeE fieldEs) $ \tE changedE ->
        condE changedE [| $(csDispatch cb) $ctxE $tE |] [| pure $tE |]
    (Just ka, Just kb) -> Just $ \ctxE nodeE fieldEs ->
      withListen (ka ctxE nodeE fieldEs) $ \tE changedE ->
        -- When the left side did not signal change the term is untouched
        -- (Note [chain semantics], contract 1), so the right side can still
        -- run on the statically matched fields.
        condE changedE [| $(csDispatch cb) $ctxE $tE |] (kb ctxE tE fieldEs)

  seqDispatch ca cb =
    [| \ctx tm -> $(csDispatch ca) ctx tm >>= $(csDispatch cb) ctx |]

  onChangeKnown ca cb shape = case csKnown ca shape of
    Nothing -> Nothing
    Just ka -> Just $ \ctxE nodeE fieldEs ->
      withListen (ka ctxE nodeE fieldEs) $ \tE changedE ->
        condE changedE [| $(csDispatch cb) $ctxE $tE |] [| pure $tE |]

  onNoChangeKnown ca cb shape = case (csKnown ca shape, csKnown cb shape) of
    (Nothing, kb) -> kb
    (Just ka, Nothing) -> Just ka
    (Just ka, Just kb) -> Just $ \ctxE nodeE fieldEs ->
      withListen (ka ctxE nodeE fieldEs) $ \tE changedE ->
        condE changedE [| pure $tE |] (kb ctxE tE fieldEs)

  combinatorDispatch op ca cb = [| $op $(csDispatch ca) $(csDispatch cb) |]

  compileStep2 a b mkKnown mkDispatch = do
    ca <- compileStep a
    cb <- compileStep b
    dispatch <- freshBinding "dispatch"
    let dispatchDec = valD (varP dispatch) (normalB (mkDispatch ca cb)) []
    pure CompiledStep
      { csDecs = csDecs ca <> csDecs cb <> [dispatchDec]
      , csKnown = mkKnown ca cb
      , csDispatch = varE dispatch
      }

compileChain :: [TransformSpec] -> Q CompiledStep
compileChain members = do
  validateChain members
  guardNames <- traverse (freshBinding . ("g_" <>) . specName) members
  let guardDecs = zipWith memberGuardDec guardNames members
  dispatch <- freshBinding "dispatch"
  let -- The shape's bucket, each member paired with the guards of the
      -- bucket members after it (the member's guarded suffix).
      bucketAt shape =
        [ (member, invoke, [g | (m, g) <- rest, Just _ <- [invokeAt shape m]])
        | (member, _guardName) : rest <- tails (zip members guardNames)
        , Just invoke <- [invokeAt shape member]
        ]
      known shape = case bucketAt shape of
        [] -> Nothing
        bucket -> Just (chainArm bucket)
      dispatchDec = valD (varP dispatch) (normalB (nodeCaseExp known)) []
  pure CompiledStep
    { csDecs = guardDecs <> [dispatchDec]
    , csKnown = known
    , csDispatch = varE dispatch
    }

-- | A bucket entered with the constructor statically known: run the members
-- in order on the original fields; after the first change, hand the term to
-- the remaining bucket members' guards. See Note [chain semantics].
chainArm
  :: [(TransformSpec, Invoke, [Name])]
  -> Q Exp -> Q Exp -> [Q Exp] -> Q Exp
chainArm bucket0 ctxE _nodeE0 fieldEs = go _nodeE0 bucket0
 where
  go nodeE [] = [| pure $nodeE |]
  go nodeE ((member, invoke, suffixGuards) : rest) = do
    t <- newName "t"
    changed <- newName "changed"
    doE
      [ bindS (tupP [varP t, varP changed])
          [| applyWith $(stringE (specName member)) $ctxE $nodeE
               $(invokeE invoke ctxE nodeE fieldEs) |]
      , noBindS $ condE (varE changed)
          (guardedSuffix suffixGuards ctxE (varE t))
          (go (varE t) rest)
      ]

-- | @g1 ctx t >>= g2 ctx >>= …@: offer the term to each remaining bucket
-- member through its full entry table.
guardedSuffix :: [Name] -> Q Exp -> Q Exp -> Q Exp
guardedSuffix [] _ctxE nodeE = [| pure $nodeE |]
guardedSuffix (g : gs) ctxE nodeE =
  foldl (\acc g' -> [| $acc >>= $(varE g') $ctxE |])
        [| $(varE g) $ctxE $nodeE |]
        gs

-- | One member's guard: match the term against the member's full entry
-- table; run the member instrumented on a match, return the term untouched
-- (without instrumentation) otherwise.
memberGuardDec :: Name -> TransformSpec -> Q Dec
memberGuardDec guardName spec =
  valD (varP guardName) (normalB (guardExp instrumented spec)) []
 where
  instrumented invoke ctxE nodeE fieldEs =
    [| fmap fst (applyWith $(stringE (specName spec)) $ctxE $nodeE
                   $(invokeE invoke ctxE nodeE fieldEs)) |]

-- | @\\ctx tm -> case tm of …@ over a transformation's entry table, with the
-- given wrapper around each invocation.
guardExp
  :: (Invoke -> Q Exp -> Q Exp -> [Q Exp] -> Q Exp)
  -> TransformSpec
  -> Q Exp
guardExp wrap spec = nodeCaseExp $ \shape -> wrap <$> invokeAt shape spec

--------------------------------------------------------------------------------
-- Descent (the congruence step of the fused traversals)
--------------------------------------------------------------------------------

{-
Note [NonRec erasure during descent]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'Clash.Rewrite.Combinators.allR' matches let-expressions through the 'Letrec'
pattern synonym and rebuilds them with 'Rec' bindings, so any 'NonRec' let a
traversal descends into comes back as a 'Rec' let. The generated descent
replicates that behavior deliberately: preserving 'NonRec' is a real behavior
change (downstream code branches on 'Rec'/'NonRec', and netlist rendering
order can shift) that must be validated on its own, separately from this
dispatch machinery.
-}

-- | The one-level descent for one constructor, replicating
-- 'Clash.Rewrite.Combinators.allR' exactly: same 'CoreContext' pushes, same
-- 'InScopeSet' extensions, and the argument's context computed from the
-- already-rewritten function. The continuation receives the rebuilt node's
-- fields and the rebuilt node itself; descent preserves the constructor, so
-- the continuation may run a statically matched program on the result.
genDescend
  :: Shape
  -> Q Exp            -- ^ recursion function @:: TransformContext -> Term -> m Term@
  -> (Q Exp, Q Exp)   -- ^ the context's InScopeSet and CoreContext stack
  -> Q Exp            -- ^ the node
  -> [Q Exp]          -- ^ the node's fields
  -> ([Q Exp] -> Q Exp -> Q Exp)  -- ^ continuation: rebuilt fields, rebuilt node
  -> Q Exp
genDescend shape go (isE, cE) nodeE fieldEs continue = case (shape, fieldEs) of
  (SVar, _) -> leaf
  (SData, _) -> leaf
  (SLiteral, _) -> leaf
  (SPrim, _) -> leaf

  (SLam, [v, e]) -> do
    e' <- newName "e'"
    doE
      [ bindS (varP e')
          [| $go (TransformContext (extendInScopeSet $isE $v) (LamBody $v : $cE)) $e |]
      , continueWith [v, varE e'] [| Lam $v $(varE e') |]
      ]

  (STyLam, [tv, e]) -> do
    e' <- newName "e'"
    doE
      [ bindS (varP e')
          [| $go (TransformContext (extendInScopeSet $isE $tv) (TyLamBody $tv : $cE)) $e |]
      , continueWith [tv, varE e'] [| TyLam $tv $(varE e') |]
      ]

  (SApp, [fun, arg]) -> do
    fun' <- newName "fun'"
    arg' <- newName "arg'"
    doE
      [ bindS (varP fun') [| $go (TransformContext $isE (AppFun : $cE)) $fun |]
        -- The argument's context is computed from the already rewritten
        -- function; the order is load-bearing.
      , bindS (varP arg')
          [| $go (TransformContext $isE (AppArg (primArg $(varE fun')) : $cE)) $arg |]
      , continueWith [varE fun', varE arg'] [| App $(varE fun') $(varE arg') |]
      ]

  (STyApp, [fun, argTy]) -> do
    fun' <- newName "fun'"
    doE
      [ bindS (varP fun') [| $go (TransformContext $isE (TyAppC : $cE)) $fun |]
      , continueWith [varE fun', argTy] [| TyApp $(varE fun') $argTy |]
      ]

  (SLet, [bnd, body]) -> do
    -- See Note [NonRec erasure during descent]
    bs <- newName "bs"
    bndrs <- newName "bndrs"
    is' <- newName "is'"
    bs' <- newName "bs'"
    body' <- newName "body'"
    doE
      [ letS
          [ valD (varP bs) (normalB [| bindToList $bnd |]) []
          , valD (varP bndrs) (normalB [| map fst $(varE bs) |]) []
          , valD (varP is') (normalB [| extendInScopeSetList $isE $(varE bndrs) |]) []
          ]
      , bindS (varP bs')
          [| traverse
               (\(b, rhs) ->
                  fmap ((,) b)
                    ($go (TransformContext $(varE is')
                            (LetBinding b $(varE bndrs) : $cE))
                         rhs))
               $(varE bs) |]
      , bindS (varP body')
          [| $go (TransformContext $(varE is') (LetBody $(varE bs) : $cE)) $body |]
      , continueWith [[| Rec $(varE bs') |], varE body']
          [| Let (Rec $(varE bs')) $(varE body') |]
      ]

  (SCase, [subj, altsTy, alts]) -> do
    subj' <- newName "subj'"
    alts' <- newName "alts'"
    doE
      [ bindS (varP subj') [| $go (TransformContext $isE (CaseScrut : $cE)) $subj |]
      , bindS (varP alts')
          [| traverse
               (\(p, e) ->
                  let (tvs, ids) = patIds p
                      isAlt = extendInScopeSetList (extendInScopeSetList $isE tvs) ids
                  in fmap ((,) p) ($go (TransformContext isAlt (CaseAlt p : $cE)) e))
               $alts |]
      , continueWith [varE subj', altsTy, varE alts']
          [| Case $(varE subj') $altsTy $(varE alts') |]
      ]

  (SCast, [e, fromTy, toTy]) -> do
    e' <- newName "e'"
    doE
      [ bindS (varP e') [| $go (TransformContext $isE (CastBody : $cE)) $e |]
      , continueWith [varE e', fromTy, toTy] [| Cast $(varE e') $fromTy $toTy |]
      ]

  (STick, [tick, e]) -> do
    e' <- newName "e'"
    doE
      [ bindS (varP e') [| $go (TransformContext $isE (TickC $tick : $cE)) $e |]
      , continueWith [tick, varE e'] [| Tick $tick $(varE e') |]
      ]

  _ -> fail ("genDescend: field count mismatch for " <> show shape)
 where
  leaf = continue fieldEs nodeE
  continueWith fieldEs' rebuiltE = noBindS (continue fieldEs' rebuiltE)

--------------------------------------------------------------------------------
-- Fused traversals
--------------------------------------------------------------------------------

-- | What a traversal does at one node: given the recursion function, the
-- node program for the matched shape ('Nothing' when the program is the
-- identity there), the context, the node, its fields, and a descent builder,
-- produce the body of @go@'s clause for that shape.
type NodeArm
  =  Q Exp                                  -- ^ the traversal's @go@
  -> Maybe (Q Exp -> Q Exp -> [Q Exp] -> Q Exp)
  -> Q Exp                                  -- ^ ctx
  -> Q Exp                                  -- ^ the node
  -> [Q Exp]                                -- ^ its fields
  -> (([Q Exp] -> Q Exp -> Q Exp) -> Q Exp) -- ^ descend with a continuation
  -> Q Exp

-- | Generate a fused traversal: a recursive @go@ whose single constructor
-- match drives both the node program and the descent. The recursion
-- structure of each traversal is pinned to its "Clash.Rewrite.Combinators"
-- counterpart, with the node program as @r@:
--
-- * 'TopDown': @'Comb.topdownR' r = 'Comb.repeatR' r '>->' 'Comb.allR' …@ —
--   on change, re-enter @go@ at this node (which also re-matches the
--   constructor); otherwise descend. See Note [topdown repeatR] in
--   "Clash.Rewrite.Combinators".
-- * 'BottomUp': descend first, run the program once on the rebuilt node —
--   whose constructor is already known, so no re-match.
-- * 'TopDownSuc': run the program once; when it fired, stop — neither
--   repeating nor descending.
-- * 'InnerMost': bottom-up; when the program fires, re-enter @go@ on the
--   result until the innermost fixpoint is reached.
-- * 'TopDownFix': see 'genTopDownFix'.
genTraversal :: Strat -> Q Exp
genTraversal strat = case strat of
  TopDown step -> fused step $ \goE known ctxE tmE fieldEs descend ->
    case known of
      Nothing -> descend rebuildOnly
      Just ka ->
        withListen (ka ctxE tmE fieldEs) $ \tE changedE ->
          condE changedE [| $goE $ctxE $tE |] (descend rebuildOnly)

  TopDownSuc step -> fused step $ \_goE known ctxE tmE fieldEs descend ->
    case known of
      Nothing -> descend rebuildOnly
      Just ka ->
        withListen (ka ctxE tmE fieldEs) $ \tE changedE ->
          condE changedE [| pure $tE |] (descend rebuildOnly)

  BottomUp step -> fused step $ \_goE known ctxE _tmE _fieldEs descend ->
    descend $ \fieldEs' rebuiltE ->
      case known of
        Nothing -> [| pure $rebuiltE |]
        Just ka -> bindTo "tm'" rebuiltE $ \tmE' -> ka ctxE tmE' fieldEs'

  InnerMost step -> fused step $ \goE known ctxE _tmE _fieldEs descend ->
    descend $ \fieldEs' rebuiltE ->
      case known of
        Nothing -> [| pure $rebuiltE |]
        Just ka -> bindTo "tm'" rebuiltE $ \tmE' ->
          withListen (ka ctxE tmE' fieldEs') $ \tE changedE ->
            condE changedE [| $goE $ctxE $tE |] [| pure $tE |]

  TopDownFix step -> do
    cs <- compileStep step
    genTopDownFix cs

  _ -> fail "genTraversal: not a traversal"
 where
  rebuildOnly :: [Q Exp] -> Q Exp -> Q Exp
  rebuildOnly _fieldEs' rebuiltE = [| pure $rebuiltE |]

  fused :: Step -> NodeArm -> Q Exp
  fused step mkArm = do
    cs <- compileStep step
    go <- newName "go"
    clauses <- traverse (goClause cs (varE go) mkArm) allShapes
    letE (csDecs cs <> [pure (FunD go clauses)]) (varE go)

  goClause :: CompiledStep -> Q Exp -> NodeArm -> Shape -> Q Clause
  goClause cs goE mkArm shape = do
    ctx <- newName "_ctx"
    is <- newName "_is"
    c <- newName "_c"
    tm <- newName "_tm"
    fieldNames <- shapeFieldNames shape
    let fieldEs = map varE fieldNames
        descend = genDescend shape goE (varE is, varE c) (varE tm) fieldEs
    body <- mkArm goE (csKnown cs shape) (varE ctx) (varE tm) fieldEs descend
    pats <- sequence
      [ asP ctx (conP 'TransformContext [varP is, varP c])
      , asP tm (conP (shapeCon shape) (map varP fieldNames))
      ]
    pure (Clause pats (NormalB body) [])

-- | 'Comb.topdownFixR' with the node program as @r@: a local @settle@
-- (@'Comb.repeatR' r@) plus the tryParent\/childChanged\/parentChanged loop
-- from Note [topdownFixR] in "Clash.Rewrite.Combinators", over a generated
-- 'Comb.allR' replica.
genTopDownFix :: CompiledStep -> Q Exp
genTopDownFix cs = do
  goFix <- newName "goFix"
  settle <- newName "settle"
  descendD <- newName "descendD"
  descendClauses <- traverse descendClause allShapes
  let settleDec = valD (varP settle) (normalB
        [| let loop ctx tm = do
                 (t1, w) <- Writer.listen ($(csDispatch cs) ctx tm)
                 if Monoid.getAny w then loop ctx t1 else pure t1
           in loop |]) []
      descendDec = pure (FunD descendD descendClauses)
      goFixDec = valD (varP goFix) (normalB
        [| let loop tryParent ctx tm = do
                 t1 <- if tryParent then $(varE settle) ctx tm else pure tm
                 (t2, w1) <- Writer.listen
                   ($(varE descendD) (loop True) ctx t1)
                 if Monoid.getAny w1
                   then do
                     (t3, w2) <- Writer.listen ($(varE settle) ctx t2)
                     if Monoid.getAny w2
                       then loop False ctx t3
                       else pure t3
                   else pure t2
           in loop |]) []
  letE (csDecs cs <> [settleDec, descendDec, goFixDec]) [| $(varE goFix) True |]
 where
  descendClause shape = do
    -- Leaf clauses never call the recursion argument; the underscore hint
    -- keeps -Wunused-matches quiet there.
    rec' <- newName (if leafShape shape then "_rec" else "rec'")
    is <- newName "_is"
    c <- newName "_c"
    tm <- newName "_tm"
    fieldNames <- shapeFieldNames shape
    body <- genDescend shape (varE rec') (varE is, varE c) (varE tm)
              (map varE fieldNames) (\_fieldEs' rebuiltE -> [| pure $rebuiltE |])
    pats <- sequence
      [ varP rec'
      , conP 'TransformContext [varP is, varP c]
      , asP tm (conP (shapeCon shape) (map varP fieldNames))
      ]
    pure (Clause pats (NormalB body) [])

--------------------------------------------------------------------------------
-- Strategies
--------------------------------------------------------------------------------

-- | Compile a strategy into its fused dispatch code. The result is an
-- expression of type @Rewrite extra@, with @extra@ fixed by the workers the
-- spec references.
compileStrategy :: Strat -> Q Exp
compileStrategy strat = case strat of
  Seq a b -> [| $(compileStrategy a) Comb.>-> $(compileStrategy b) |]
  DeepSeqS a b -> [| $(compileStrategy a) Comb.>-!-> $(compileStrategy b) |]
  OnChange a b -> [| $(compileStrategy a) Comb.!-> $(compileStrategy b) |]
  OnNoChange a b -> [| $(compileStrategy a) Comb.>-! $(compileStrategy b) |]
  Repeat a -> [| Comb.repeatR $(compileStrategy a) |]
  Pass name rewriteName -> [| apply $(stringE name) $(varE rewriteName) |]
  CallStrategy name -> varE name
  TopDown{} -> genTraversal strat
  BottomUp{} -> genTraversal strat
  TopDownFix{} -> genTraversal strat
  TopDownSuc{} -> genTraversal strat
  InnerMost{} -> genTraversal strat

-- | Compile a strategy into its unfused reference: the same node programs,
-- run through the "Clash.Rewrite.Combinators" traversals. Agrees with
-- 'compileStrategy' on results, transformation counts, and change flags; see
-- Note [chain semantics] for the only observable difference.
compileStrategyReference :: Strat -> Q Exp
compileStrategyReference = compileStrategyReferenceWith id

-- | 'compileStrategyReference' with a renaming applied to 'CallStrategy'
-- targets, so a reference-compiled strategy can call reference-compiled
-- versions of the strategies it references.
compileStrategyReferenceWith :: (Name -> Name) -> Strat -> Q Exp
compileStrategyReferenceWith rename = goStrat
 where
  goStrat strat = case strat of
    Seq a b -> [| $(goStrat a) Comb.>-> $(goStrat b) |]
    DeepSeqS a b -> [| $(goStrat a) Comb.>-!-> $(goStrat b) |]
    OnChange a b -> [| $(goStrat a) Comb.!-> $(goStrat b) |]
    OnNoChange a b -> [| $(goStrat a) Comb.>-! $(goStrat b) |]
    Repeat a -> [| Comb.repeatR $(goStrat a) |]
    Pass name rewriteName -> [| apply $(stringE name) $(varE rewriteName) |]
    CallStrategy name -> varE (rename name)
    TopDown step -> [| Comb.topdownR $(goStep step) |]
    BottomUp step -> [| Comb.bottomupR $(goStep step) |]
    TopDownFix step -> [| Comb.topdownFixR $(goStep step) |]
    TopDownSuc step -> [| Comb.topdownSucR $(goStep step) |]
    InnerMost step -> [| Comb.innerMost $(goStep step) |]

  goStep step = case step of
    SeqS a b -> [| $(goStep a) Comb.>-> $(goStep b) |]
    OnChangeS a b -> [| $(goStep a) Comb.!-> $(goStep b) |]
    OnNoChangeS a b -> [| $(goStep a) Comb.>-! $(goStep b) |]
    EmbedS strat -> goStrat strat
    Chain members -> do
      validateChain members
      -- Per constructor, the sequential apply chain over that constructor's
      -- bucket, each member guarded on its full entry table. See
      -- Note [chain semantics].
      nodeCaseExp $ \shape ->
        case [m | m <- members, Just _ <- [invokeAt shape m]] of
          [] -> Nothing
          bucket -> Just $ \ctxE nodeE _fieldEs ->
            [| $(foldr1 (\x y -> [| $x Comb.>-> $y |]) (map memberApplyE bucket))
                 $ctxE $nodeE |]

  memberApplyE member =
    [| apply $(stringE (specName member)) $(asRewriteQ member) |]

-- | A node program as a plain rewrite: match the constructor once and run
-- that constructor's program, without traversing. For composing a 'Step'
-- with plain combinators, and for testing chains in isolation.
dispatchQ :: Step -> Q Exp
dispatchQ step = do
  cs <- compileStep step
  letE (csDecs cs) (csDispatch cs)

-- | A transformation as a plain uninstrumented rewrite: match the term
-- against the entry table, run the worker on a match, return the term
-- untouched otherwise. Splice this where a transformation is invoked from
-- inside another transformation (any instrumentation is the caller's).
asRewriteQ :: TransformSpec -> Q Exp
asRewriteQ = guardExp invokeE
