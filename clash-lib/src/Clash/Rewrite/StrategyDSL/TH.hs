{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Compile a "Clash.Rewrite.StrategyDSL" strategy into executable dispatch
  code. 'compileStrategy' produces the fused fast path: one constructor match
  per node drives both the dispatch of the node program and the descent into
  the node's children.
-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Rewrite.StrategyDSL.TH
  ( compileStrategy
  , compileStrategyReference
  , dispatchQ
  , asRewriteQ
  ) where

import Data.Char (isAlphaNum)
import Data.Foldable (traverse_)
import Data.List (tails)
import Language.Haskell.TH hiding (Prim)

import Clash.Core.Term
  (Bind (..), CoreContext (..), Term (..), bindToList, patIds, primArg)
import Clash.Core.VarEnv (extendInScopeSet, extendInScopeSetList)
import Clash.Rewrite.StrategyDSL
  ( Constructor (..), Step (..), Strat (..), Transformation (..)
  , TransformationWorker (..)
  )
import Clash.Rewrite.Types (TransformContext (..))
import Clash.Rewrite.Util (apply, applyWith)

import qualified Clash.Rewrite.Combinators as Comb
import qualified Control.Monad as Monad
import qualified Control.Monad.Writer as Writer
import qualified Data.Monoid as Monoid

-- | The term constructor belonging to a 'Constructor'.
constructorName :: Constructor -> Name
constructorName con = case con of
  CVar -> 'Var
  CData -> 'Data
  CLiteral -> 'Literal
  CPrim -> 'Prim
  CLam -> 'Lam
  CTyLam -> 'TyLam
  CApp -> 'App
  CTyApp -> 'TyApp
  CLet -> 'Let
  CCase -> 'Case
  CCast -> 'Cast
  CTick -> 'Tick

-- | Binder hints for a constructor's fields. Underscore-prefixed because not
-- every generated arm uses every field.
constructorFieldHints :: Constructor -> [String]
constructorFieldHints con = case con of
  CVar -> ["_i"]
  CData -> ["_dc"]
  CLiteral -> ["_lit"]
  CPrim -> ["_p"]
  CLam -> ["_v", "_e"]
  CTyLam -> ["_tv", "_e"]
  CApp -> ["_fun", "_arg"]
  CTyApp -> ["_fun", "_argTy"]
  CLet -> ["_bnd", "_body"]
  CCase -> ["_subj", "_altsTy", "_alts"]
  CCast -> ["_e", "_fromTy", "_toTy"]
  CTick -> ["_tick", "_e"]

constructorFieldNames :: Constructor -> Q [Name]
constructorFieldNames = traverse newName . constructorFieldHints

allConstructors :: [Constructor]
allConstructors = [minBound .. maxBound]

-- | Constructors whose nodes have no subterms to descend into.
isLeafConstructor :: Constructor -> Bool
isLeafConstructor con = con `elem` [CVar, CData, CLiteral, CPrim]

-- | How a bucket member is invoked once the constructor has matched.
data Invoke
  = InvokeFields Name -- ^ worker applied to ctx, node, and the node's fields
  | InvokeNode Name   -- ^ worker applied to ctx and the node only

-- | The member's invocation at this constructor, if it is in the
-- constructor's bucket.
invokeAt :: Constructor -> Transformation -> Maybe Invoke
invokeAt con transformation = go transformation.workers
 where
  go [] = Nothing
  go (FieldWorker con' worker : rest)
    | con' == con = Just (InvokeFields worker)
    | otherwise = go rest
  go (Worker con' worker : rest)
    | con' == con = Just (InvokeNode worker)
    | otherwise = go rest
  go (AnyWorker worker : _) = Just (InvokeNode worker)

invokeE :: Invoke -> Q Exp -> Q Exp -> [Q Exp] -> Q Exp
invokeE (InvokeFields worker) ctxE nodeE fieldEs =
  foldl appE (varE worker) (ctxE : nodeE : fieldEs)
invokeE (InvokeNode worker) ctxE nodeE _fieldEs =
  [| $(varE worker) $ctxE $nodeE |]

validateChain :: [Transformation] -> Q ()
validateChain members = do
  Monad.when (null members) (fail "compileStrategy: empty chain")
  case [name | name : rest <- tails (map (.name) members), name `elem` rest] of
    name : _ ->
      fail ("compileStrategy: duplicate transformation name in one chain: "
            <> show name <> " (alias one occurrence with 'named')")
    [] -> pure ()
  traverse_ validateTransformation members

-- | A transformation's worker table must be non-empty, register at most one
-- worker per constructor, and not mix 'AnyWorker' with per-constructor
-- workers: a transformation either fires at any constructor or at specific
-- ones, never both.
validateTransformation :: Transformation -> Q ()
validateTransformation transformation
  | null workers = failWith "declares no workers"
  | any isAnyWorker workers, any (not . isAnyWorker) workers
  = failWith "mixes AnyWorker with per-constructor workers"
  | length (filter isAnyWorker workers) > 1 = failWith "has a duplicate AnyWorker"
  | con : _ <- duplicateConstructors = failWith ("has two workers for " <> show con)
  | otherwise = pure ()
 where
  workers = transformation.workers

  failWith reason =
    fail ("compileStrategy: transformation " <> show transformation.name
          <> " " <> reason)

  isAnyWorker AnyWorker{} = True
  isAnyWorker _ = False

  duplicateConstructors =
    [ con
    | con : rest <- tails [c | Just c <- map workerConstructor workers]
    , con `elem` rest
    ]

  workerConstructor (FieldWorker con _) = Just con
  workerConstructor (Worker con _) = Just con
  workerConstructor (AnyWorker _) = Nothing

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

-- | @\\ctx tm -> case tm of { \<arm per constructor with a program\>;
-- _ -> pure tm }@
-- The wildcard is omitted when every constructor has an arm.
nodeCaseExp
  :: (Constructor -> Maybe (Q Exp -> Q Exp -> [Q Exp] -> Q Exp)) -> Q Exp
nodeCaseExp armFor = do
  ctx <- newName "ctx"
  tm <- newName "tm"
  let arms =
        [ do fieldNames <- constructorFieldNames con
             match (conP (constructorName con) (map varP fieldNames))
                   (normalB (armBody (varE ctx) (varE tm) (map varE fieldNames)))
                   []
        | con <- allConstructors
        , Just armBody <- [armFor con]
        ]
      fallthrough
        | length arms == length allConstructors = []
        | otherwise = [match wildP (normalB [| pure $(varE tm) |]) []]
  lamE [varP ctx, varP tm] (caseE (varE tm) (arms <> fallthrough))

-- | A compiled node program. 'known' is the program entered with the
-- node's constructor statically known and its fields in scope; 'Nothing'
-- means the program is the identity at that constructor. 'dispatch'
-- references a generated binding running the program on a term of unknown
-- constructor.
data CompiledStep = CompiledStep
  { decs :: [Q Dec]
  , known :: Constructor -> Maybe (Q Exp -> Q Exp -> [Q Exp] -> Q Exp)
  , dispatch :: Q Exp
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
      { decs = [dec]
      , known = \_con ->
          Just (\ctxE nodeE _fieldEs -> [| $(varE embedded) $ctxE $nodeE |])
      , dispatch = varE embedded
      }
 where
  -- These need signatures: without them the record selectors below are
  -- inferred as 'HasField' constraints rather than resolved against
  -- 'CompiledStep'.
  seqKnown, onChangeKnown, onNoChangeKnown
    :: CompiledStep -> CompiledStep -> Constructor
    -> Maybe (Q Exp -> Q Exp -> [Q Exp] -> Q Exp)
  seqDispatch :: CompiledStep -> CompiledStep -> Q Exp
  combinatorDispatch :: Q Exp -> CompiledStep -> CompiledStep -> Q Exp
  compileStep2
    :: Step -> Step
    -> (CompiledStep -> CompiledStep -> Constructor
        -> Maybe (Q Exp -> Q Exp -> [Q Exp] -> Q Exp))
    -> (CompiledStep -> CompiledStep -> Q Exp)
    -> Q CompiledStep

  seqKnown ca cb con = case (ca.known con, cb.known con) of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just kb) -> Just kb
    (Just ka, Nothing) -> Just $ \ctxE nodeE fieldEs ->
      withListen (ka ctxE nodeE fieldEs) $ \tE changedE ->
        condE changedE [| $(cb.dispatch) $ctxE $tE |] [| pure $tE |]
    (Just ka, Just kb) -> Just $ \ctxE nodeE fieldEs ->
      withListen (ka ctxE nodeE fieldEs) $ \tE changedE ->
        -- When the left side did not signal change the term is untouched,
        -- so the right side can still run on the statically matched fields.
        condE changedE [| $(cb.dispatch) $ctxE $tE |] (kb ctxE tE fieldEs)

  seqDispatch ca cb =
    [| \ctx tm -> $(ca.dispatch) ctx tm >>= $(cb.dispatch) ctx |]

  onChangeKnown ca cb con = case ca.known con of
    Nothing -> Nothing
    Just ka -> Just $ \ctxE nodeE fieldEs ->
      withListen (ka ctxE nodeE fieldEs) $ \tE changedE ->
        condE changedE [| $(cb.dispatch) $ctxE $tE |] [| pure $tE |]

  onNoChangeKnown ca cb con = case (ca.known con, cb.known con) of
    (Nothing, kb) -> kb
    (Just ka, Nothing) -> Just ka
    (Just ka, Just kb) -> Just $ \ctxE nodeE fieldEs ->
      withListen (ka ctxE nodeE fieldEs) $ \tE changedE ->
        condE changedE [| pure $tE |] (kb ctxE tE fieldEs)

  combinatorDispatch op ca cb = [| $op $(ca.dispatch) $(cb.dispatch) |]

  compileStep2 a b mkKnown mkDispatch = do
    ca <- compileStep a
    cb <- compileStep b
    dispatch <- freshBinding "dispatch"
    let dispatchDec = valD (varP dispatch) (normalB (mkDispatch ca cb)) []
    pure CompiledStep
      { decs = ca.decs <> cb.decs <> [dispatchDec]
      , known = mkKnown ca cb
      , dispatch = varE dispatch
      }

compileChain :: [Transformation] -> Q CompiledStep
compileChain members = do
  validateChain members
  guardNames <- traverse (\member -> freshBinding ("g_" <> member.name)) members
  let guardDecs = zipWith memberGuardDec guardNames members
  dispatch <- freshBinding "dispatch"
  let -- The constructor's bucket, each member paired with the guards of all
      -- members after it in the chain (the member's guarded suffix).
      bucketAt con =
        [ (member, invoke, map snd rest)
        | (member, _guardName) : rest <- tails (zip members guardNames)
        , Just invoke <- [invokeAt con member]
        ]
      known con = case bucketAt con of
        [] -> Nothing
        bucket -> Just (chainArm bucket)
      dispatchDec = valD (varP dispatch) (normalB (nodeCaseExp known)) []
  pure CompiledStep
    { decs = guardDecs <> [dispatchDec]
    , known
    , dispatch = varE dispatch
    }

-- | A bucket entered with the constructor statically known: run the members
-- in order on the original fields; after the first change, hand the term to
-- the remaining chain members' guards.
chainArm
  :: [(Transformation, Invoke, [Name])]
  -> Q Exp -> Q Exp -> [Q Exp] -> Q Exp
chainArm bucket0 ctxE nodeE0 fieldEs = go nodeE0 bucket0
 where
  go :: Q Exp -> [(Transformation, Invoke, [Name])] -> Q Exp
  go nodeE [] = [| pure $nodeE |]
  go nodeE ((member, invoke, suffixGuards) : rest) = do
    t <- newName "t"
    changed <- newName "changed"
    doE
      [ bindS (tupP [varP t, varP changed])
          [| applyWith $(stringE member.name) $ctxE $nodeE
               $(invokeE invoke ctxE nodeE fieldEs) |]
      , noBindS $ condE (varE changed)
          (guardedSuffix suffixGuards ctxE (varE t))
          (go (varE t) rest)
      ]

-- | @g1 ctx t >>= g2 ctx >>= …@: offer the term to each remaining chain
-- member through its full worker table.
guardedSuffix :: [Name] -> Q Exp -> Q Exp -> Q Exp
guardedSuffix [] _ctxE nodeE = [| pure $nodeE |]
guardedSuffix (g : gs) ctxE nodeE =
  foldl (\acc g' -> [| $acc >>= $(varE g') $ctxE |])
        [| $(varE g) $ctxE $nodeE |]
        gs

-- | One member's guard: match the term against the member's full worker
-- table; run the member instrumented on a match, return the term untouched
-- (without instrumentation) otherwise.
memberGuardDec :: Name -> Transformation -> Q Dec
memberGuardDec guardName transformation =
  valD (varP guardName) (normalB (guardExp instrumented transformation)) []
 where
  instrumented invoke ctxE nodeE fieldEs =
    [| fmap fst (applyWith $(stringE transformation.name) $ctxE $nodeE
                   $(invokeE invoke ctxE nodeE fieldEs)) |]

-- | @\\ctx tm -> case tm of …@ over a transformation's worker table, with the
-- given wrapper around each invocation.
guardExp
  :: (Invoke -> Q Exp -> Q Exp -> [Q Exp] -> Q Exp)
  -> Transformation
  -> Q Exp
guardExp wrap transformation =
  nodeCaseExp $ \con -> wrap <$> invokeAt con transformation

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
  :: Constructor
  -> Q Exp            -- ^ recursion function @:: TransformContext -> Term -> m Term@
  -> (Q Exp, Q Exp)   -- ^ the context's InScopeSet and CoreContext stack
  -> Q Exp            -- ^ the node
  -> [Q Exp]          -- ^ the node's fields
  -> ([Q Exp] -> Q Exp -> Q Exp)  -- ^ continuation: rebuilt fields, rebuilt node
  -> Q Exp
genDescend con go (isE, cE) nodeE fieldEs continue = case (con, fieldEs) of
  (CVar, _) -> leaf
  (CData, _) -> leaf
  (CLiteral, _) -> leaf
  (CPrim, _) -> leaf

  (CLam, [v, e]) -> do
    e' <- newName "e'"
    doE
      [ bindS (varP e')
          [| $go (TransformContext (extendInScopeSet $isE $v) (LamBody $v : $cE)) $e |]
      , continueWith [v, varE e'] [| Lam $v $(varE e') |]
      ]

  (CTyLam, [tv, e]) -> do
    e' <- newName "e'"
    doE
      [ bindS (varP e')
          [| $go (TransformContext (extendInScopeSet $isE $tv) (TyLamBody $tv : $cE)) $e |]
      , continueWith [tv, varE e'] [| TyLam $tv $(varE e') |]
      ]

  (CApp, [fun, arg]) -> do
    fun' <- newName "fun'"
    arg' <- newName "arg'"
    doE
      [ bindS (varP fun') [| $go (TransformContext $isE (AppFun : $cE)) $fun |]
        -- The argument's context is computed from the already rewritten
        -- function, so the function has to be rewritten first.
      , bindS (varP arg')
          [| $go (TransformContext $isE (AppArg (primArg $(varE fun')) : $cE)) $arg |]
      , continueWith [varE fun', varE arg'] [| App $(varE fun') $(varE arg') |]
      ]

  (CTyApp, [fun, argTy]) -> do
    fun' <- newName "fun'"
    doE
      [ bindS (varP fun') [| $go (TransformContext $isE (TyAppC : $cE)) $fun |]
      , continueWith [varE fun', argTy] [| TyApp $(varE fun') $argTy |]
      ]

  (CLet, [bnd, body]) -> do
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

  (CCase, [subj, altsTy, alts]) -> do
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

  (CCast, [e, fromTy, toTy]) -> do
    e' <- newName "e'"
    doE
      [ bindS (varP e') [| $go (TransformContext $isE (CastBody : $cE)) $e |]
      , continueWith [varE e', fromTy, toTy] [| Cast $(varE e') $fromTy $toTy |]
      ]

  (CTick, [tick, e]) -> do
    e' <- newName "e'"
    doE
      [ bindS (varP e') [| $go (TransformContext $isE (TickC $tick : $cE)) $e |]
      , continueWith [tick, varE e'] [| Tick $tick $(varE e') |]
      ]

  _ -> fail ("genDescend: field count mismatch for " <> show con)
 where
  leaf = continue fieldEs nodeE
  continueWith fieldEs' rebuiltE = noBindS (continue fieldEs' rebuiltE)

-- | What a traversal does at one node: given the recursion function, the
-- node program for the matched constructor ('Nothing' when the program is the
-- identity there), the context, the node, its fields, and a descent builder,
-- produce the body of @go@'s clause for that constructor.
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
-- * 'TopDown': @'Comb.topdownR' r = 'Comb.repeatR' r '>->' 'Comb.allR' …@. On
--   change, re-enter @go@ at this node (which also re-matches the
--   constructor); otherwise descend. See Note [topdown repeatR] in
--   "Clash.Rewrite.Combinators".
-- * 'BottomUp': descend first, run the program once on the rebuilt node,
--   whose constructor is already known, so no re-match.
-- * 'TopDownSuc': run the program once; when it fired, stop, neither
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
    clauses <- traverse (goClause cs (varE go) mkArm) allConstructors
    letE (cs.decs <> [pure (FunD go clauses)]) (varE go)

  goClause :: CompiledStep -> Q Exp -> NodeArm -> Constructor -> Q Clause
  goClause cs goE mkArm con = do
    ctx <- newName "_ctx"
    is <- newName "_is"
    c <- newName "_c"
    tm <- newName "_tm"
    fieldNames <- constructorFieldNames con
    let fieldEs = map varE fieldNames
        descend = genDescend con goE (varE is, varE c) (varE tm) fieldEs
    body <- mkArm goE (cs.known con) (varE ctx) (varE tm) fieldEs descend
    pats <- sequence
      [ asP ctx (conP 'TransformContext [varP is, varP c])
      , asP tm (conP (constructorName con) (map varP fieldNames))
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
  descendClauses <- traverse descendClause allConstructors
  let settleDec = valD (varP settle) (normalB
        [| let loop ctx tm = do
                 (t1, w) <- Writer.listen ($(cs.dispatch) ctx tm)
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
  letE (cs.decs <> [settleDec, descendDec, goFixDec]) [| $(varE goFix) True |]
 where
  descendClause con = do
    -- Leaf clauses never call the recursion argument; the underscore hint
    -- keeps -Wunused-matches quiet there.
    rec' <- newName (if isLeafConstructor con then "_rec" else "rec'")
    is <- newName "_is"
    c <- newName "_c"
    tm <- newName "_tm"
    fieldNames <- constructorFieldNames con
    body <- genDescend con (varE rec') (varE is, varE c) (varE tm)
              (map varE fieldNames) (\_fieldEs' rebuiltE -> [| pure $rebuiltE |])
    pats <- sequence
      [ varP rec'
      , conP 'TransformContext [varP is, varP c]
      , asP tm (conP (constructorName con) (map varP fieldNames))
      ]
    pure (Clause pats (NormalB body) [])

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

-- | Compile a strategy into its unfused reference: the same transformations,
-- run as a plain @apply@ chain through the "Clash.Rewrite.Combinators"
-- traversals. Agrees with 'compileStrategy' on results, transformation
-- counts, and change flags.
compileStrategyReference :: Strat -> Q Exp
compileStrategyReference = goStrat
 where
  goStrat strat = case strat of
    Seq a b -> [| $(goStrat a) Comb.>-> $(goStrat b) |]
    DeepSeqS a b -> [| $(goStrat a) Comb.>-!-> $(goStrat b) |]
    OnChange a b -> [| $(goStrat a) Comb.!-> $(goStrat b) |]
    OnNoChange a b -> [| $(goStrat a) Comb.>-! $(goStrat b) |]
    Repeat a -> [| Comb.repeatR $(goStrat a) |]
    Pass name rewriteName -> [| apply $(stringE name) $(varE rewriteName) |]
    CallStrategy name -> varE name
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
      -- The sequential apply chain over all members, each guarded on its full
      -- worker table.
      foldr1 (\x y -> [| $x Comb.>-> $y |]) (map memberApplyE members)

  memberApplyE member =
    [| apply $(stringE member.name) $(asRewriteQ member) |]

-- | A node program as a plain rewrite: match the constructor once and run
-- that constructor's program, without traversing. For composing a 'Step'
-- with plain combinators, and for testing chains in isolation.
dispatchQ :: Step -> Q Exp
dispatchQ step = do
  cs <- compileStep step
  letE cs.decs cs.dispatch

-- | A transformation as a plain uninstrumented rewrite: match the term
-- against the worker table, run the worker on a match, return the term
-- untouched otherwise. Splice this where a transformation is invoked from
-- inside another transformation (any instrumentation is the caller's).
asRewriteQ :: Transformation -> Q Exp
asRewriteQ transformation = do
  validateTransformation transformation
  guardExp invokeE transformation
