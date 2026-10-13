{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Compile a "Clash.Rewrite.StrategyDSL" strategy into executable dispatch
  code. 'compileStrategy' produces the fused fast path: one constructor
  match per node drives both the dispatch of the node program and the descent
  into the node's children.

  = Target

  A strategy compiles to a transformation of scoped Clash Core (see
  "Clash.Rewrite.Scoped"). The 'Target' says how: the term constructors, how a
  transformation is called, the instrumentation and combinators, and how a
  traversal descends into a node. A traversal opens the binders it goes
  under.
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

import Clash.Rewrite.StrategyDSL
  ( Constructor (..), Step (..), Strat (..), Transformation (..)
  , TransformationWorker (..)
  )

import qualified Clash.Core.Scoped.Syntax as S
import qualified Clash.Rewrite.Scoped.Apply as SApply
import qualified Clash.Rewrite.Scoped.Combinators as SComb
import qualified Clash.Rewrite.Types as S
import qualified Control.Monad as Monad
import qualified Control.Monad.Writer as Writer
import qualified Data.Monoid as Monoid

-- * Targets

-- | One way to descend into a node: given a continuation receiving the
-- rebuilt node's fields and the rebuilt node itself, the descent.
type Descend = ([Q Exp] -> Q Exp -> Q Exp) -> Q Exp

-- | How a strategy is compiled, see the module header
data Target = Target
  { patterns :: Constructor -> [(Name, [String])]
  -- ^ The term constructors of a DSL constructor, with binder hints for their
  -- fields. Underscore-prefixed because not every generated arm uses every
  -- field.
  , wrap :: Q Exp -> Q Exp
  -- ^ Make a transformation of a @\\ctx tm -> ...@ lambda
  , call :: Q Exp -> Q Exp -> Q Exp
  -- ^ Apply a transformation to a context
  , applyWithName :: Name
  , applyName :: Name
  , seqName, deepSeqName, onChangeName, onNoChangeName, repeatName :: Name
  , topdownName, bottomupName, topdownFixName, topdownSucName, innerMostName :: Name
  , goDec :: Name -> (Constructor -> Q Exp -> Q Exp -> [Q Exp] -> Descend -> Q Exp) -> Q Dec
  -- ^ @goDec go body@: the declaration of a recursive traversal @go@, whose
  -- arm for a constructor is given by @body@, which gets the context, the
  -- node, its fields, and the descent into it.
  , descendDec :: Name -> Q Dec
  -- ^ The declaration of @descendD@, such that @descendD rec@ is the
  -- transformation applying @rec@ to the children of a node, like
  -- 'SComb.allR'
  , passE :: String -> Name -> Q Exp
  -- ^ A whole-term pass of a rewrite
  }

-- | Call a transformation on a context and a term
callE :: Target -> Q Exp -> Q Exp -> Q Exp -> Q Exp
callE target f ctxE tmE = [| $(target.call f ctxE) $tmE |]

-- | The target of scoped Clash Core
coreTarget :: Target
coreTarget = Target
  { patterns = corePatterns
  , wrap = \e -> [| S.Transform $e |]
  , call = \f ctxE -> [| S.runTransform $f $ctxE |]
  , applyWithName = 'SApply.applyWith
  , applyName = 'SApply.apply
  , seqName = '(SComb.>->)
  , deepSeqName = '(SComb.>-!->)
  , onChangeName = '(SComb.!->)
  , onNoChangeName = '(SComb.>-!)
  , repeatName = 'SComb.repeatR
  , topdownName = 'SComb.topdownR
  , bottomupName = 'SComb.bottomupR
  , topdownFixName = 'SComb.topdownFixR
  , topdownSucName = 'SComb.topdownSucR
  , innerMostName = 'SComb.innerMost
  , goDec = coreGoDec
  , descendDec = coreDescendDec
  , passE = \name rewriteName ->
      [| SApply.apply $(stringE name) (S.Transform $(varE rewriteName)) |]
  }

-- | The term constructors of scoped Core belonging to a 'Constructor'. A
-- variable is local ('S.Var') or global ('S.GlobalVar').
corePatterns :: Constructor -> [(Name, [String])]
corePatterns con = case con of
  CVar -> [('S.Var, ["_occ", "_ty", "_nm"]), ('S.GlobalVar, ["_g"])]
  CData -> [('S.Data, ["_dc"])]
  CLiteral -> [('S.Literal, ["_lit"])]
  CPrim -> [('S.Prim, ["_p"])]
  CLam -> [('S.Lam, ["_b", "_e"])]
  CTyLam -> [('S.TyLam, ["_b", "_e"])]
  CApp -> [('S.App, ["_fun", "_arg"])]
  CTyApp -> [('S.TyApp, ["_fun", "_argTy"])]
  CLet -> [('S.Let, ["_bnd", "_body"])]
  CCase -> [('S.Case, ["_subj", "_altsTy", "_alts"])]
  CCast -> [('S.Cast, ["_e", "_fromTy", "_toTy"])]
  CTick -> [('S.Tick, ["_tick", "_e"])]

allConstructors :: [Constructor]
allConstructors = [minBound .. maxBound]

-- | The term constructors of a target, with their DSL constructor
allPatterns :: Target -> [(Constructor, Name, [String])]
allPatterns target =
  [ (con, conName, hints)
  | con <- allConstructors
  , (conName, hints) <- target.patterns con
  ]

-- * Invoking workers

-- | How a bucket member is invoked once the constructor has matched.
data Invoke
  = InvokeFields Name -- ^ worker applied to ctx, node, and the node's fields
  | InvokeNode Name   -- ^ worker applied to ctx and the node only

-- | The member's invocation at this constructor, if it is in the
-- constructor's bucket.
invokeAt :: Target -> Constructor -> Transformation -> Maybe Invoke
invokeAt _target con transformation = go transformation.workers
 where
  go [] = Nothing
  go (FieldWorker con' worker : rest)
    | con' == con = Just (InvokeFields worker)
    | otherwise = go rest
  go (Worker con' worker : rest)
    | con' == con = Just (InvokeNode worker)
    | otherwise = go rest
  go (AnyWorker worker : _) = Just (InvokeNode worker)

invokeE :: Target -> Invoke -> Q Exp -> Q Exp -> [Q Exp] -> Q Exp
invokeE _ (InvokeFields worker) ctxE nodeE fieldEs =
  foldl appE (varE worker) (ctxE : nodeE : fieldEs)
invokeE _ (InvokeNode worker) ctxE nodeE _fieldEs =
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
-- ones, never both. A transformation of scoped Core has no 'FieldWorker' for
-- 'CVar', see "Clash.Rewrite.StrategyDSL".
validateTransformation :: Transformation -> Q ()
validateTransformation transformation
  | null workers = failWith "declares no workers"
  | any isAnyWorker workers, any (not . isAnyWorker) workers
  = failWith "mixes AnyWorker with per-constructor workers"
  | length (filter isAnyWorker workers) > 1 = failWith "has a duplicate AnyWorker"
  | con : _ <- duplicateConstructors = failWith ("has two workers for " <> show con)
  | any isVarFieldWorker workers
  = failWith "has a FieldWorker for CVar; use a Worker"
  | otherwise = pure ()
 where
  workers = transformation.workers

  failWith reason =
    fail ("compileStrategy: transformation " <> show transformation.name
          <> " " <> reason)

  isAnyWorker AnyWorker{} = True
  isAnyWorker _ = False

  isVarFieldWorker (FieldWorker CVar _) = True
  isVarFieldWorker _ = False

  duplicateConstructors =
    [ con
    | con : rest <- tails [c | Just c <- map workerConstructor workers]
    , con `elem` rest
    ]

  workerConstructor (FieldWorker con _) = Just con
  workerConstructor (Worker con _) = Just con
  workerConstructor (AnyWorker _) = Nothing

-- * Generating code

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

-- | A fresh name for a generated let-binding, derived from a transformation
-- name. Underscore-prefixed: not every generated binding ends up used.
freshBinding :: String -> Q Name
freshBinding hint = newName ('_' : map sanitize hint)
 where
  sanitize c = if isAlphaNum c then c else '_'

-- | @\\ctx tm -> case tm of { \<arm per constructor with a program\>;
-- _ -> pure tm }@, as a transformation of the target.
-- The wildcard is omitted when every constructor has an arm.
nodeCaseExp
  :: Target
  -> (Constructor -> Maybe (Q Exp -> Q Exp -> [Q Exp] -> Q Exp)) -> Q Exp
nodeCaseExp target armFor = do
  ctx <- newName "ctx"
  tm <- newName "tm"
  let arms =
        [ do fieldNames <- traverse newName hints
             match (conP conName (map varP fieldNames))
                   (normalB (armBody (varE ctx) (varE tm) (map varE fieldNames)))
                   []
        | (con, conName, hints) <- allPatterns target
        , Just armBody <- [armFor con]
        ]
      fallthrough
        | length arms == length (allPatterns target) = []
        | otherwise = [match wildP (normalB [| pure $(varE tm) |]) []]
  target.wrap (lamE [varP ctx, varP tm] (caseE (varE tm) (arms <> fallthrough)))

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

compileStep :: Target -> Step -> Q CompiledStep
compileStep target step = case step of
  Chain members -> compileChain target members
  SeqS a b -> compileStep2 a b seqKnown seqDispatch
  OnChangeS a b ->
    compileStep2 a b onChangeKnown (combinatorDispatch target.onChangeName)
  OnNoChangeS a b ->
    compileStep2 a b onNoChangeKnown (combinatorDispatch target.onNoChangeName)

  EmbedS strat -> do
    embedded <- freshBinding "embedded"
    let dec = valD (varP embedded) (normalB (compileStrategyFor target strat)) []
    pure CompiledStep
      { decs = [dec]
      , known = \_con ->
          Just (\ctxE nodeE _fieldEs -> callE target (varE embedded) ctxE nodeE)
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
  combinatorDispatch :: Name -> CompiledStep -> CompiledStep -> Q Exp
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
        condE changedE (callE target cb.dispatch ctxE tE) [| pure $tE |]
    (Just ka, Just kb) -> Just $ \ctxE nodeE fieldEs ->
      withListen (ka ctxE nodeE fieldEs) $ \tE changedE ->
        -- When the left side did not signal change the term is untouched,
        -- so the right side can still run on the statically matched fields.
        condE changedE (callE target cb.dispatch ctxE tE) (kb ctxE tE fieldEs)

  seqDispatch ca cb =
    target.wrap
      [| \ctx tm -> $(callE target ca.dispatch [| ctx |] [| tm |])
                      >>= $(target.call cb.dispatch [| ctx |]) |]

  onChangeKnown ca cb con = case ca.known con of
    Nothing -> Nothing
    Just ka -> Just $ \ctxE nodeE fieldEs ->
      withListen (ka ctxE nodeE fieldEs) $ \tE changedE ->
        condE changedE (callE target cb.dispatch ctxE tE) [| pure $tE |]

  onNoChangeKnown ca cb con = case (ca.known con, cb.known con) of
    (Nothing, kb) -> kb
    (Just ka, Nothing) -> Just ka
    (Just ka, Just kb) -> Just $ \ctxE nodeE fieldEs ->
      withListen (ka ctxE nodeE fieldEs) $ \tE changedE ->
        condE changedE [| pure $tE |] (kb ctxE tE fieldEs)

  combinatorDispatch op ca cb = [| $(varE op) $(ca.dispatch) $(cb.dispatch) |]

  compileStep2 a b mkKnown mkDispatch = do
    ca <- compileStep target a
    cb <- compileStep target b
    dispatch <- freshBinding "dispatch"
    let dispatchDec = valD (varP dispatch) (normalB (mkDispatch ca cb)) []
    pure CompiledStep
      { decs = ca.decs <> cb.decs <> [dispatchDec]
      , known = mkKnown ca cb
      , dispatch = varE dispatch
      }

compileChain :: Target -> [Transformation] -> Q CompiledStep
compileChain target members = do
  validateChain members
  guardNames <- traverse (\member -> freshBinding ("g_" <> member.name)) members
  let guardDecs = zipWith (memberGuardDec target) guardNames members
  dispatch <- freshBinding "dispatch"
  let -- The constructor's bucket, each member paired with the guards of all
      -- members after it in the chain (the member's guarded suffix).
      bucketAt con =
        [ (member, invoke, map snd rest)
        | (member, _guardName) : rest <- tails (zip members guardNames)
        , Just invoke <- [invokeAt target con member]
        ]
      known con = case bucketAt con of
        [] -> Nothing
        bucket -> Just (chainArm target bucket)
      dispatchDec = valD (varP dispatch) (normalB (nodeCaseExp target known)) []
  pure CompiledStep
    { decs = guardDecs <> [dispatchDec]
    , known
    , dispatch = varE dispatch
    }

-- | A bucket entered with the constructor statically known: run the members
-- in order on the original fields; after the first change, hand the term to
-- the remaining chain members' guards.
chainArm
  :: Target
  -> [(Transformation, Invoke, [Name])]
  -> Q Exp -> Q Exp -> [Q Exp] -> Q Exp
chainArm target bucket0 ctxE nodeE0 fieldEs = go nodeE0 bucket0
 where
  go :: Q Exp -> [(Transformation, Invoke, [Name])] -> Q Exp
  go nodeE [] = [| pure $nodeE |]
  go nodeE ((member, invoke, suffixGuards) : rest) = do
    t <- newName "t"
    changed <- newName "changed"
    doE
      [ bindS (tupP [varP t, varP changed])
          [| $(varE target.applyWithName) $(stringE member.name) $ctxE $nodeE
               $(invokeE target invoke ctxE nodeE fieldEs) |]
      , noBindS $ condE (varE changed)
          (guardedSuffix target suffixGuards ctxE (varE t))
          (go (varE t) rest)
      ]

-- | @g1 ctx t >>= g2 ctx >>= …@: offer the term to each remaining chain
-- member through its full worker table.
guardedSuffix :: Target -> [Name] -> Q Exp -> Q Exp -> Q Exp
guardedSuffix _ [] _ctxE nodeE = [| pure $nodeE |]
guardedSuffix target (g : gs) ctxE nodeE =
  foldl (\acc g' -> [| $acc >>= $(target.call (varE g') ctxE) |])
        (callE target (varE g) ctxE nodeE)
        gs

-- | One member's guard: match the term against the member's full worker
-- table; run the member instrumented on a match, return the term untouched
-- (without instrumentation) otherwise.
memberGuardDec :: Target -> Name -> Transformation -> Q Dec
memberGuardDec target guardName transformation =
  valD (varP guardName) (normalB (guardExp target instrumented transformation)) []
 where
  instrumented _target invoke ctxE nodeE fieldEs =
    [| fmap fst ($(varE target.applyWithName) $(stringE transformation.name) $ctxE $nodeE
                   $(invokeE target invoke ctxE nodeE fieldEs)) |]

-- | @\\ctx tm -> case tm of …@ over a transformation's worker table, with the
-- given wrapper around each invocation.
guardExp
  :: Target
  -> (Target -> Invoke -> Q Exp -> Q Exp -> [Q Exp] -> Q Exp)
  -> Transformation
  -> Q Exp
guardExp target wrapInvoke transformation =
  nodeCaseExp target $ \con ->
    wrapInvoke target <$> invokeAt target con transformation

-- ** Descent into a node of scoped Core

-- | The one-level descent for one constructor of scoped Core, replicating
-- 'Clash.Rewrite.Scoped.Combinators.allR': it opens the binders it goes under,
-- pushes the same frames, and gives the node itself when no child changed
-- (see Note [sharing unchanged nodes] in "Clash.Rewrite.Scoped.Combinators").
-- The continuation runs in the scope of the opened binders, where the
-- rebuilt fields are in scope. The rebuilt node it gets is evaluated.
genDescend
  :: Constructor
  -> Q Exp            -- ^ recursion: a 'S.Transform'
  -> Q Exp            -- ^ the context
  -> Q Exp            -- ^ the node
  -> [Q Exp]          -- ^ the node's fields
  -> ([Q Exp] -> Q Exp -> Q Exp)  -- ^ continuation: rebuilt fields, rebuilt node
  -> Q Exp
genDescend con go ctxE nodeE fieldEs continue = case (con, fieldEs) of
  (CVar, _) -> leaf
  (CData, _) -> leaf
  (CLiteral, _) -> leaf
  (CPrim, _) -> leaf

  (CLam, [b, e]) -> under 'S.underLam 'SComb.rebuildLam b e

  (CTyLam, [b, e]) -> under 'S.underTyLam 'SComb.rebuildTyLam b e

  (CApp, [fun, arg]) -> do
    fun' <- newName "fun'"
    arg' <- newName "arg'"
    doE
      [ bindS (varP fun') (rec' [| S.pushFrame S.AppFun $ctxE |] fun)
        -- The argument's context is computed from the already rewritten
        -- function, so the function has to be rewritten first.
      , bindS (varP arg')
          (rec' [| S.pushFrame (S.appArgFrame $(varE fun')) $ctxE |] arg)
      , noBindS $ rebuilt [varE fun', varE arg']
          [| SComb.rebuildApp $nodeE $fun $arg $(varE fun') $(varE arg') |]
      ]

  (CTyApp, [fun, argTy]) -> do
    fun' <- newName "fun'"
    doE
      [ bindS (varP fun') (rec' [| S.pushFrame S.TyAppC $ctxE |] fun)
      , noBindS $ rebuilt [varE fun', argTy]
          [| SComb.rebuildTyApp $nodeE $fun $(varE fun') $argTy |]
      ]

  (CLet, [bnd, body]) -> do
    bnd' <- newName "bnd'"
    body' <- newName "body'"
    [| SComb.descendLet $go $ctxE $bnd $body
         $(lamE [varP bnd', varP body']
             (rebuilt [varE bnd', varE body']
                [| SComb.rebuildLet $nodeE $bnd $body $(varE bnd') $(varE body') |])) |]

  (CCase, [subj, altsTy, alts]) -> do
    subj' <- newName "subj'"
    alts' <- newName "alts'"
    doE
      [ bindS (varP subj') (rec' [| S.pushFrame S.CaseScrut $ctxE |] subj)
      , bindS (varP alts')
          [| traverse
               (\alt -> S.underAlt $ctxE alt
                  (\ctxAlt p e -> do
                     e' <- S.runTransform $go ctxAlt e
                     pure $! SComb.rebuildAlt alt p e'))
               $alts |]
      , noBindS $ rebuilt [varE subj', altsTy, varE alts']
          [| SComb.rebuildCase $nodeE $subj $alts $(varE subj') $altsTy $(varE alts') |]
      ]

  (CCast, [e, fromTy, toTy]) -> do
    e' <- newName "e'"
    doE
      [ bindS (varP e') (rec' [| S.pushFrame S.CastBody $ctxE |] e)
      , noBindS $ rebuilt [varE e', fromTy, toTy]
          [| SComb.rebuildCast $nodeE $e $(varE e') $fromTy $toTy |]
      ]

  (CTick, [tick, e]) -> do
    e' <- newName "e'"
    doE
      [ bindS (varP e') (rec' [| S.pushFrame (S.TickC $tick) $ctxE |] e)
      , noBindS $ rebuilt [tick, varE e']
          [| SComb.rebuildTick $nodeE $e $tick $(varE e') |]
      ]

  _ -> fail ("genDescend: field count mismatch for " <> show con)
 where
  leaf = continue fieldEs nodeE

  rec' ctx' e = [| S.runTransform $go $ctx' $e |]

  -- The node itself when unchanged, a copy otherwise (one of the
  -- @SComb.rebuild*@ functions); evaluated, so that no thunk is allocated for
  -- it
  rebuilt fieldEs' rebuildE = do
    node' <- newName "node'"
    letE [valD (bangP (varP node')) (normalB rebuildE) []]
      (continue fieldEs' (varE node'))

  -- Open a binder, transform the body in its context, and continue in its
  -- scope
  under underFun rebuildName b e = do
    ctx' <- newName "ctx'"
    b' <- newName "b'"
    e' <- newName "e'"
    e'' <- newName "e''"
    [| $(varE underFun) $ctxE $b $e
         $(lamE [varP ctx', varP b', varP e']
             (doE
               [ bindS (varP e'') (rec' (varE ctx') (varE e'))
               , noBindS $ rebuilt [varE b', varE e'']
                   [| $(varE rebuildName) $nodeE $b $e $(varE b') $(varE e'') |]
               ])) |]

-- | A recursive traversal of scoped Core: a 'S.Transform' of a lambda
-- with a case-expression on the node
coreGoDec
  :: Name -> (Constructor -> Q Exp -> Q Exp -> [Q Exp] -> Descend -> Q Exp) -> Q Dec
coreGoDec go body = do
  ctx <- newName "_ctx"
  tm <- newName "_tm"
  let arm (con, conName, hints) = do
        fieldNames <- traverse newName hints
        let fieldEs = map varE fieldNames
            descend = genDescend con (varE go) (varE ctx) (varE tm) fieldEs
        match (conP conName (map varP fieldNames))
              (normalB (body con (varE ctx) (varE tm) fieldEs descend))
              []
  valD (varP go)
    (normalB [| S.Transform
                  $(lamE [varP ctx, varP tm]
                      (caseE (varE tm) (map arm (allPatterns coreTarget)))) |])
    []

-- | @descendD rec@, see 'descendDec'
coreDescendDec :: Name -> Q Dec
coreDescendDec descendD = do
  rec' <- newName "rec'"
  ctx <- newName "_ctx"
  tm <- newName "_tm"
  let arm (con, conName, hints) = do
        fieldNames <- traverse newName hints
        match (conP conName (map varP fieldNames))
              (normalB (genDescend con (varE rec') (varE ctx) (varE tm)
                          (map varE fieldNames)
                          (\_fieldEs' rebuiltE -> [| pure $rebuiltE |])))
              []
  valD (varP descendD)
    (normalB (lamE [varP rec']
      [| S.Transform
           $(lamE [varP ctx, varP tm]
               (caseE (varE tm) (map arm (allPatterns coreTarget)))) |]))
    []

-- ** Traversals

-- | What a traversal does at one node: given the recursion function, the
-- node program for the matched constructor ('Nothing' when the program is the
-- identity there), the context, the node, its fields, and a descent builder,
-- produce the body of @go@'s arm for that constructor.
type NodeArm
  =  Q Exp                                  -- ^ the traversal's @go@
  -> Maybe (Q Exp -> Q Exp -> [Q Exp] -> Q Exp)
  -> Q Exp                                  -- ^ ctx
  -> Q Exp                                  -- ^ the node
  -> [Q Exp]                                -- ^ its fields
  -> Descend                                -- ^ descend with a continuation
  -> Q Exp

-- | Generate a fused traversal: a recursive @go@ whose single constructor
-- match drives both the node program and the descent. The recursion
-- structure of each traversal is pinned to its
-- "Clash.Rewrite.Scoped.Combinators" counterpart, with the node program as
-- @r@:
--
-- * 'TopDown': @'SComb.topdownR' r = 'SComb.repeatR' r '>->' 'SComb.allR' …@. On
--   change, re-enter @go@ at this node (which also re-matches the
--   constructor); otherwise descend. See Note [topdown repeatR] in
--   "Clash.Rewrite.Scoped.Combinators".
-- * 'BottomUp': descend first, run the program once on the rebuilt node,
--   whose constructor is already known, so no re-match.
-- * 'TopDownSuc': run the program once; when it fired, stop, neither
--   repeating nor descending.
-- * 'InnerMost': bottom-up; when the program fires, re-enter @go@ on the
--   result until the innermost fixpoint is reached.
-- * 'TopDownFix': see 'genTopDownFix'.
genTraversal :: Target -> Strat -> Q Exp
genTraversal target strat = case strat of
  TopDown step -> fused step $ \goE known ctxE tmE fieldEs descend ->
    case known of
      Nothing -> descend rebuildOnly
      Just ka ->
        withListen (ka ctxE tmE fieldEs) $ \tE changedE ->
          condE changedE (callE target goE ctxE tE) (descend rebuildOnly)

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
        Just ka -> ka ctxE rebuiltE fieldEs'

  InnerMost step -> fused step $ \goE known ctxE _tmE _fieldEs descend ->
    descend $ \fieldEs' rebuiltE ->
      case known of
        Nothing -> [| pure $rebuiltE |]
        Just ka ->
          withListen (ka ctxE rebuiltE fieldEs') $ \tE changedE ->
            condE changedE (callE target goE ctxE tE) [| pure $tE |]

  TopDownFix step -> do
    cs <- compileStep target step
    genTopDownFix target cs

  _ -> fail "genTraversal: not a traversal"
 where
  rebuildOnly :: [Q Exp] -> Q Exp -> Q Exp
  rebuildOnly _fieldEs' rebuiltE = [| pure $rebuiltE |]

  fused :: Step -> NodeArm -> Q Exp
  fused step mkArm = do
    cs <- compileStep target step
    go <- newName "go"
    let goE = varE go
    letE
      (cs.decs <> [target.goDec go (\con -> mkArm goE (cs.known con))])
      goE

-- | 'SComb.topdownFixR' with the node program as @r@: a local @settle@
-- (@'SComb.repeatR' r@) plus the tryParent\/childChanged\/parentChanged loop
-- from Note [topdownFixR] in "Clash.Rewrite.Scoped.Combinators", over a
-- generated 'SComb.allR' replica.
genTopDownFix :: Target -> CompiledStep -> Q Exp
genTopDownFix target cs = do
  goFix <- newName "goFix"
  settle <- newName "settle"
  descendD <- newName "descendD"
  let call' f ctxE tmE = callE target f ctxE tmE
      settleDec = valD (varP settle) (normalB (target.wrap
        [| \ctx tm ->
             let loop t0 = do
                   (t1, w) <- Writer.listen $(call' cs.dispatch [| ctx |] [| t0 |])
                   if Monoid.getAny w then loop t1 else pure t1
             in loop tm |])) []
      goFixDec = valD (varP goFix) (normalB
        [| let loop tryParent = $(target.wrap
                 [| \ctx tm -> do
                      t1 <- if tryParent
                              then $(call' (varE settle) [| ctx |] [| tm |])
                              else pure tm
                      (t2, w1) <- Writer.listen
                        $(call' [| $(varE descendD) (loop True) |] [| ctx |] [| t1 |])
                      if Monoid.getAny w1
                        then do
                          (t3, w2) <- Writer.listen
                            $(call' (varE settle) [| ctx |] [| t2 |])
                          if Monoid.getAny w2
                            then $(call' [| loop False |] [| ctx |] [| t3 |])
                            else pure t3
                        else pure t2 |])
           in loop |]) []
  letE (cs.decs <> [settleDec, target.descendDec descendD, goFixDec])
    [| $(varE goFix) True |]

-- * Compiling strategies

-- | Compile a strategy for a target, see 'compileStrategy'
compileStrategyFor :: Target -> Strat -> Q Exp
compileStrategyFor target strat = case strat of
  Seq a b -> binop target.seqName a b
  DeepSeqS a b -> binop target.deepSeqName a b
  OnChange a b -> binop target.onChangeName a b
  OnNoChange a b -> binop target.onNoChangeName a b
  Repeat a -> [| $(varE target.repeatName) $(compileStrategyFor target a) |]
  Pass name rewriteName -> target.passE name rewriteName
  CallStrategy name -> varE name
  TopDown{} -> genTraversal target strat
  BottomUp{} -> genTraversal target strat
  TopDownFix{} -> genTraversal target strat
  TopDownSuc{} -> genTraversal target strat
  InnerMost{} -> genTraversal target strat
 where
  binop op a b =
    [| $(varE op) $(compileStrategyFor target a) $(compileStrategyFor target b) |]

-- | Compile a strategy into its unfused reference for a target, see
-- 'compileStrategyReference'
compileStrategyReferenceFor :: Target -> Strat -> Q Exp
compileStrategyReferenceFor target = goStrat
 where
  binop op a b = [| $(varE op) $a $b |]

  goStrat strat = case strat of
    Seq a b -> binop target.seqName (goStrat a) (goStrat b)
    DeepSeqS a b -> binop target.deepSeqName (goStrat a) (goStrat b)
    OnChange a b -> binop target.onChangeName (goStrat a) (goStrat b)
    OnNoChange a b -> binop target.onNoChangeName (goStrat a) (goStrat b)
    Repeat a -> [| $(varE target.repeatName) $(goStrat a) |]
    Pass name rewriteName -> target.passE name rewriteName
    CallStrategy name -> varE name
    TopDown step -> [| $(varE target.topdownName) $(goStep step) |]
    BottomUp step -> [| $(varE target.bottomupName) $(goStep step) |]
    TopDownFix step -> [| $(varE target.topdownFixName) $(goStep step) |]
    TopDownSuc step -> [| $(varE target.topdownSucName) $(goStep step) |]
    InnerMost step -> [| $(varE target.innerMostName) $(goStep step) |]

  goStep step = case step of
    SeqS a b -> binop target.seqName (goStep a) (goStep b)
    OnChangeS a b -> binop target.onChangeName (goStep a) (goStep b)
    OnNoChangeS a b -> binop target.onNoChangeName (goStep a) (goStep b)
    EmbedS strat -> goStrat strat
    Chain members -> do
      validateChain members
      -- The sequential apply chain over all members, each guarded on its full
      -- worker table.
      foldr1 (\x y -> binop target.seqName x y) (map memberApplyE members)

  memberApplyE member =
    [| $(varE target.applyName) $(stringE member.name)
         $(asRewriteQFor target member) |]

-- | A transformation as a plain uninstrumented rewrite for a target, see
-- 'asRewriteQ'
asRewriteQFor :: Target -> Transformation -> Q Exp
asRewriteQFor target transformation = do
  validateTransformation transformation
  guardExp target invokeE transformation

-- | A node program as a plain rewrite for a target, see 'dispatchQ'
dispatchQFor :: Target -> Step -> Q Exp
dispatchQFor target step = do
  cs <- compileStep target step
  letE cs.decs cs.dispatch

-- ** Scoped Clash Core

-- | Compile a strategy of transformations of scoped Core into its fused
-- dispatch code. The result is an expression of type
-- @'Clash.Rewrite.Types.Rewrite' extra@, with @extra@ fixed by
-- the workers the spec references.
compileStrategy :: Strat -> Q Exp
compileStrategy = compileStrategyFor coreTarget

-- | Compile a strategy into its unfused reference: the same transformations,
-- run as a plain @apply@ chain through the "Clash.Rewrite.Scoped.Combinators"
-- traversals
compileStrategyReference :: Strat -> Q Exp
compileStrategyReference = compileStrategyReferenceFor coreTarget

-- | A node program as a plain rewrite: match the constructor once and run
-- that constructor's program, without traversing. For composing a 'Step'
-- with plain combinators, and for testing chains in isolation.
dispatchQ :: Step -> Q Exp
dispatchQ = dispatchQFor coreTarget

-- | A transformation as a plain uninstrumented rewrite, an expression of type
-- @'Clash.Rewrite.Types.Rewrite' extra@: match the term against
-- the worker table, run the worker on a match, return the term untouched
-- otherwise. Splice this where a transformation is invoked from inside
-- another transformation (any instrumentation is the caller's).
asRewriteQ :: Transformation -> Q Exp
asRewriteQ = asRewriteQFor coreTarget
