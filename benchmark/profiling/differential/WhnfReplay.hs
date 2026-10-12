{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Replays calls of 'Clash.Rewrite.Util.whnfRW' with the evaluator of Clash
  Core and with that of scoped Clash Core ("Clash.Core.Scoped.Evaluator"),
  compares their results, and measures the allocation of both.

  The calls are read from a dump: a sequence of records, each of which is the
  length (a big-endian 'Int64') of the 'Binary' encoding of

  @
  (Bool, [[LetBinding]], Term)
  @

  that is: whether the term is the subject of a case-expression, the
  let-bindings of the 'Clash.Core.Term.LetBody' frames of its context
  (innermost first), and the term. The instrumented @whnfRW@ of the evaluator
  spike (branch @eval-spike@, commit @b8bd16587@) writes them, with
  @WHNF_DUMP=FILE:N@ for every N-th call.

  = Comparing

  The current side evaluates the term with the let-bindings of the context
  on the heap, like 'Clash.Rewrite.Util.whnfHeap', and binds the bindings it
  allocated around the result, like 'Clash.Rewrite.Util.heapLetBindings'.

  The scoped side converts the context to scoped Core as nested recursive
  let-expressions around the term, and goes down to the term with
  'Clash.Rewrite.Scoped.Types.underLetRec', which puts their bindings on the
  heap of the context. It evaluates the term with that heap, and binds the
  bindings it allocated around the result
  ('Clash.Core.Scoped.Evaluator.bindAllocated').

  The results are compared up to alpha-equivalence, after unfolding the
  bindings around them a few times (like the spike did): the two sides name
  and order the bindings they allocate differently, and the current side
  keeps the value a binding was updated with, where the scoped one may keep
  the right-hand side it allocated. Known differences:

  * __coercions__: the coercion arguments of data constructors that the
    scoped primitive reductions build are applications of @_CO_@ to their
    type (see 'Clash.Core.Scoped.Util.primCo'), those of Clash Core have the
    type in the primitive.

  = Primitives

  With @--prims@, it reports per primitive how often the scoped evaluator
  reduced it with a ported reduction ("Clash.Core.Scoped.Evaluator.Primitives")
  and through the adapter, and how many calls used the adapter at all. With
  @--call N@, it also lists the reductions of that call in order. With
  @--check-prims@, it also runs the adapter on every call of a ported
  reduction, and compares the results ("Test.Clash.Scoped.Evaluator"): that
  checks the ported reductions on the arguments they get in real designs.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Repeated runs of a workload must not share work
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}

module WhnfReplay (ReplayOptions (..), whnfReplay) where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, when)
import qualified Control.Lens as Lens
import Control.Monad.State.Strict (runState)
import Data.Binary (decode)
import Data.Binary.Get (getInt64be, runGetOrFail)
import qualified Data.ByteString.Lazy as B
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getAllocationCounter)
import GHC.Stats (RTSStats (allocated_bytes), getRTSStats, getRTSStatsEnabled)
import System.IO (hFlush, stdout)
import System.Mem (performMinorGC)
import System.Timeout (timeout)
import Text.Printf (printf)

import qualified Clash.Core.Evaluator.Types as C
import qualified Clash.Core.FreeVars as C
import qualified Clash.Core.Pretty as C (showPpr)
import qualified Clash.Core.Term as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import qualified Clash.Core.VarEnv as C
import Clash.Driver.Types (Binding (..), BindingMap)
import Clash.Rewrite.Util (contextLetBindings, heapLetBindings)
import qualified Clash.Util.Supply as Supply

import Clash.GHC.Evaluator (evaluator)

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Evaluator (bindAllocated, whnf)
import Test.Clash.Scoped.Evaluator.Adapter
  (clashVarsIds, clashVarsTyVars, emptyClashVars, toClashTermWith)
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Subst (teleIdVars)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TypeEnv (addGlobalTypes, mkTypeEnv)
import Clash.Rewrite.Scoped.Types (TransformContext, rootContext, tfHeap, tfScope, underLetRec)
import Clash.Rewrite.Scoped.Whnf (evaluatorEnv)

import Test.Clash.Scoped.Evaluator (canonicalCoercions, readback, termSize)
import qualified Test.Clash.Scoped.Evaluator as P

-- | A dumped 'whnfRW' call
data Call = Call
  { callSubj :: !Bool
  , callCtx :: [[C.LetBinding]]
  -- ^ The let-bindings of the 'C.LetBody' frames of the context, innermost
  -- first
  , callTerm :: C.Term
  }

readCalls :: FilePath -> IO [Call]
readCalls file = go <$> B.readFile file
 where
  go bs
    | B.null bs = []
    | otherwise = case runGetOrFail getInt64be bs of
        Left (_, _, err) -> error err
        Right (rest, _, len) ->
          let (rec, rest') = B.splitAt (fromIntegral (len :: Int64)) rest
              (s, c, t) = decode rec
          in Call s c t : go rest'

-- | The context of a call, as nested recursive let-expressions around its
-- term
callBinding :: Call -> C.Term
callBinding c = List.foldl' (\acc ls -> C.Let (C.Rec ls) acc) (callTerm c) (callCtx c)

-- | The free variables of a call
callFreeVars :: Call -> ([C.TyVar], [C.Id])
callFreeVars c = (C.dependencySortTyVars (nub tvs), nub ids)
 where
  t = callBinding c
  ids = Lens.toListOf C.freeLocalIds t
  tvs = Lens.toListOf C.termFreeTyVars t ++ concatMap (Lens.toListOf C.typeFreeVars . C.varType) ids
  nub :: [C.Var a] -> [C.Var a]
  nub = List.nubBy (\a b -> C.varUniq a == C.varUniq b)

-- * The current side

-- | The inputs of the current evaluator
data CurInput = CurInput !Bool !C.PureHeap !C.InScopeSet C.Term

curInput :: Call -> CurInput
curInput c =
  CurInput (callSubj c)
    (contextLetBindings (map C.LetBody (callCtx c)) (callTerm c))
    (C.mkInScopeSet (C.mkVarSet tvs <> C.mkVarSet ids <> C.mkVarSet binders))
    (callTerm c)
 where
  (tvs, ids) = callFreeVars c
  binders = map fst (concat (callCtx c))

runCurrent :: C.TyConMap -> BindingMap -> Supply.Supply -> CurInput -> C.Term
runCurrent tcm bm sup (CurInput isSubj lh is0 t) =
  let (_, ph, v) = C.whnf' evaluator bm lh tcm (IntMap.empty, 0) sup is0 isSubj t
      new = ph `C.differenceVarEnv` lh
  in if C.nullVarEnv new then v else C.Letrec (heapLetBindings tcm new) v

-- * The scoped side

-- | A call converted to scoped Core: the context of its term, the term, and
-- the scoped variables of the free variables and the let-binders of the
-- context
data ScopedCall where
  ScopedCall
    :: Distinct n
    => !Bool -> TransformContext n -> Term n -> [(C.TyVar, Type n)] -> [(C.Id, Term n)]
    -> ScopedCall

toScopedCall :: Call -> ScopedCall
toScopedCall c = withConvEnv tvs ids $ \env ->
  let sc = convScope env
      tvsS = [(tv, fromClashType env (C.VarTy tv)) | tv <- tvs]
      idsS = [(i, fromClashTerm env (C.Var i)) | i <- ids]
  in go (rootContext sc) (fromClashTerm env (callBinding c)) (reverse (callCtx c)) tvsS idsS
 where
  (tvs, ids) = callFreeVars c

  go :: Distinct n
     => TransformContext n -> Term n -> [[C.LetBinding]]
     -> [(C.TyVar, Type n)] -> [(C.Id, Term n)] -> ScopedCall
  go ctx t [] tvsS idsS = ScopedCall (callSubj c) ctx t tvsS idsS
  go ctx (Let (Rec bs xs) body) (ls : rest) tvsS idsS =
    underLetRec ctx bs xs body $ \ctx' _ bs' _ body' ->
      go ctx' body' rest (sinkPairs tvsS)
        (zip (map fst ls) (teleIdVars bs') ++ sinkPairs idsS)
  go _ _ _ _ _ = error "toScopedCall: expected a recursive let-expression"

  sinkPairs :: (Sinkable e, DExt n l) => [(a, e n)] -> [(a, e l)]
  sinkPairs = map (fmap sink)

-- | The result of the scoped evaluator, with the bindings it allocated bound
-- around it
runScoped
  :: Distinct n => Env -> BindingMap -> Bool -> TransformContext n -> Term n -> Term n
runScoped env bm isSubj ctx e =
  whnf env isSubj (initMachine (tfScope ctx) (tfHeap ctx) bm (IntMap.empty, 0) e) $ \m ->
    bindAllocated m (mTerm m)

-- * Comparing

-- | Number of term nodes of a scoped term
scopedSize :: Term n -> Int
scopedSize = go 0
 where
  go :: Int -> Term x -> Int
  go !acc = \case
    Lam _ e -> go (acc + 1) e
    TyLam _ e -> go (acc + 1) e
    App e1 e2 -> go (go (acc + 1) e1) e2
    TyApp e _ -> go (acc + 1) e
    Let (NonRec _ x) e -> go (go (acc + 1) x) e
    Let (Rec _ xs) e -> go (List.foldl' go (acc + 1) xs) e
    Case e _ alts -> List.foldl' (\a (Alt _ r) -> go a r) (go (acc + 1) e) alts
    Cast e _ _ -> go (acc + 1) e
    Tick _ e -> go (acc + 1) e
    _ -> acc + 1

data Outcome = Agree | AgreeKnown | Differ | Failed String
  deriving (Eq, Ord, Show)

instance NFData Outcome where
  rnf = \case
    Failed s -> rnf s
    _ -> ()

-- | Compare the results of a call. Gives both results, normalized, if they
-- differ.
compareCall
  :: C.TyConMap -> BindingMap -> Supply.Supply -> Env -> Call
  -> (Outcome, Maybe (String, String))
compareCall tcm bm sup env c = case toScopedCall c of
  ScopedCall isSubj ctx e tvs ids ->
    let sc = tfScope ctx
        rS = runScoped env bm isSubj ctx e
        (rSC, vars) = runState (toClashTermWith rS) emptyClashVars
        rC = runCurrent tcm bm sup (curInput c)
        backS f = fromClashTermWith sc (clashVarsTyVars vars) (clashVarsIds vars) (f (readback 2 rSC))
        backC f = fromClashTermWith sc tvs ids (f (readback 2 rC))
    in if aeqTm (backS id) (backC id) then (Agree, Nothing)
       else if aeqTm (backS canonicalCoercions) (backC canonicalCoercions) then (AgreeKnown, Nothing)
       else (Differ, Just (C.showPpr (readback 2 rC), C.showPpr (readback 2 rSC)))

-- | The options of 'whnfReplay'
data ReplayOptions = ReplayOptions
  { roReps :: Int
  -- ^ Repetitions of the measurements
  , roShow :: Int
  -- ^ Number of differences to show
  , roTop :: Int
  -- ^ Number of calls with the most extra allocation to show
  , roOnly :: Maybe Int
  -- ^ Only replay the call with this index
  , roPrims :: Bool
  -- ^ Report the reductions of primitives, see the module header
  , roCheckPrims :: Bool
  -- ^ Compare every call of a ported reduction with the adapter
  }

-- | Replay the calls of a dump, see the module header
whnfReplay
  :: ReplayOptions
  -> C.TyConMap
  -> BindingMap
  -> FilePath
  -- ^ The dump
  -> IO ()
whnfReplay opts tcm bm dumpFile = do
  let reps = roReps opts
      nShow = roShow opts
      nTop = roTop opts
      recording = roPrims opts || roCheckPrims opts
  sup <- Supply.newSupply
  calls0 <- readCalls dumpFile
  let calls = maybe calls0 (\k -> [calls0 !! k]) (roOnly opts)
  printf "%s: %d calls (%d as case subject)\n" dumpFile (length calls)
    (length (filter callSubj calls))
  hFlush stdout
  let te = addGlobalTypes (fmap bindingId bm) (mkTypeEnv tcm)
      env = evaluatorEnv te
  eventsRef <- newIORef []
  let checkEnv
        | recording = P.recordingEnv eventsRef (roCheckPrims opts) te evaluator
        | otherwise = env

  -- Correctness
  results <- forM (zip [0 :: Int ..] calls) $ \(i, c) -> do
    writeIORef eventsRef []
    r <- try $ timeout 20000000 $ evaluate $ force $ compareCall tcm bm sup checkEnv c
    events <- readIORef eventsRef >>= mapM forceEvent . reverse
    pure $ case r of
      Left (ex :: SomeException) -> (i, Failed (takeWhile (/= '\n') (show ex)), Nothing, events)
      Right Nothing -> (i, Failed "timeout", Nothing, events)
      Right (Just (o, d)) -> (i, o, d, events)
  let byOutcome = Map.fromListWith (+) [(outcomeKey o, 1 :: Int) | (_, o, _, _) <- results]
  forM_ (Map.toList byOutcome) $ \(k, cnt) -> printf "  %-50s %7d\n" k cnt
  forM_ (take nShow [(i, d) | (i, Differ, Just d, _) <- results]) $ \(i, (cur, sco)) ->
    printf "  -- call %d differs\n  current: %s\n  scoped:  %s\n" i
      (take 1500 (unwords (words cur))) (take 1500 (unwords (words sco)))
  when recording $
    reportPrims opts [(i, events) | (i, _, _, events) <- results]
  hFlush stdout

  -- Allocation, on the calls on which both succeeded
  statsOn <- getRTSStatsEnabled
  when statsOn $ do
    let okCalls = [c | ((_, o, _, _), c) <- zip results calls, o `elem` [Agree, AgreeKnown, Differ]]
        cis = map curInput okCalls
        scs = map toScopedCall okCalls
    _ <- evaluate (sum [termSize t | CurInput _ _ _ t <- cis])
    _ <- evaluate (length [() | CurInput _ lh is0 _ <- cis, lh `seq` is0 `seq` True])
    _ <- evaluate (sum [scopedSize e + length (tfHeap ctx) | ScopedCall _ ctx e _ _ <- scs])
    printf "  measuring on %d calls\n" (length okCalls)
    cur <- measure reps "current evaluator" $ \j ->
      evaluate (j + sum [termSize (runCurrent tcm bm sup ci) | ci <- cis])
    sco <- measure reps "scoped evaluator" $ \j ->
      evaluate (j + sum [ scopedSize (runScoped env bm isSubj ctx e)
                        | ScopedCall isSubj ctx e _ _ <- scs ])
    printf "  allocation ratio scoped/current: %.3f\n"
      (fromIntegral sco / fromIntegral cur :: Double)
    when (nTop > 0) $ do
      perCall <- forM (zip3 [0 :: Int ..] cis scs) $ \(i, ci, sc) -> do
        aC <- allocation (evaluate (termSize (runCurrent tcm bm sup ci)))
        aS <- allocation $ case sc of
          ScopedCall isSubj ctx e _ _ -> evaluate (scopedSize (runScoped env bm isSubj ctx e))
        pure (aS - aC, i, aC, aS)
      printf "  calls with the most extra allocation (scoped - current):\n"
      forM_ (take nTop (List.sortOn (\(d, _, _, _) -> negate d) perCall)) $ \(d, i, aC, aS) ->
        case (okCalls !! i, scs !! i) of
          (c, ScopedCall _ ctx e _ _) ->
            printf "    call %6d: +%10d (current %10d, scoped %10d), subject %s, %d context bindings, term %d nodes: %s\n"
              i d aC aS (show (callSubj c)) (length (tfHeap ctx)) (scopedSize e)
              (take 300 (unwords (words (C.showPpr (callTerm c)))))
 where
  outcomeKey = \case
    Failed s -> "failed: " ++ take 80 s
    o -> show o

-- | The allocation of an action
allocation :: IO a -> IO Int64
allocation act = do
  c0 <- getAllocationCounter
  _ <- act
  c1 <- getAllocationCounter
  pure (c0 - c1)

-- | Run a workload a number of times, and print its allocation and time per
-- run. Gives the allocation.
measure :: Int -> String -> (Int -> IO a) -> IO Integer
measure reps label act = do
  _ <- act 0
  performMinorGC
  s0 <- getRTSStats
  t0 <- getMonotonicTime
  forM_ [1 .. reps] act
  t1 <- getMonotonicTime
  performMinorGC
  s1 <- getRTSStats
  let alloc = fromIntegral (allocated_bytes s1 - allocated_bytes s0) `div` fromIntegral reps
  printf "  %-40s %14d bytes %10.3f ms (indicative)\n" label alloc
    ((t1 - t0) * 1000 / fromIntegral reps)
  pure alloc

-- * Primitives

-- | A call of the reduction of a primitive, with its comparison with the
-- adapter forced
data PrimCall = PrimCall
  { pcName :: Text
  , pcVia :: P.PrimVia
  , pcReduced :: Bool
  , pcCheck :: Maybe (Either String P.PrimOutcome)
  -- ^ The comparison, or the exception it threw
  }

forceEvent :: P.PrimEvent -> IO PrimCall
forceEvent (P.PrimEvent nm via reduced check) = PrimCall nm via reduced <$> case check of
  Nothing -> pure Nothing
  Just o -> do
    r <- try (timeout 20000000 (evaluate (forceOutcome o)))
    pure $ Just $ case r of
      Left (ex :: SomeException) -> Left (takeWhile (/= '\n') (show ex))
      Right Nothing -> Left "timeout"
      Right (Just o') -> Right o'
 where
  forceOutcome o = case o of
    P.Differ a b -> length a `seq` length b `seq` o
    P.OnlyPorted a -> length a `seq` o
    P.OnlyReference a -> length a `seq` o
    _ -> o

-- | Counts of the calls of the reductions of a primitive
data PrimCounts = PrimCounts
  { pcPortedReduced :: !Int
  , pcPortedStuck :: !Int
  , pcAdaptedReduced :: !Int
  , pcAdaptedStuck :: !Int
  , pcCalls :: !Int
  -- ^ The number of replayed calls of the evaluator in which it occurs
  , pcChecks :: !(Map.Map String Int)
  }

instance Semigroup PrimCounts where
  PrimCounts a b c d e f <> PrimCounts a' b' c' d' e' f' =
    PrimCounts (a + a') (b + b') (c + c') (d + d') (e + e') (Map.unionWith (+) f f')

primCounts :: PrimCall -> PrimCounts
primCounts pc = PrimCounts
  { pcPortedReduced = n (pcVia pc == P.Ported && pcReduced pc)
  , pcPortedStuck = n (pcVia pc == P.Ported && not (pcReduced pc))
  , pcAdaptedReduced = n (pcVia pc == P.Adapted && pcReduced pc)
  , pcAdaptedStuck = n (pcVia pc == P.Adapted && not (pcReduced pc))
  , pcCalls = 0
  , pcChecks = maybe Map.empty (\c -> Map.singleton (checkKey c) 1) (pcCheck pc)
  }
 where
  n b = if b then 1 else 0

checkKey :: Either String P.PrimOutcome -> String
checkKey = either (\e -> "check failed: " <> take 60 e) P.outcomeKey

-- | Report the reductions of primitives, see the module header
reportPrims :: ReplayOptions -> [(Int, [PrimCall])] -> IO ()
reportPrims opts results = do
  let usesAdapter = any ((== P.Adapted) . pcVia)
      noPrims = length [() | (_, []) <- results]
      adapted = length [() | (_, evs) <- results, usesAdapter evs]
      ported = length results - noPrims - adapted
  printf "  calls without primitives: %d, only ported reductions: %d, with the adapter: %d\n"
    noPrims ported adapted
  let perPrim = Map.fromListWith (flip (<>))
        [ (pcName e, (primCounts e) {pcCalls = 0})
        | (_, evs) <- results, e <- evs ]
      callsPerPrim = Map.fromListWith (+)
        [ (nm, 1 :: Int) | (_, evs) <- results, nm <- List.nub (map pcName evs) ]
      rows = List.sortOn (\(nm, c) -> (negate (total c), nm))
        [ (nm, c {pcCalls = Map.findWithDefault 0 nm callsPerPrim}) | (nm, c) <- Map.toList perPrim ]
      total c = pcPortedReduced c + pcPortedStuck c + pcAdaptedReduced c + pcAdaptedStuck c
  printf "  %-60s %17s %17s %8s\n" ("primitive" :: String) ("ported red/stuck" :: String)
    ("adapter red/stuck" :: String) ("calls" :: String)
  forM_ rows $ \(nm, c) -> do
    printf "  %-60s %8d/%-8d %8d/%-8d %8d\n" (Text.unpack nm)
      (pcPortedReduced c) (pcPortedStuck c) (pcAdaptedReduced c) (pcAdaptedStuck c) (pcCalls c)
    forM_ (Map.toList (pcChecks c)) $ \(k, cnt) ->
      printf "      check %-40s %8d\n" k cnt
  when (roCheckPrims opts) $
    forM_ (take (roShow opts) [ (i, pcName e, a, b)
                              | (i, evs) <- results, e <- evs
                              , Just (Right (P.Differ a b)) <- [pcCheck e] ]) $ \(i, nm, a, b) ->
      printf "  -- call %d: %s differs from the adapter\n  ported:  %s\n  adapter: %s\n" i
        (Text.unpack nm) (take 1500 (unwords (words a))) (take 1500 (unwords (words b)))
  case (roOnly opts, results) of
    (Just _, [(_, evs)]) -> do
      printf "  reductions of the call, in order:\n"
      forM_ evs $ \e ->
        printf "    %-60s %-8s %s%s\n" (Text.unpack (pcName e)) (show (pcVia e))
          (if pcReduced e then "reduced" else "stuck" :: String)
          (maybe "" ((", check: " <>) . checkKey) (pcCheck e))
    _ -> pure ()
