{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Differential tests at captured sites: the sites at which a transformation
  fired during normalization, recorded by "Clash.Rewrite.Capture". Many
  transformations never fire on the inputs of normalization, which
  "Test.Clash.Differential" compares on, because they only apply to terms that
  other transformations create.

  A t'Corpus' is the captures of a design. 'runAtSite' runs a normalizer
  action in the state of a site: the final state of the session, with the
  supply of uniques, '_curFun' and the other state that a capture records.
  'compareSite' runs a t'SiteComparison' at a site:

  * __Replay.__ It runs the current transformation, which must reproduce the
    captured result, up to alpha-equivalence. If it doesn't, the
    transformation is not deterministic, or the capture lacks state that it
    depends on: see "Clash.Rewrite.Capture" for what a capture has.

  * __Scoped.__ It runs the scoped transformation through
    'Clash.Rewrite.Scoped.ViaFoil.viaFoil', which converts the term at the
    site and the let-bindings of its context, as the normalizer does for a
    ported transformation. It compares the result with the one of the current
    transformation, as 'Test.Clash.Differential.compareBinding' does.

  Both sides start from the same state, including the supply of uniques.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Clash.Differential.Sites
  ( -- * Corpora
    Corpus (..)
  , mkCorpus
  , runAtSite
  , siteContext
    -- * Sites of unit tests
  , emptyFinalState
  , mkCapture
    -- * Comparisons
  , SiteComparison (..)
  , Replay (..)
  , SiteOutcome (..)
  , compareSite
  , compareAtSite
    -- * Summaries
  , SiteSummary (..)
  , compareCorpus
  , addOutcome
  , renderSiteSummary
  , renderSite
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Control.Lens as Lens
import qualified Control.Monad.Writer as Writer
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)

import Clash.Core.FreeVars (dependencySortTyVars, freeLocalIds, termFreeTyVars)
import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.Term as C
import Clash.Core.Var (Id, varName)
import Clash.Core.VarEnv
  (eltsVarEnv, emptyVarEnv, mkVarEnv, restrictInScopeSet)
import Clash.Driver.Types (BindingMap, ClashEnv (..))
import Clash.Normalize.Strategy (constantPropagation, normalization)
import Clash.Normalize.Types
  (NormRewrite, NormalizeSession, NormalizeState (..))
import Clash.Rewrite.Capture
import Clash.Rewrite.Types
  (RewriteEnv, RewriteState (..), TransformContext (..), clashEnv, runR)
import Clash.Util (noSrcSpan)
import Clash.Unique (Unique)
import Clash.Util.Supply (newSupply, reserveUniques, resumeSupply, supplyRange)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.Convert (convScope, fromClashTerm, withConvEnv)
import Clash.Rewrite.Scoped (ScopedRewrite)
import Clash.Rewrite.Scoped.ViaFoil (viaFoil)

import Test.Clash.Differential
  (KnownDifference, Mismatch (..), Outcome (..), Summary (..), attempt,
   compareResults, renderMismatch, truncateText)

-- * Corpora

-- | What is needed to run transformations at the captured sites of a design
data Corpus = Corpus
  { corpusEnv :: RewriteEnv
  , corpusState :: RewriteState NormalizeState
  -- ^ The final state of the session; 'runAtSite' sets the state of a site
  , corpusBindings :: IntMap BindingMap
  -- ^ The global bindings of every epoch, see
  -- 'Clash.Rewrite.Capture.capBindingsEpoch'
  }

-- | A corpus with the final state of a session, and the epochs of the global
-- bindings that were added during the session (see
-- 'Clash.Rewrite.Capture.clBindingEpochs'). Makes sure that new supplies of
-- uniques don't hand out the uniques of the session, see
-- 'Clash.Util.Supply.reserveUniques'.
mkCorpus :: RewriteEnv -> FinalState -> [(Unique, Int)] -> IO Corpus
mkCorpus env fs epochs = do
  reserveUniques (fsUniqueBound fs)
  sup <- newSupply
  let final = fsBindings fs
      initial = List.foldl' (flip UniqMap.delete) final (map fst epochs)
      added = IntMap.fromListWith (<>) [(e, [u]) | (u, e) <- epochs]
      -- Every epoch adds its bindings to the ones of the epoch before it
      byEpoch = IntMap.fromDistinctAscList (zip (IntMap.keys added) (drop 1 (List.scanl'
        (\bs us -> List.foldl' (\acc u -> maybe acc (\b -> UniqMap.insert u b acc) (UniqMap.lookup u final)) bs us)
        initial (IntMap.elems added))))
  pure Corpus
    { corpusBindings = IntMap.insert 0 initial byEpoch
    , corpusEnv = env
    , corpusState = RewriteState
        { _transformCounter = 0
        , _transformAppliedCounters = mempty
        , _transformTriedCounters = mempty
        , _bindings = fsBindings fs
        , _uniqSupply = sup
        , _curFun = error "Test.Clash.Differential.Sites: no curFun"
        , _nameCounter = 0
        , _globalHeap = (IntMap.empty, 0)
        , _workFreeBinders = fsWorkFreeBinders fs
        , _hwTypeCache = Map.empty
        , _extra = NormalizeState
            { _normalized = fsNormalized fs
            , _specialisationCache = fsSpecialisationCache fs
            , _specialisationHistory = fsSpecialisationHistory fs
            , _inlineHistory = fsInlineHistory fs
            , _primitiveArgs = fsPrimitiveArgs fs
            , _recursiveComponents = fsRecursiveComponents fs
            , _normalizationStrategy = normalization
            , _constantPropagationStrategy = constantPropagation
            , _workFreeAppCache = HashMap.fromList (fsWorkFreeAppCache fs)
            }
        }
    }

-- | Run a normalizer action in the state of a site. Every run gets the same
-- supply of uniques: the one of the site.
runAtSite :: Corpus -> Capture -> NormalizeSession a -> IO a
runAtSite c cap m = do
  sup <- resumeSupply (capSupply cap)
  let st = (corpusState c)
        { _bindings = maybe (_bindings (corpusState c)) snd
            (IntMap.lookupLE (capBindingsEpoch cap) (corpusBindings c))
        , _transformCounter = capTransformCounter cap
        , _uniqSupply = sup
        , _curFun = capCurFun cap
        , _nameCounter = capNameCounter cap
        , _globalHeap = capGlobalHeap cap
        }
  (\(a, _, _) -> a) <$> runR m (corpusEnv c) st

-- | The context of a site
siteContext :: Capture -> TransformContext
siteContext cap = TransformContext (capInScope cap) (capContext cap)

-- * Sites of unit tests

-- | The final state of a session that didn't change the bindings, nor fill
-- any cache
emptyFinalState :: RewriteEnv -> BindingMap -> FinalState
emptyFinalState env bs = FinalState
  { fsBindings = bs
  , fsWorkFreeBinders = emptyVarEnv
  , fsNormalized = emptyVarEnv
  , fsSpecialisationCache = Map.empty
  , fsSpecialisationHistory = emptyVarEnv
  , fsInlineHistory = emptyVarEnv
  , fsPrimitiveArgs = Map.empty
  , fsRecursiveComponents = emptyVarEnv
  , fsWorkFreeAppCache = []
  , fsTopEntities = []
  , fsOpts = captureOpts (envOpts (Lens.view clashEnv env))
  , fsUniqueBound = 0
  }

-- | A capture of a transformation that rewrote a term to another one, in a
-- context and while normalizing the given binding, as the normalizer
-- captures it: with its context trimmed (see 'trimContext'), and a new supply
-- of uniques
mkCapture
  :: Text
  -- ^ Name of the transformation
  -> Id
  -- ^ The binding being normalized
  -> TransformContext
  -> C.Term
  -> C.Term
  -> IO Capture
mkCapture nm cf (TransformContext is frames) e0 e1 = do
  sup <- newSupply
  let (frames1, keep) = trimContext frames [e0, e1]
  pure Capture
    { capName = nm
    , capFiring = 1
    , capTransformCounter = 0
    , capCurFun = (cf, noSrcSpan)
    , capInScope = restrictInScopeSet keep is
    , capContext = frames1
    , capInput = e0
    , capOutput = e1
    , capSupply = supplyRange sup
    , capNameCounter = 0
    , capGlobalHeap = (IntMap.empty, 0)
    , capBindingsEpoch = 0
    }

-- * Comparisons

-- | A comparison of a current transformation with its scoped port, at the
-- sites at which the current one fired
data SiteComparison = SiteComparison
  { scName :: String
  , scDescription :: String
  , scTransformations :: [Text]
  -- ^ The names of the transformation in captures. A strategy can give a
  -- transformation another name, see 'Clash.Rewrite.StrategyDSL.named'.
  , scCurrent :: NormRewrite
  , scScoped :: Maybe (ScopedRewrite NormalizeState)
  -- ^ Without one, only replays the current transformation
  , scKnownDifferences :: [KnownDifference]
  -- ^ Applied when comparing the scoped side with the current one, and the
  -- current one with the captured result: the normalizer runs the port of a
  -- ported transformation, so the captured result is the one of the port.
  }

-- | Whether the current transformation reproduces the captured result
data Replay
  = Reproduced
  | ReproducedKnown [String]
  -- ^ Up to the known differences with these names
  | NotReproduced Mismatch
  -- ^ Its result is different, or it crashed
  | NotFired C.Term
  -- ^ It didn't fire: the captured result

-- | The outcome of comparing at a site
data SiteOutcome = SiteOutcome
  { soReplay :: Replay
  , soScoped :: Maybe Outcome
  -- ^ The result of the scoped transformation compared with the one of the
  -- current transformation, if it has one and the current one didn't crash
  }

-- | Run a transformation at a site, giving its result and whether it fired
runRewriteAt :: Corpus -> Capture -> NormRewrite -> IO (C.Term, Bool)
runRewriteAt c cap rw = do
  (t, Monoid.Any ch) <- runAtSite c cap $
    Writer.listen (rw (siteContext cap) (capInput cap))
  t' <- evaluate (force t)
  pure (t', ch)

-- | Run a comparison at a site. Running a side takes at most the given number
-- of microseconds.
compareSite :: Int -> SiteComparison -> Corpus -> Capture -> IO SiteOutcome
compareSite limit sc c cap = do
  rc <- timed "current side" $ runRewriteAt c cap (scCurrent sc)
  case rc of
    Left m -> pure (SiteOutcome (NotReproduced m) Nothing)
    Right (cur, fired) -> do
      replay <-
        if not fired
          then pure (NotFired (capOutput cap))
          else timed "comparing with the captured result"
                 (evaluate (compareAtSite (scKnownDifferences sc) (capOutput cap) cur)) >>= \case
            Left m -> pure (NotReproduced m)
            Right Equal -> pure Reproduced
            Right (KnownDifferent kds) -> pure (ReproducedKnown kds)
            Right (Different m) -> pure (NotReproduced m)
      scoped <- case scScoped sc of
        Nothing -> pure Nothing
        Just srw -> fmap (Just . either Different id) $ do
          rs <- timed "scoped side" $
            runRewriteAt c cap (viaFoil (scName sc) srw)
          case rs of
            Left m -> pure (Left m)
            Right (s, _) -> timed "comparing" $
              evaluate (compareAtSite (scKnownDifferences sc) cur s)
      pure (SiteOutcome replay scoped)
 where
  timed :: String -> IO a -> IO (Either Mismatch a)
  timed stage act = timeout limit (attempt stage act) >>= \case
    Just r -> pure r
    Nothing -> pure (Left (Crash stage ("timed out after " <> show (limit `div` 1000000) <> " s")))

-- | Compare two terms at a site, see 'Test.Clash.Differential.compareBinding':
-- they are converted to scoped Core in a scope with their free variables
compareAtSite :: [KnownDifference] -> C.Term -> C.Term -> Outcome
compareAtSite kds a b =
  withConvEnv tvs ids $ \env ->
    compareResults kds (convScope env) (fromClashTerm env a) (fromClashTerm env b)
 where
  tvs = dependencySortTyVars (eltsVarEnv (mkVarEnv [(v, v) | v <- Lens.toListOf termFreeTyVars a <> Lens.toListOf termFreeTyVars b]))
  ids = eltsVarEnv (mkVarEnv [(v, v) | v <- Lens.toListOf freeLocalIds a <> Lens.toListOf freeLocalIds b])

-- * Summaries

-- | The outcomes of a comparison at the sites of a corpus
data SiteSummary = SiteSummary
  { ssSites :: !Int
  , ssReproduced :: !Int
  , ssReproducedKnown :: !Int
  -- ^ Up to known differences
  , ssNotReproduced :: !Int
  , ssNotFired :: !Int
  , ssReplayCrashed :: !Int
  -- ^ How many of the ones not reproduced crashed or timed out
  , ssScoped :: Summary
  -- ^ Of the scoped side, compared with the current one. Has no mismatches:
  -- those are in 'ssMismatches'.
  , ssMismatches :: [(Capture, Either Replay Mismatch)]
  -- ^ The first sites that the current side didn't reproduce ('Left'), or
  -- where the scoped side differs ('Right')
  }

instance Semigroup SiteSummary where
  SiteSummary a1 b1 k1 c1 d1 e1 s1 m1 <> SiteSummary a2 b2 k2 c2 d2 e2 s2 m2 =
    SiteSummary (a1 + a2) (b1 + b2) (k1 + k2) (c1 + c2) (d1 + d2) (e1 + e2) (s1 <> s2) (m1 <> m2)

instance Monoid SiteSummary where
  mempty = SiteSummary 0 0 0 0 0 0 mempty []

-- | Run a comparison at the captured sites of its transformation, of which
-- the given predicate holds. Keeps the first @n@ mismatches of either kind.
-- Consumes the captures one at a time, see
-- 'Clash.Rewrite.Capture.decodeCaptureLog'.
compareCorpus
  :: Int
  -- ^ Timeout per side, in microseconds
  -> Int
  -- ^ Mismatches to keep
  -> (Capture -> Bool)
  -> SiteComparison
  -> Corpus
  -> [Capture]
  -> IO SiteSummary
compareCorpus limit n p sc c = go mempty
 where
  select cap = capName cap `elem` scTransformations sc && p cap

  go s [] = pure s {ssMismatches = reverse (ssMismatches s)}
  go !s (cap : caps)
    | select cap = do
        o <- compareSite limit sc c cap
        go (addOutcome n cap o s) caps
    | otherwise = go s caps

-- | Add the outcome at a site to a summary, keeping the first @n@ mismatches.
-- Mismatches are kept in reverse.
addOutcome :: Int -> Capture -> SiteOutcome -> SiteSummary -> SiteSummary
addOutcome n cap (SiteOutcome replay scoped) s = s3
 where
  s1 = s {ssSites = ssSites s + 1}
  keep m s' = if length (ssMismatches s') < n then s' {ssMismatches = (cap, m) : ssMismatches s'} else s'
  s2 = case replay of
    Reproduced -> s1 {ssReproduced = ssReproduced s1 + 1}
    ReproducedKnown _ -> s1 {ssReproducedKnown = ssReproducedKnown s1 + 1}
    NotFired _ -> keep (Left replay) s1 {ssNotFired = ssNotFired s1 + 1}
    NotReproduced m -> keep (Left replay) s1
      { ssNotReproduced = ssNotReproduced s1 + 1
      , ssReplayCrashed = ssReplayCrashed s1 + (case m of Crash {} -> 1; Mismatch {} -> 0)
      }
  s3 = case scoped of
    Nothing -> s2
    Just o ->
      let one = Summary 1 0 Map.empty 0 0 []
          sm = case o of
            Equal -> one {sumEqual = 1}
            KnownDifferent kds -> one {sumKnownDifferent = Map.singleton (List.intercalate "+" kds) 1}
            Different m -> one {sumDifferent = 1, sumCrashed = case m of Crash {} -> 1; Mismatch {} -> 0}
          s2' = s2 {ssScoped = ssScoped s2 <> sm}
      in case o of
           Different m -> keep (Right m) s2'
           _ -> s2'

-- | A summary of a comparison on a corpus, on one or two lines
renderSiteSummary :: String -> SiteSummary -> String
renderSiteSummary nm s =
  nm <> ": " <> show (ssSites s) <> " sites, current side: "
    <> show (ssReproduced s) <> " reproduced, "
    <> (if ssReproducedKnown s > 0
          then show (ssReproducedKnown s) <> " reproduced up to known differences, "
          else "")
    <> show (ssNotReproduced s) <> " not reproduced"
    <> (if ssReplayCrashed s > 0 then " (" <> show (ssReplayCrashed s) <> " crashed)" else "")
    <> ", " <> show (ssNotFired s) <> " not fired"
    <> scoped
 where
  sc = ssScoped s
  scoped
    | sumCompared sc == 0 = ""
    | otherwise =
        "\n  scoped side: " <> show (sumEqual sc) <> " equal, "
          <> show (sum (sumKnownDifferent sc)) <> " known-different"
          <> known
          <> ", " <> show (sumDifferent sc) <> " different"
          <> (if sumCrashed sc > 0 then " (" <> show (sumCrashed sc) <> " crashed)" else "")
  known
    | Map.null (sumKnownDifferent sc) = ""
    | otherwise = " (" <> List.intercalate ", "
        [k <> ": " <> show v | (k, v) <- Map.toList (sumKnownDifferent sc)] <> ")"

-- | A mismatch at a site, with terms truncated to @l@ lines of @w@ characters
renderSite :: Int -> Int -> Capture -> Either Replay Mismatch -> String
renderSite l w cap r = header <> body
 where
  (cf, _) = capCurFun cap
  header =
    "=== " <> Text.unpack (capName cap) <> " #" <> show (capFiring cap)
      <> " in " <> Text.unpack (nameOcc (varName cf)) <> ": " <> what <> "\n"
  (what, body) = case r of
    Left (NotFired out) ->
      ( "the current side didn't fire; the captured result:"
      , unlines (map ("    " <>) (lines (truncateText l w (showPpr out)))) )
    Left (NotReproduced m) ->
      ( "the current side didn't reproduce the captured result \
        \(\"current\" is the captured result, \"scoped\" the one of the current side)"
      , renderMismatch l w cf m )
    Left Reproduced -> ("reproduced", "")
    Left (ReproducedKnown _) -> ("reproduced up to known differences", "")
    Right m -> ("the scoped side differs from the current side", renderMismatch l w cf m)
