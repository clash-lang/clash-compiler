{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Capturing the sites at which transformations fire, for the differential
  tests of the port to scoped Clash Core. Many transformations never fire on
  the inputs of normalization, because they only apply to terms that other
  transformations create. A capture records a term that a transformation
  rewrote during normalization, what it rewrote it to, and enough of its
  context and of the state of the rewrite session to run the transformation
  there again.

  = Enabling

  Capturing is off unless the environment variable @CLASH_CAPTURE_REWRITES@ is
  set when the process starts. Its value is a directory, which this module
  doesn't use: @clash-profile-normalization-run@ writes the captures of every
  design there. @CLASH_CAPTURE_LIMIT@ sets how many captures to keep per
  transformation and session (default 2000). When capturing is off, the
  only cost is a test of a constant per transformation that is applied.

  = Sampling

  'Clash.Rewrite.Util.applyWith' calls 'recordCapture' whenever a
  transformation signals a change. Per transformation, the captures are a
  uniform sample of all of its firings in a session (reservoir sampling), with
  a fixed pseudo-random sequence, so the sample is the same on every run.
  Whole-term strategies run with 'Clash.Rewrite.Util.runRewrite', such as
  @normalization@, are not captured: their input is a whole binding.

  The sample is stratified by size: sites at which the term or the result is
  large ('largeSite') are sampled separately, and only a few of them are
  kept ('largeLimit'). Encoded captures share no terms with each other (see
  "Clash.Rewrite.Capture.Encoding"), and a transformation that fires at the
  let-expression of a whole binding would otherwise fill the corpus with
  copies of that binding.

  = What is captured

  A t'Capture' has the term the transformation was offered and its result,
  its context, and the parts of the 'RewriteState' that change during a
  session and that a transformation reads: the supply of uniques
  ('Clash.Util.Supply.supplyRange'), '_curFun', '_nameCounter' and
  '_globalHeap'.

  The context is trimmed, as the full context of a site is often as big as
  the binding it occurs in, and a session has many captures:

  * A 'LetBody' frame keeps the bindings of the variables of the term, of its
    result, and of the bindings kept so far, transitively: the evaluator may
    use those ('Clash.Rewrite.Util.whnfRW'). The other bindings can't be
    reached from the term.
  * A 'LetBinding' frame keeps the siblings that are kept in the frame above.
  * The in-scope set keeps the term variables of the terms and of the kept
    bindings, the binders of the frames, and its seed
    ('Clash.Core.VarEnv.restrictInScopeSet'). So
    'Clash.Core.VarEnv.uniqAway' may pick other uniques for new binders than
    it did during normalization, which only changes the result up to
    alpha-equivalence.

  The rest of the state is recorded once per session, at its end
  (t'FinalState'): the global bindings, and the caches of the normalizer.
  These only grow during a session, and an entry never changes once it is
  added. So a transformation finds the entries that it found during
  normalization in the final state too, and the entries it added itself,
  with the values it computed.

  That is not enough for the global bindings: a transformation that creates a
  global binding would find the one it created during normalization, and
  pick another unique. So a capture records which global bindings existed
  ('capBindingsEpoch'). The caches can still have entries that a
  transformation added after the capture, e.g. a specialization that it
  would otherwise have created anew. And the counters of how often a
  function was inlined or specialized ('Clash.Normalize.Types._inlineHistory',
  'Clash.Normalize.Types._specialisationHistory') do change during a session:
  rerunning a transformation sees their final values, which only makes a
  difference when a limit was reached in the meantime.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

module Clash.Rewrite.Capture
  ( -- * Enabling
    captureEnabled
  , captureLimit
  , largeSite
  , largeLimit
    -- * Captures
  , Capture (..)
  , FinalState (..)
  , CaptureOpts (..)
  , captureOpts
  , applyCaptureOpts
  , CaptureLog (..)
    -- * Recording
  , recordCapture
  , recordFinalState
  , takeCaptureLog
    -- * Trimming contexts
  , trimContext
  ) where

import           Control.DeepSeq          (NFData)
import           Control.Exception        (evaluate)
import qualified Control.Lens             as Lens
import           Data.Binary              (Binary)
import           Data.Bits                (shiftR, xor)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.IORef
  (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import           Data.IntMap.Strict       (IntMap)
import qualified Data.IntMap.Strict       as IntMap
import qualified Data.List                as List
import           Data.Map.Strict          (Map)
import           Data.Set                 (Set)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Word                (Word64)
import           GHC.Exts                 (isTrue#, reallyUnsafePtrEquality#)
import           GHC.Generics             (Generic)
import           System.Environment       (lookupEnv)
import           System.IO.Unsafe         (unsafePerformIO)
import           System.Mem.StableName    (StableName, makeStableName)
import           Text.Read                (readMaybe)

import           Clash.Core.Evaluator.Types (PrimHeap)
import           Clash.Core.FreeVars      (freeLocalIds)
import           Clash.Core.Term
  (Context, CoreContext (..), LetBinding, Term, TickInfo (..), patIds)
import           Clash.Core.TermInfo      (termSizeSmallerThan)
import           Clash.Core.Type          (Type)
import           Clash.Core.Var           (Id, Var (..))
import           Clash.Core.VarEnv
  (InScopeSet, VarEnv, VarSet, elemVarSet, eltsVarSet, emptyVarSet, extendVarSet,
   lookupVarEnv, mkVarEnv, restrictInScopeSet, unionVarSet, unitVarSet)
import           Clash.Data.UniqMap       (UniqMap)
import qualified Clash.Data.UniqMap       as UniqMap
import           Clash.Driver.Types       (BindingMap, ClashOpts (..))
import           Clash.Rewrite.Types
  (RewriteState (..), TransformContext (..), curFunLegacy)
import           Clash.Unique             (Unique)
import           Clash.Util               (SrcSpan)
import           Clash.Util.Supply        (supplyRange)

-- * Enabling

-- | Whether to capture, see the module header. Read once, when first needed.
captureEnabled :: Bool
captureEnabled = unsafePerformIO $ maybe False (not . null) <$> lookupEnv "CLASH_CAPTURE_REWRITES"
{-# NOINLINE captureEnabled #-}

-- | How many captures to keep per transformation and session
captureLimit :: Int
captureLimit = unsafePerformIO $
  maybe 2000 (\s -> maybe (error ("CLASH_CAPTURE_LIMIT: not a number: " <> s)) id (readMaybe s))
    <$> lookupEnv "CLASH_CAPTURE_LIMIT"
{-# NOINLINE captureLimit #-}

-- * Captures

-- | A site at which a transformation fired, see the module header
data Capture = Capture
  { capName :: Text
  -- ^ The name of the transformation
  , capFiring :: !Word
  -- ^ Which firing of the transformation in the session this is, from 1
  , capTransformCounter :: !Word
  -- ^ How many transformations fired before it in the session
  , capCurFun :: (Id, SrcSpan)
  -- ^ The binding being normalized
  , capInScope :: InScopeSet
  -- ^ The trimmed in-scope set
  , capContext :: Context
  -- ^ The trimmed context, innermost frame first
  , capInput :: Term
  -- ^ The term the transformation was offered
  , capOutput :: Term
  -- ^ What it rewrote it to
  , capSupply :: (Unique, Unique)
  -- ^ The 'supplyRange' of the supply of uniques, before the transformation
  , capNameCounter :: !Int
  , capGlobalHeap :: PrimHeap
  , capBindingsEpoch :: !Int
  -- ^ Which global bindings existed: those of 'clBindingEpochs' of this epoch
  -- or before, and those that existed at the first capture of the session
  }

-- | The options of a session that transformations read. 'ClashOpts' has no
-- 'Binary' instance.
data CaptureOpts = CaptureOpts
  { coInlineLimit :: Int
  , coSpecLimit :: Int
  , coInlineFunctionLimit :: Word
  , coInlineConstantLimit :: Word
  , coInlineWFCacheLimit :: Word
  , coEvaluatorFuelLimit :: Word
  , coNewInlineStrat :: Bool
  , coUltra :: Bool
  , coAggressiveXOpt :: Bool
  , coIntWidth :: Int
  }
  deriving (Show, Generic, NFData, Binary)

-- | The options of a session that transformations read
captureOpts :: ClashOpts -> CaptureOpts
captureOpts o = CaptureOpts
  { coInlineLimit = opt_inlineLimit o
  , coSpecLimit = opt_specLimit o
  , coInlineFunctionLimit = opt_inlineFunctionLimit o
  , coInlineConstantLimit = opt_inlineConstantLimit o
  , coInlineWFCacheLimit = opt_inlineWFCacheLimit o
  , coEvaluatorFuelLimit = opt_evaluatorFuelLimit o
  , coNewInlineStrat = opt_newInlineStrat o
  , coUltra = opt_ultra o
  , coAggressiveXOpt = opt_aggressiveXOpt o
  , coIntWidth = opt_intWidth o
  }

-- | Set the options of a session
applyCaptureOpts :: CaptureOpts -> ClashOpts -> ClashOpts
applyCaptureOpts c o = o
  { opt_inlineLimit = coInlineLimit c
  , opt_specLimit = coSpecLimit c
  , opt_inlineFunctionLimit = coInlineFunctionLimit c
  , opt_inlineConstantLimit = coInlineConstantLimit c
  , opt_inlineWFCacheLimit = coInlineWFCacheLimit c
  , opt_evaluatorFuelLimit = coEvaluatorFuelLimit c
  , opt_newInlineStrat = coNewInlineStrat c
  , opt_ultra = coUltra c
  , opt_aggressiveXOpt = coAggressiveXOpt c
  , opt_intWidth = coIntWidth c
  }

-- | The state of a normalization session at its end, see the module header
data FinalState = FinalState
  { fsBindings :: BindingMap
  , fsWorkFreeBinders :: VarEnv Bool
  , fsNormalized :: BindingMap
  , fsSpecialisationCache :: Map (Id, Int, Either Term Type) Id
  , fsSpecialisationHistory :: VarEnv Int
  , fsInlineHistory :: VarEnv (VarEnv Int)
  , fsPrimitiveArgs :: Map Text (Set Int)
  , fsRecursiveComponents :: VarEnv Bool
  , fsWorkFreeAppCache :: [(Term, Term)]
  , fsTopEntities :: [Id]
  , fsOpts :: CaptureOpts
  , fsUniqueBound :: Unique
  -- ^ An upper bound on the uniques handed out in the session's process, see
  -- 'Clash.Util.Supply.supplyBound'
  }

-- | The captures of a session. See "Clash.Rewrite.Capture.Encoding" for how
-- to encode it.
data CaptureLog = CaptureLog
  { clFinal :: FinalState
  , clFirings :: [(Text, Word)]
  -- ^ How often every transformation fired in the session
  , clBindingEpochs :: [(Unique, Int)]
  -- ^ The global bindings that were added after the first capture, and the
  -- epochs in which they were (see 'capBindingsEpoch')
  , clCaptures :: [Capture]
  -- ^ By transformation, and by firing
  }

-- * Recording

-- | The captures of a transformation so far: how often it fired, and the
-- sample, by slot
data Reservoir = Reservoir !Word !(IntMap Capture)

emptyReservoir :: Reservoir
emptyReservoir = Reservoir 0 IntMap.empty

-- | The captures of a transformation: how often it fired, and the samples of
-- its sites with small and with large terms
data Reservoirs = Reservoirs !Word !Reservoir !Reservoir

data CaptureState = CaptureState
  { csReservoirs :: !(HashMap Text Reservoirs)
  , csFinal :: !(Maybe FinalState)
  , csBindings :: !(Maybe BindingMap)
  -- ^ The global bindings at the last capture
  , csEpoch :: !Int
  -- ^ The epoch of the global bindings, see 'capBindingsEpoch': incremented
  -- whenever a capture finds bindings that the previous one didn't
  , csEpochs :: !(UniqMap Int)
  -- ^ The epochs of the bindings that were added after the first capture
  }

emptyCaptureState :: CaptureState
emptyCaptureState = CaptureState HashMap.empty Nothing Nothing 0 UniqMap.empty

captureRef :: IORef CaptureState
captureRef = unsafePerformIO (newIORef emptyCaptureState)
{-# NOINLINE captureRef #-}

-- | A pseudo-random number for the @n@-th firing, see 'recordCapture'
-- (splitmix64)
mix64 :: Word64 -> Word64
mix64 z0 = z3 `xor` (z3 `shiftR` 31)
 where
  z1 = z0 + 0x9e3779b97f4a7c15
  z2 = (z1 `xor` (z1 `shiftR` 30)) * 0xbf58476d1ce4e5b9
  z3 = (z2 `xor` (z2 `shiftR` 27)) * 0x94d049bb133111eb

-- | Sites at which the term or the result has at least this many nodes (see
-- 'Clash.Core.TermInfo.termSize') are large. A transformation that fires at
-- the let-expression of a whole binding (e.g. 'flattenLet') would otherwise
-- capture that binding thousands of times.
largeSite :: Word
largeSite = 2000

-- | How many large sites to keep per transformation and session
largeLimit :: Int
largeLimit = max 1 (captureLimit `div` 40)

-- | Record that a transformation fired, given the state before it ran. Keeps
-- a uniform sample of 'captureLimit' of the firings at small sites, and one of
-- 'largeLimit' of those at large sites (see 'largeSite'), per transformation
-- (algorithm R).
recordCapture
  :: String
  -- ^ Name of the transformation
  -> TransformContext
  -> Term
  -- ^ The term it was offered
  -> Term
  -- ^ Its result
  -> RewriteState extra
  -- ^ The state before it ran
  -> IO ()
recordCapture name ctx e0 e1 st = do
  rs <- csReservoirs <$> readIORef captureRef
  let nm = Text.pack name
      Reservoirs fired small large =
        HashMap.lookupDefault (Reservoirs 0 emptyReservoir emptyReservoir) nm rs
      fired1 = fired + 1
      isLarge = not (termSizeSmallerThan largeSite e0 && termSizeSmallerThan largeSite e1)
      sample limit (Reservoir n slots) = do
        let n1 = n + 1
            slot
              | n < fromIntegral limit = Just (fromIntegral n)
              | otherwise =
                  let r = fromIntegral (mix64 (fromIntegral n1) `mod` fromIntegral n1)
                  in if r < limit then Just r else Nothing
        case slot of
          Nothing -> pure (Reservoir n1 slots)
          Just i -> do
            fbs <- framesBindings (tfContext ctx)
            epoch <- bindingsEpoch (_bindings st)
            c <- evaluateCapture (mkCapture nm fired1 ctx fbs epoch e0 e1 st)
            pure (Reservoir n1 (IntMap.insert i c slots))
  rs1 <-
    if isLarge
      then Reservoirs fired1 small <$> sample largeLimit large
      else (\small1 -> Reservoirs fired1 small1 large) <$> sample captureLimit small
  atomicModifyIORef' captureRef $ \cs ->
    (cs {csReservoirs = HashMap.insert nm rs1 (csReservoirs cs)}, ())
{-# NOINLINE recordCapture #-}

-- | Evaluate the parts of a capture that refer to the state, or to the full
-- context, so that the capture doesn't keep those alive. Doesn't evaluate the
-- terms: forcing them costs as much as encoding them, as they share types.
evaluateCapture :: Capture -> IO Capture
evaluateCapture c = do
  _ <- evaluate (capCurFun c)
  _ <- evaluate (capSupply c)
  _ <- evaluate (capGlobalHeap c)
  _ <- evaluate (capInScope c)
  _ <- evaluate (foldr frame () (capContext c))
  evaluate c
 where
  frame f r = case f of
    LetBody bs -> length bs `seq` r
    LetBinding _ sibs -> length sibs `seq` r
    _ -> f `seq` r

mkCapture
  :: Text
  -> Word
  -> TransformContext
  -> [Maybe (Either (Bindings Term) (Bindings ()))]
  -- ^ The bindings of the frames of the context, see 'trimContextWith'
  -> Int
  -- ^ The epoch of the global bindings
  -> Term
  -> Term
  -> RewriteState extra
  -> Capture
mkCapture nm n (TransformContext is frames) fbs epoch e0 e1 st = Capture
  { capName = nm
  , capFiring = n
  , capTransformCounter = _transformCounter st
  , capCurFun = st Lens.^. curFunLegacy
  , capInScope = restrictInScopeSet keep is
  , capContext = frames1
  , capInput = e0
  , capOutput = e1
  , capSupply = supplyRange (_uniqSupply st)
  , capNameCounter = _nameCounter st
  , capGlobalHeap = _globalHeap st
  , capBindingsEpoch = epoch
  }
 where
  (frames1, keep) = trimContextWith (zip frames fbs) [e0, e1]

-- | The epoch of the global bindings of a capture, see 'capBindingsEpoch'.
-- Global bindings are only ever added during a session, and rarely, so this
-- only compares them with those of the previous capture when they are a
-- different object.
bindingsEpoch :: BindingMap -> IO Int
bindingsEpoch bs = do
  cs <- readIORef captureRef
  case csBindings cs of
    Just old | isTrue# (reallyUnsafePtrEquality# old bs) -> pure (csEpoch cs)
    Nothing -> do
      writeIORef captureRef cs {csBindings = Just bs}
      pure (csEpoch cs)
    Just old -> do
      let added = UniqMap.difference bs old
          epoch = if UniqMap.null added then csEpoch cs else csEpoch cs + 1
          epochs = List.foldl' (\m u -> UniqMap.insert u epoch m) (csEpochs cs) (UniqMap.keys added)
      writeIORef captureRef cs {csBindings = Just bs, csEpoch = epoch, csEpochs = epochs}
      pure epoch

-- | The bindings of the 'LetBody' and 'LetBinding' frames of a context,
-- cached by the identity of their lists (see 'StableName')
framesBindings :: Context -> IO [Maybe (Either (Bindings Term) (Bindings ()))]
framesBindings = mapM $ \case
  LetBody bs -> Just . Left <$> cached letBodyCache bs (mkBindings bs)
  LetBinding _ sibs -> Just . Right <$> cached letBindingCache sibs (mkBindings [(x, ()) | x <- sibs])
  _ -> pure Nothing
 where
  cached :: IORef (HashMap (StableName [a]) b) -> [a] -> b -> IO b
  cached ref xs mk = do
    xs1 <- evaluate xs
    sn <- makeStableName xs1
    HashMap.lookup sn <$> readIORef ref >>= \case
      Just b -> pure b
      Nothing -> do
        b <- evaluate mk
        -- Keep the cache small: it keeps the frames alive
        modifyIORef' ref (\m -> HashMap.insert sn b (if HashMap.size m >= 64 then HashMap.empty else m))
        pure b

letBodyCache :: IORef (HashMap (StableName [LetBinding]) (Bindings Term))
letBodyCache = unsafePerformIO (newIORef HashMap.empty)
{-# NOINLINE letBodyCache #-}

letBindingCache :: IORef (HashMap (StableName [Id]) (Bindings ()))
letBindingCache = unsafePerformIO (newIORef HashMap.empty)
{-# NOINLINE letBindingCache #-}

-- | Record the state of a session at its end
recordFinalState :: FinalState -> IO ()
recordFinalState fs = do
  -- Not forced: that costs as much as encoding it, as terms share types
  atomicModifyIORef' captureRef (\cs -> (cs {csFinal = Just fs}, ()))

-- | The captures of the last session, if it recorded its final state. Starts
-- a new log.
takeCaptureLog :: IO (Maybe CaptureLog)
takeCaptureLog = do
  cs <- atomicModifyIORef' captureRef (\cs -> (emptyCaptureState, cs))
  let byName = List.sortOn fst (HashMap.toList (csReservoirs cs))
  pure $ csFinal cs >>= \fs -> Just CaptureLog
    { clFinal = fs
    , clFirings = [(nm, n) | (nm, Reservoirs n _ _) <- byName]
    , clBindingEpochs = UniqMap.toList (csEpochs cs)
    , clCaptures = concat
        [ List.sortOn capFiring (IntMap.elems small <> IntMap.elems large)
        | (_, Reservoirs _ (Reservoir _ small) (Reservoir _ large)) <- byName ]
    }

-- * Trimming contexts

-- | The bindings of a 'LetBody' frame, or the siblings of a 'LetBinding'
-- frame, by binder: their positions, and the right-hand sides
type Bindings a = VarEnv (Int, Id, a)

-- | The bindings of a list of let-bindings
mkBindings :: [(Id, a)] -> Bindings a
mkBindings bs = mkVarEnv [(x, (i, x, rhs)) | (i, (x, rhs)) <- zip [0 ..] bs]

-- | Trim the context of terms, see the module header. Also gives the
-- variables to keep in the in-scope set: the term variables of the terms and
-- the kept bindings, and the binders of the kept frames. Doesn't look into
-- types, as the variables of a term share them.
trimContext :: Context -> [Term] -> (Context, VarSet)
trimContext frames = trimContextWith [(f, bindingsOf f) | f <- frames]
 where
  bindingsOf = \case
    LetBody bs -> Just (Left (mkBindings bs))
    LetBinding _ sibs -> Just (Right (mkBindings [(x, ()) | x <- sibs]))
    _ -> Nothing

-- | 'trimContext', given the bindings of the 'LetBody' and 'LetBinding'
-- frames ('Left' and 'Right'), so that they can be cached: their frames are
-- often as big as a binding, and many sites share them. Costs a lookup per
-- variable of the kept terms.
trimContextWith
  :: [(CoreContext, Maybe (Either (Bindings Term) (Bindings ())))]
  -> [Term]
  -> (Context, VarSet)
trimContextWith frames0 terms = go (foldMap freeVarsOf terms) frames0
 where
  go :: VarSet -> [(CoreContext, Maybe (Either (Bindings Term) (Bindings ())))] -> (Context, VarSet)
  go vs [] = ([], vs)
  go vs ((f, fbs) : fs) = case (f, fbs) of
    (LetBody _, Just (Left env)) ->
      let (bs1, vs1) = closeBindings vs env
      in cons (LetBody bs1) (go (binders vs1 (map fst bs1)) fs)
    (LetBinding v _, Just (Right env)) ->
      let sibs = map (\(_, x, ()) -> x) (List.sortOn (\(i, _, ()) -> i) (used vs env))
      in cons (LetBinding v sibs) (go (binders vs [v]) fs)
    (LamBody v, _) -> cons f (go (binders vs [v]) fs)
    (TyLamBody tv, _) -> cons f (go (binders vs [tv]) fs)
    (CaseAlt p, _) ->
      let (tvs, ids) = patIds p
      in cons f (go (binders (binders vs tvs) ids) fs)
    (TickC t, _) -> cons f (go (vs `unionVarSet` tickVars t) fs)
    _ -> cons f (go vs fs)

  cons f (fs, vs) = (f : fs, vs)

  binders :: VarSet -> [Var a] -> VarSet
  binders = List.foldl' extendVarSet

  -- The bindings of the variables of a set
  used :: VarSet -> Bindings a -> [(Int, Id, a)]
  used vs env = [b | v <- eltsVarSet vs, Just b <- [lookupVarEnv v env]]

  -- The bindings of the variables, transitively, in their original order
  closeBindings :: VarSet -> Bindings Term -> ([(Id, Term)], VarSet)
  closeBindings vs0 env =
    ([(x, rhs) | (_, x, rhs) <- List.sortOn (\(i, _, _) -> i) kept], vsN)
   where
    (vsN, kept) = work vs0 emptyVarSet [] (used vs0 env)

    -- Every binding is added at most once
    work vs _ acc [] = (vs, acc)
    work vs nd acc (b@(_, x, rhs) : rest)
      | x `elemVarSet` nd = work vs nd acc rest
      | otherwise =
          let fvs = freeVarsOf rhs
          in work (vs `unionVarSet` fvs) (extendVarSet nd x) (b : acc)
                  (used fvs env ++ rest)

  freeVarsOf :: Term -> VarSet
  freeVarsOf = Lens.foldMapOf freeLocalIds unitVarSet

  tickVars = \case
    Attributes _ e -> freeVarsOf e
    _ -> emptyVarSet
