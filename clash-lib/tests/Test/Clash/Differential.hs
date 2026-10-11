{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  A differential test harness for the port to scoped Clash Core
  ("Clash.Core.Scoped"): it runs a current function and its scoped port on the
  same bindings, and compares their results up to alpha-equivalence.

  A t'Comparison' pairs two t'Side's, the current and the scoped one. A side
  says how to run its monad for a t'Design', and what to do with a binding.
  The scoped side gets the body of the binding converted to scoped Core.
  'compareBinding' runs both, converts the result of the current side to
  scoped Core, and compares the two with 'aeqTm'. If they differ, it applies
  the comparison's t'KnownDifference's to both, which normalize intended
  differences away, and compares again. What remains is reported with the
  first differing subterm ('firstDiff').

  Both sides of a comparison start from the same state: in particular, from
  the same supply of uniques. So they create the same uniques if they draw
  them in the same order, which matters for the new global binders that e.g.
  specialization creates. Every binding starts from the initial state of the
  t'Design': state doesn't carry over from one binding to the next.

  This module is shared by the unit tests and the @clash-differential@
  executable (@benchmark/profiling/differential@), so it only depends on
  @clash-lib@ and the libraries that it depends on. It is not part of
  @clash-lib@ itself, as it is test infrastructure, and it goes away together
  with the conversion to and from scoped Core ("Clash.Core.Scoped.Convert").
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Clash.Differential
  ( -- * Designs
    Design (..)
  , mkDesign
  , designBindingList
  , runNormalizeSession
    -- * Comparisons
  , Comparison (..)
  , Side (..)
  , pureSide
  , normalizeSide
    -- * Known differences
  , KnownDifference (..)
  , applyKnownDifferences
  , recToNonRec
  , letrecOrder
  , coercionForm
  , everywhereTm
  , everywhereScopedTm
    -- * Running transformations
  , Mode (..)
  , strategyFor
  , currentRewrite
  , scopedRewrite
    -- * Comparing
  , Outcome (..)
  , Mismatch (..)
  , compareBinding
  , compareResults
  , attempt
  , Summary (..)
  , compareDesign
    -- * Differences
  , Diff (..)
  , firstDiff
    -- * Reporting
  , renderSummary
  , renderMismatch
  , truncateText
  ) where

import Control.DeepSeq (force, rnf)
import Control.Exception
  (SomeAsyncException, SomeException, evaluate, fromException, throwIO, try)
import qualified Control.Lens as Lens
import Data.Functor.Identity (Identity (..))
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Clash.Core.Name (nameOcc)
import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.Term as C
import Clash.Core.Var (Id, varName)
import Clash.Core.VarEnv (emptyInScopeSet, emptyVarEnv, eltsVarEnv)
import Clash.Driver.Types (Binding (..), BindingMap)
import Clash.Normalize.Strategy (constantPropagation, normalization)
import Clash.Normalize.Types
  (NormRewrite, NormalizeSession, NormalizeState (..))
import Clash.Rewrite.StrategyDSL (Strat, ToStep, bottomup)
import qualified Clash.Rewrite.StrategyDSL as DSL
import Clash.Rewrite.Types (RewriteEnv, RewriteState (..), curFun, runR)
import Clash.Rewrite.Util (runRewrite)
import Clash.Util.Supply (Supply, newSupply)

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Convert (fromClashTermClosed, toClashTerm, toClashType)
import Clash.Core.Scoped.Subst
  (openAlt, openBind, openIdBinder, openTyBinder, unsinkIdBinder)
import Clash.Core.Scoped.Term (walkTerm)
import Clash.Core.Scoped.Util (LetGroup (RecGroup), mkLets, regroupLetBindings)
import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped
  (ScopedRewrite, bottomupR, repeatR, rootContext, runScopedTransform)

-- * Designs

-- | The bindings to compare on, and what is needed to run the normalizer on
-- them
data Design = Design
  { designEnv :: RewriteEnv
  , designBindings :: BindingMap
  , designSupply :: Supply
  -- ^ The supply of uniques that both sides start from
  }

-- | A design with a new supply of uniques
mkDesign :: RewriteEnv -> BindingMap -> IO Design
mkDesign env bs = Design env bs <$> newSupply

-- | The bindings of a design, in the order of their uniques
designBindingList :: Design -> [Binding C.Term]
designBindingList = eltsVarEnv . designBindings

-- | The initial state of normalization, see 'Clash.Normalize.runNormalization'
initialState :: Design -> RewriteState NormalizeState
initialState d = RewriteState
  { _transformCounter = 0
  , _transformAppliedCounters = mempty
  , _transformTriedCounters = mempty
  , _bindings = designBindings d
  , _uniqSupply = designSupply d
  , _curFun = error "Test.Clash.Differential: no curFun, set it with curFun .="
  , _nameCounter = 0
  , _globalHeap = (IntMap.empty, 0)
  , _workFreeBinders = emptyVarEnv
  , _hwTypeCache = Map.empty
  , _extra = NormalizeState
      { _normalized = emptyVarEnv
      , _specialisationCache = Map.empty
      , _specialisationHistory = emptyVarEnv
      , _inlineHistory = emptyVarEnv
      , _primitiveArgs = Map.empty
      , _recursiveComponents = emptyVarEnv
      , _normalizationStrategy = normalization
      , _constantPropagationStrategy = constantPropagation
      , _workFreeAppCache = HashMap.empty
      }
  }

-- | Run a normalizer action on a design, from its initial state
runNormalizeSession :: Design -> NormalizeSession a -> IO a
runNormalizeSession d m = (\(a, _, _) -> a) <$> runR m (designEnv d) (initialState d)

-- * Comparisons

-- | One side of a comparison: a function of a binding, and how to run the
-- monad it runs in. The function gets the body of the binding, as current
-- Core for the current side, and as scoped Core for the scoped one.
data Side m a = Side
  { sideRun :: forall r. Design -> m r -> IO r
  , sideFun :: Binding C.Term -> a -> m a
  }

-- | A pure function of the body of a binding
pureSide :: (a -> a) -> Side Identity a
pureSide f = Side (\_ -> pure . runIdentity) (\_ -> Identity . f)

-- | A function in the normalizer's monad, run with 'runNormalizeSession'
normalizeSide
  :: (Binding C.Term -> a -> NormalizeSession a) -> Side NormalizeSession a
normalizeSide = Side runNormalizeSession

-- | A comparison of a current function with its scoped port
data Comparison = forall mc ms. Comparison
  { cmpName :: String
  , cmpDescription :: String
  , cmpCurrent :: Side mc C.Term
  , cmpScoped :: Side ms (Term 'VoidS)
  , cmpKnownDifferences :: [KnownDifference]
  -- ^ Intended differences between the two, applied in order
  }

-- * Known differences

-- | An intended difference between the current function and its port: a
-- normalization applied to the results of both sides, so that results that
-- only differ in this way compare equal. Results that are only equal after
-- normalization are reported as known-different, not as equal. It gets the
-- scope of the results.
data KnownDifference = KnownDifference
  { kdName :: String
  , kdDescription :: String
  , kdNormalize :: forall n. Distinct n => Scope n -> Term n -> Term n
  }

-- | Apply known differences, in order
applyKnownDifferences :: Distinct n => [KnownDifference] -> Scope n -> Term n -> Term n
applyKnownDifferences kds sc t = List.foldl' (\acc kd -> kdNormalize kd sc acc) t kds

-- | A recursive let-binding of a single binder that doesn't refer to itself
-- is a non-recursive one.
--
-- The current traversal used to turn every 'C.NonRec' it went under into a
-- 'C.Rec', and current transformations create 'C.Rec' bindings where a port
-- can create a 'NonRec' one.
recToNonRec :: KnownDifference
recToNonRec = KnownDifference
  { kdName = "rec-to-nonrec"
  , kdDescription = "a Rec of one binding that doesn't refer to itself is a NonRec"
  , kdNormalize = \_ -> everywhereTm go
  }
 where
  go :: Term x -> Term x
  go = \case
    e@(Let (Rec (TCons b TNil) [x]) body) -> case unsinkIdBinder b x of
      Just x' -> Let (NonRec b x') body
      Nothing -> e
    e -> e

-- | Apply a function at every node of a term, bottom-up: at a node, after
-- applying it to the node's subterms. Binders are kept as they are. Doesn't
-- go into types, nor into ticks.
everywhereTm :: (forall x. Term x -> Term x) -> Term n -> Term n
everywhereTm f = go
 where
  go :: Term y -> Term y
  go e0 = f $ case e0 of
    Lam b e -> Lam b (go e)
    TyLam b e -> TyLam b (go e)
    App e1 e2 -> App (go e1) (go e2)
    TyApp e ty -> TyApp (go e) ty
    Let (NonRec b x) e -> Let (NonRec b (go x)) (go e)
    Let (Rec bs xs) e -> Let (Rec bs (map go xs)) (go e)
    Case e ty alts -> Case (go e) ty [Alt p (go a) | Alt p a <- alts]
    Cast e t1 t2 -> Cast (go e) t1 t2
    Tick t e -> Tick t (go e)
    e -> e

-- | The bindings of a recursive let-expression are in the order in which the
-- body refers to them, and then the bindings it refers to, and so on
-- (breadth-first, each term in the order of 'walkTerm'). Bindings that are
-- not referred to keep their order, at the end.
--
-- The current transformations order recursive let-bindings by unique in
-- places (e.g. 'Clash.Rewrite.Util.removeUnusedBinders'), where their ports
-- keep the order of the bindings.
letrecOrder :: KnownDifference
letrecOrder = KnownDifference
  { kdName = "letrec-order"
  , kdDescription =
      "the bindings of a letrec are in the order in which the body and the \
      \bindings refer to them"
  , kdNormalize = everywhereScopedTm go
  }
 where
  go :: Distinct x => Scope x -> Term x -> Term x
  go sc = \case
    e@(Let (Rec bs xs) body)
      | perm /= [0 .. length xs - 1] ->
          regroupLetBindings sc bs xs body [RecGroup perm] (\_ binds body' -> mkLets binds body')
      | otherwise -> e
     where
      positions = IntMap.fromList (zip (teleRaws bs) [0 ..])
      refs :: Term y -> [Int]
      refs = walkTerm $ \case
        Var _ _ n -> IntMap.lookup (tmNameRaw n) positions
        _ -> Nothing
      rhss = IntMap.fromList (zip [0 ..] xs)
      reached = visit [] IntSet.empty (refs body)
      visit acc _ [] = reverse acc
      visit acc seen (i : is)
        | i `IntSet.member` seen = visit acc seen is
        | otherwise = visit (i : acc) (IntSet.insert i seen) (is ++ maybe [] refs (IntMap.lookup i rhss))
      perm = reached ++ [i | i <- [0 .. length xs - 1], i `notElem` reached]
    e -> e

  teleRaws :: Tele IdBinder a b -> [Int]
  teleRaws = \case
    TNil -> []
    TCons b rest -> tmNameRaw (tmNameOf b) : teleRaws rest

-- | A coercion is the primitive @_CO_@, without its type as a type argument.
--
-- The primitive reductions of the scoped evaluator build vectors with
-- 'Clash.Core.Scoped.Util.primCo', which applies @_CO_@ to its type, as
-- "Clash.GHC.GHC2Core" does, where those of Clash Core use
-- 'Clash.Core.Util.primCo', which keeps the type in the 'C.PrimInfo'.
coercionForm :: KnownDifference
coercionForm = KnownDifference
  { kdName = "coercion-form"
  , kdDescription = "a coercion is the primitive _CO_, without a type argument"
  , kdNormalize = \_ -> everywhereTm go
  }
 where
  go :: Term x -> Term x
  go = \case
    TyApp e@(Prim p) _ | C.primName p == Text.pack "_CO_" -> e
    e -> e

-- | Apply a function at every node of a term, bottom-up, like
-- 'everywhereTm', giving it the scope of the node: goes under binders by
-- opening them.
everywhereScopedTm
  :: Distinct n
  => (forall x. Distinct x => Scope x -> Term x -> Term x)
  -> Scope n
  -> Term n
  -> Term n
everywhereScopedTm f = go
 where
  go :: Distinct y => Scope y -> Term y -> Term y
  go sc e0 = f sc $ case e0 of
    Lam b e -> openIdBinder sc b e $ \sc' b' e' -> Lam b' (go sc' e')
    TyLam b e -> openTyBinder sc b e $ \sc' b' e' -> TyLam b' (go sc' e')
    App e1 e2 -> App (go sc e1) (go sc e2)
    TyApp e ty -> TyApp (go sc e) ty
    Let bind e -> openBind sc bind e $ \sc' bind' e' -> case bind' of
      NonRec b x -> Let (NonRec b (go sc x)) (go sc' e')
      Rec bs xs -> Let (Rec bs (map (go sc') xs)) (go sc' e')
    Case e ty alts ->
      Case (go sc e) ty [openAlt sc a (\sc' p e' -> Alt p (go sc' e')) | a <- alts]
    Cast e t1 t2 -> Cast (go sc e) t1 t2
    Tick t e -> Tick t (go sc e)
    e -> e

-- * Running transformations

-- | How to run a single transformation over a term
data Mode
  = Once
  -- ^ Bottom-up, once at every node ('Clash.Rewrite.Combinators.bottomupR')
  | Fixpoint
  -- ^ Bottom-up traversals until the transformation no longer fires
  -- (@'Clash.Rewrite.Combinators.repeatR' . 'Clash.Rewrite.Combinators.bottomupR'@)
  deriving (Eq, Show)

-- | The strategy that runs a current transformation in the given mode: a
-- 'Clash.Rewrite.StrategyDSL.Transformation', or e.g. a
-- @'Clash.Rewrite.StrategyDSL.chain' [t1, t2]@.
-- Compile it with 'Clash.Rewrite.StrategyDSL.TH.compileStrategy', in a module
-- that imports this one, and run it with 'currentRewrite':
--
-- @
-- currentRewrite "caseLet" $(compileStrategy (strategyFor Fixpoint caseLet))
-- @
strategyFor :: ToStep s => Mode -> s -> Strat
strategyFor Once t = bottomup t
strategyFor Fixpoint t = DSL.repeatR (bottomup t)

-- | Rewrite the body of a binding the way the normalizer does (see
-- 'Clash.Normalize.Util.rewriteExpr'), e.g. with a strategy compiled from
-- 'strategyFor'
currentRewrite :: String -> NormRewrite -> Side NormalizeSession C.Term
currentRewrite nm rw = normalizeSide $ \b tm -> do
  curFun Lens..= (bindingId b, bindingLoc b)
  runRewrite nm emptyInScopeSet rw tm

-- | Rewrite the body of a binding with a scoped transformation, in the given
-- mode: bottom-up once at every node ('Clash.Rewrite.Scoped.bottomupR'), or
-- until it no longer fires ('Clash.Rewrite.Scoped.repeatR'). The scoped
-- counterpart of 'currentRewrite' with 'strategyFor'.
scopedRewrite :: Mode -> ScopedRewrite NormalizeState -> Side NormalizeSession (Term 'VoidS)
scopedRewrite mode r = normalizeSide $ \b tm -> do
  curFun Lens..= (bindingId b, bindingLoc b)
  runScopedTransform strategy (rootContext emptyScope) tm
 where
  strategy = case mode of
    Once -> bottomupR r
    Fixpoint -> repeatR (bottomupR r)

-- * Comparing

-- | The outcome of comparing the results for one binding
data Outcome
  = Equal
  | KnownDifferent [String]
  -- ^ Equal after applying the known differences with these names
  | Different Mismatch

-- | Why the results for a binding are different
data Mismatch
  = Mismatch
      { mmDiff :: Maybe Diff
      -- ^ The first difference. 'Nothing' if 'firstDiff' doesn't find one,
      -- which is a bug in 'firstDiff'.
      , mmCurrent :: C.Term
      , mmScoped :: C.Term
      -- ^ Both results, after applying the known differences
      }
  | Crash
      { crashStage :: String
      -- ^ Where: running either side, converting the result, comparing
      , crashMessage :: String
      }

-- | Run an action, turning a synchronous exception into a 'Crash'
attempt :: String -> IO a -> IO (Either Mismatch a)
attempt stage act = try act >>= \case
  Right a -> pure (Right a)
  Left (e :: SomeException)
    | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
    | otherwise -> pure (Left (Crash stage (show e)))

-- | Fully evaluate a scoped term
forceTm :: Term n -> IO (Term n)
forceTm t = evaluate (rnf (toClashTerm t)) >> pure t

-- | Run both sides of a comparison on a binding, and compare their results
compareBinding :: Comparison -> Design -> Binding C.Term -> IO Outcome
compareBinding (Comparison _ _ cur sco kds) d b = either Different id <$> do
  let tm = bindingTerm b
  rc <- attempt "current side" $
    sideRun cur d (sideFun cur b tm) >>= evaluate . force
  rs <- attempt "scoped side" $
    forceTm (fromClashTermClosed tm) >>= sideRun sco d . sideFun sco b >>= forceTm
  case (rc, rs) of
    (Left m, _) -> pure (Left m)
    (_, Left m) -> pure (Left m)
    (Right c, Right s) -> do
      sc <- attempt "converting the result of the current side" $
        forceTm (fromClashTermClosed c)
      case sc of
        Left m -> pure (Left m)
        Right c' -> attempt "comparing" (evaluate (compareResults kds emptyScope c' s))

-- | Compare the results of both sides, in the given scope
compareResults :: Distinct n => [KnownDifference] -> Scope n -> Term n -> Term n -> Outcome
compareResults kds sc c s
  | aeqTm c s = Equal
  | aeqTm c' s' = KnownDifferent (explaining 1)
  | otherwise = Different (Mismatch (firstDiff c' s') (toClashTerm c') (toClashTerm s'))
 where
  c' = applyKnownDifferences kds sc c
  s' = applyKnownDifferences kds sc s

  -- The shortest prefix of the known differences that explains the
  -- difference
  explaining k
    | k >= length kds = map kdName kds
    | aeqTm (applyKnownDifferences (take k kds) sc c) (applyKnownDifferences (take k kds) sc s) =
        map kdName (take k kds)
    | otherwise = explaining (k + 1)

-- | The results of comparing on all bindings of a design
data Summary = Summary
  { sumCompared :: !Int
  , sumEqual :: !Int
  , sumKnownDifferent :: !(Map String Int)
  -- ^ By the names of the known differences that explain them
  , sumDifferent :: !Int
  , sumCrashed :: !Int
  -- ^ How many of the different ones crashed
  , sumMismatches :: [(Id, Mismatch)]
  -- ^ The first mismatches, see 'compareDesign'
  }

instance Semigroup Summary where
  Summary c1 e1 k1 d1 x1 m1 <> Summary c2 e2 k2 d2 x2 m2 =
    Summary (c1 + c2) (e1 + e2) (Map.unionWith (+) k1 k2) (d1 + d2) (x1 + x2) (m1 <> m2)

instance Monoid Summary where
  mempty = Summary 0 0 Map.empty 0 0 []

-- | Compare on all bindings of a design. Keeps the first @n@ mismatches.
compareDesign :: Int -> Comparison -> Design -> IO Summary
compareDesign n cmp d = go mempty (designBindingList d)
 where
  go s [] = pure s {sumMismatches = reverse (sumMismatches s)}
  go !s (b:bs) = do
    o <- compareBinding cmp d b
    let s1 = s {sumCompared = sumCompared s + 1}
    go (case o of
          Equal -> s1 {sumEqual = sumEqual s + 1}
          KnownDifferent kds ->
            s1 {sumKnownDifferent = Map.insertWith (+) (List.intercalate "+" kds) 1 (sumKnownDifferent s)}
          Different m -> s1
            { sumDifferent = sumDifferent s + 1
            , sumCrashed = sumCrashed s + (case m of Crash {} -> 1; Mismatch {} -> 0)
            , sumMismatches =
                if sumDifferent s < n then (bindingId b, m) : sumMismatches s else sumMismatches s
            })
       bs

-- * Differences

-- | Where two terms first differ: the path to the differing subterms (or
-- types), and both of them, pretty printed
data Diff = Diff
  { diffPath :: [String]
  , diffCurrent :: String
  , diffScoped :: String
  }

-- | The first place where two terms are not alpha-equivalent, in the order
-- in which 'aeqTm' compares them. Agrees with 'aeqTm': it finds a difference
-- if and only if the terms are not alpha-equivalent.
firstDiff :: Term n -> Term n -> Maybe Diff
firstDiff = diffTm emptyLv []

-- | The level of the next binder, and the levels of the bound names of either
-- side, as in "Clash.Core.Scoped.Alpha"
data Lv = Lv !Int !(IntMap Int) !(IntMap Int)

emptyLv :: Lv
emptyLv = Lv 0 IntMap.empty IntMap.empty

under :: Int -> Int -> Lv -> Lv
under k1 k2 (Lv l m1 m2) = Lv (l + 1) (IntMap.insert k1 l m1) (IntMap.insert k2 l m2)

sameName :: Lv -> Int -> Int -> Bool
sameName (Lv _ m1 m2) n1 n2 = case (IntMap.lookup n1 m1, IntMap.lookup n2 m2) of
  (Just l1, Just l2) -> l1 == l2
  (Nothing, Nothing) -> n1 == n2
  _ -> False

hereTm :: [String] -> Term x -> Term y -> Maybe Diff
hereTm path t1 t2 =
  Just (Diff (reverse path) (showPpr (toClashTerm t1)) (showPpr (toClashTerm t2)))

hereTy :: [String] -> Type x -> Type y -> Maybe Diff
hereTy path t1 t2 =
  Just (Diff (reverse path) (showPpr (toClashType t1)) (showPpr (toClashType t2)))

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (\m r -> maybe r Just m) Nothing

-- | The first difference of two lists of equal length, with an index in the
-- path
diffList :: String -> [String] -> (a -> b -> [String] -> Maybe Diff) -> [a] -> [b] -> Maybe Diff
diffList step path f xs ys =
  firstJust [f x y ((step <> "[" <> show i <> "]") : path) | (i, x, y) <- zip3 [(0 :: Int)..] xs ys]

diffTy :: Lv -> [String] -> Type x -> Type y -> Maybe Diff
diffTy lv path t1 t2 = case (t1, t2) of
  (VarTy _ _ n1, VarTy _ _ n2) | sameName lv (tyNameRaw n1) (tyNameRaw n2) -> Nothing
  (ConstTy c1, ConstTy c2) | c1 == c2 -> Nothing
  (ForAllTy b1 s1, ForAllTy b2 s2) -> firstJust
    [ diffTy lv ("ForAllTy.kind" : path) (tyBinderKind b1) (tyBinderKind b2)
    , diffTy (underTy b1 b2 lv) ("ForAllTy.body" : path) s1 s2 ]
  (AppTy f1 a1, AppTy f2 a2) -> firstJust
    [ diffTy lv ("AppTy.fun" : path) f1 f2
    , diffTy lv ("AppTy.arg" : path) a1 a2 ]
  (LitTy l1, LitTy l2) | l1 == l2 -> Nothing
  (AnnType a1 s1, AnnType a2 s2) | a1 == a2 -> diffTy lv ("AnnType.type" : path) s1 s2
  _ -> hereTy path t1 t2

underTy :: TyBinder x x' -> TyBinder y y' -> Lv -> Lv
underTy b1 b2 = under (tyNameRaw (tyNameOf b1)) (tyNameRaw (tyNameOf b2))

underId :: IdBinder x x' -> IdBinder y y' -> Lv -> Lv
underId b1 b2 = under (tmNameRaw (tmNameOf b1)) (tmNameRaw (tmNameOf b2))

diffTm :: Lv -> [String] -> Term x -> Term y -> Maybe Diff
diffTm lv path t1 t2 = case (t1, t2) of
  (Var _ _ n1, Var _ _ n2) | sameName lv (tmNameRaw n1) (tmNameRaw n2) -> Nothing
  (GlobalVar g1, GlobalVar g2) | g1 == g2 -> Nothing
  (Data dc1, Data dc2) | dc1 == dc2 -> Nothing
  (Literal l1, Literal l2) | l1 == l2 -> Nothing
  (Prim p1, Prim p2) | C.primName p1 == C.primName p2 -> Nothing
  (Lam b1 e1, Lam b2 e2) -> firstJust
    [ diffTy lv ("Lam.type" : path) (idBinderType b1) (idBinderType b2)
    , diffTm (underId b1 b2 lv) ("Lam.body" : path) e1 e2 ]
  (TyLam b1 e1, TyLam b2 e2) -> firstJust
    [ diffTy lv ("TyLam.kind" : path) (tyBinderKind b1) (tyBinderKind b2)
    , diffTm (underTy b1 b2 lv) ("TyLam.body" : path) e1 e2 ]
  (App f1 a1, App f2 a2) -> firstJust
    [ diffTm lv ("App.fun" : path) f1 f2
    , diffTm lv ("App.arg" : path) a1 a2 ]
  (TyApp e1 s1, TyApp e2 s2) -> firstJust
    [ diffTm lv ("TyApp.fun" : path) e1 e2
    , diffTy lv ("TyApp.type" : path) s1 s2 ]
  (Let (NonRec b1 x1) e1, Let (NonRec b2 x2) e2) -> firstJust
    [ diffTy lv ("Let.type" : path) (idBinderType b1) (idBinderType b2)
    , diffTm lv ("Let.rhs" : path) x1 x2
    , diffTm (underId b1 b2 lv) ("Let.body" : path) e1 e2 ]
  (Let (Rec bs1 xs1) e1, Let (Rec bs2 xs2) e2)
    | teleLength bs1 == teleLength bs2 && length xs1 == length xs2 ->
        diffIdTele lv ("Letrec" : path) bs1 bs2 $ \lv1 -> firstJust
          [ diffList "Letrec.rhs" path (\x1 x2 p -> diffTm lv1 p x1 x2) xs1 xs2
          , diffTm lv1 ("Letrec.body" : path) e1 e2 ]
  (Case e1 _ as1, Case e2 _ as2) | length as1 == length as2 -> firstJust
    [ diffTm lv ("Case.subject" : path) e1 e2
    , diffList "Case.alt" path
        (\a1 a2 p -> diffAlt lv a1 a2 p (hereTm ("Alt.pattern" : p) t1 t2)) as1 as2 ]
  (Cast e1 f1 to1, Cast e2 f2 to2) -> firstJust
    [ diffTm lv ("Cast.body" : path) e1 e2
    , diffTy lv ("Cast.from" : path) f1 f2
    , diffTy lv ("Cast.to" : path) to1 to2 ]
  (Tick i1 e1, Tick i2 e2) -> firstJust
    [ diffTick lv ("Tick.info" : path) i1 i2
    , diffTm lv ("Tick.body" : path) e1 e2 ]
  _ -> hereTm path t1 t2

-- | Compare the types of two telescopes of term binders, and continue under
-- them. They have the same length.
diffIdTele
  :: Lv -> [String] -> Tele IdBinder x x' -> Tele IdBinder y y'
  -> (Lv -> Maybe Diff) -> Maybe Diff
diffIdTele lv path = go lv (0 :: Int)
 where
  go :: Lv -> Int -> Tele IdBinder a a' -> Tele IdBinder b b' -> (Lv -> Maybe Diff) -> Maybe Diff
  go l i (TCons b1 bs1) (TCons b2 bs2) k = firstJust
    [ diffTy l (("type[" <> show i <> "]") : path) (idBinderType b1) (idBinderType b2)
    , go (underId b1 b2 l) (i + 1) bs1 bs2 k ]
  go l _ _ _ k = k l

-- | Like 'diffIdTele', for type binders
diffTyTele
  :: Lv -> [String] -> Tele TyBinder x x' -> Tele TyBinder y y'
  -> (Lv -> Maybe Diff) -> Maybe Diff
diffTyTele lv path = go lv (0 :: Int)
 where
  go :: Lv -> Int -> Tele TyBinder a a' -> Tele TyBinder b b' -> (Lv -> Maybe Diff) -> Maybe Diff
  go l i (TCons b1 bs1) (TCons b2 bs2) k = firstJust
    [ diffTy l (("kind[" <> show i <> "]") : path) (tyBinderKind b1) (tyBinderKind b2)
    , go (underTy b1 b2 l) (i + 1) bs1 bs2 k ]
  go l _ _ _ k = k l

-- | Compare two alternatives. If their patterns differ, the result is the
-- last argument.
diffAlt :: Lv -> Alt x -> Alt y -> [String] -> Maybe Diff -> Maybe Diff
diffAlt lv (Alt p1 e1) (Alt p2 e2) path patternsDiffer = case (p1, p2) of
  (DataPat dc1 tvs1 ids1, DataPat dc2 tvs2 ids2)
    | dc1 == dc2
    , teleLength tvs1 == teleLength tvs2
    , teleLength ids1 == teleLength ids2 ->
        diffTyTele lv ("DataPat.tyvars" : path) tvs1 tvs2 $ \lv1 ->
          diffIdTele lv1 ("DataPat.ids" : path) ids1 ids2 $ \lv2 ->
            diffTm lv2 ("Alt.body" : path) e1 e2
  (LitPat l1, LitPat l2) | l1 == l2 -> diffTm lv ("Alt.body" : path) e1 e2
  (DefaultPat, DefaultPat) -> diffTm lv ("Alt.body" : path) e1 e2
  _ -> patternsDiffer

diffTick :: Lv -> [String] -> TickInfo x -> TickInfo y -> Maybe Diff
diffTick lv path i1 i2 = case (i1, i2) of
  (SrcSpan s1, SrcSpan s2) | s1 == s2 -> Nothing
  (NameMod m1 t1, NameMod m2 t2) | m1 == m2 -> diffTy lv ("NameMod.type" : path) t1 t2
  (DeDup, DeDup) -> Nothing
  (NoDeDup, NoDeDup) -> Nothing
  (Attributes t1 e1, Attributes t2 e2) -> firstJust
    [ diffTy lv ("Attributes.type" : path) t1 t2
    , diffTm lv ("Attributes.term" : path) e1 e2 ]
  _ -> Just (Diff (reverse path) (showTick i1) (showTick i2))
 where
  showTick :: TickInfo z -> String
  showTick = \case
    SrcSpan s -> "SrcSpan " <> showPpr s
    NameMod m t -> "NameMod " <> show m <> " " <> showPpr (toClashType t)
    DeDup -> "DeDup"
    NoDeDup -> "NoDeDup"
    Attributes t e -> "Attributes " <> showPpr (toClashType t) <> " " <> showPpr (toClashTerm e)

-- * Reporting

-- | At most @l@ lines of at most @w@ characters
truncateText :: Int -> Int -> String -> String
truncateText l w s = List.intercalate "\n" (map line (take l ls) <> more)
 where
  ls = lines s
  line x = case splitAt w x of
    (x', []) -> x'
    (x', _) -> x' <> "..."
  more = ["... (" <> show (length ls - l) <> " more lines)" | not (null (drop l ls))]

indent :: String -> String
indent = List.intercalate "\n" . map ("    " <>) . lines

-- | A summary of comparing on a design, on one line
renderSummary :: String -> Summary -> String
renderSummary nm s =
  nm <> ": " <> show (sumCompared s) <> " bindings compared, "
    <> show (sumEqual s) <> " equal, "
    <> show (sum (sumKnownDifferent s)) <> " known-different"
    <> known
    <> ", " <> show (sumDifferent s) <> " different"
    <> (if sumCrashed s > 0 then " (" <> show (sumCrashed s) <> " crashed)" else "")
 where
  known
    | Map.null (sumKnownDifferent s) = ""
    | otherwise = " (" <> List.intercalate ", "
        [k <> ": " <> show v | (k, v) <- Map.toList (sumKnownDifferent s)] <> ")"

-- | A mismatch of a binding, with the terms truncated to @l@ lines of @w@
-- characters
renderMismatch :: Int -> Int -> Id -> Mismatch -> String
renderMismatch l w i m = unlines $
  ("--- " <> Text.unpack (nameOcc (varName i))) : case m of
    Crash stage msg ->
      ["crashed in " <> stage <> ":", indent (truncateText l w msg)]
    Mismatch d c s ->
      (case d of
         Nothing -> ["no differing subterm found (a bug in firstDiff)"]
         Just (Diff path dc ds) ->
           [ "first difference at: " <> (if null path then "the root" else List.intercalate " / " path)
           , "  current:"
           , indent (truncateText l w dc)
           , "  scoped:"
           , indent (truncateText l w ds) ]) <>
      [ "current result:"
      , indent (truncateText l w (showPpr c))
      , "scoped result:"
      , indent (truncateText l w (showPpr s)) ]
