{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Helpers to test the scoped evaluator ("Clash.Core.Scoped.Evaluator") and
  its reductions of primitives against the evaluator of Clash Core. Shared by
  the unit tests and @clash-differential@, which has the reductions of Clash
  Core (@Clash.GHC.Evaluator.evaluator@).

  = Comparing a reduction with the adapter

  A ported reduction is compared with the reduction of Clash Core through the
  adapter ("Clash.Core.Scoped.Evaluator.Adapter"):

  * 'comparePrimCall' evaluates a call of a primitive, given as a Clash Core
    term, once with the ported reduction and once with the adapter.

  * 'compareReductions' runs two reductions on the same arguments and machine,
    e.g. inside a real evaluation: see 'recordingEnv'.

  Results are compared up to alpha-equivalence, after unfolding the bindings
  allocated around them ('sameResult'): the two sides name and order those
  bindings differently.

  = Generating calls

  'genPrimCall' generates calls of a primitive from its type: literals for
  the primitive types, numbers for the type-level naturals, values of the
  sized types, of vectors and of trees, and constructors of other data types.
  'genOpenPrimCall' also generates calls whose arguments it can't generate
  otherwise, such as class dictionaries, with opaque values.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- 'recordingEnv' must record every reduction
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}

module Test.Clash.Scoped.Evaluator
  ( -- * Evaluating
    scopedWhnf
    -- * Environments
  , portedEnv
  , adapterEnv
  , onlyPortedEnv
    -- * Comparing results
  , readback
  , canonicalCoercions
  , sameResult
  , termSize
    -- * Comparing reductions
  , PrimOutcome (..)
  , outcomeKey
  , comparePrimCall
  , Check (..)
  , compareReductions
    -- * Recording reductions
  , PrimVia (..)
  , PrimEvent (..)
  , recordingEnv
    -- * Generating calls
  , genPrimCall
  , genOpenPrimCall
  , genArg
  ) where

import Control.Monad (forM, replicateM)
import qualified Control.Lens as Lens
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, modifyIORef')
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.QuickCheck (Gen, choose, chooseInteger, elements, frequency, oneof)

import Control.Monad.Trans.Except (runExcept)
import qualified Clash.Core.Evaluator.Types as CE
import Clash.Core.DataCon (DataCon (..))
import qualified Clash.Core.FreeVars as C
import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Name as C
import Clash.Core.Pretty (showPpr)
import qualified Clash.Core.Subst as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.TyCon as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import qualified Clash.Core.Util as C
import qualified Clash.Core.Var as C
import qualified Clash.Core.VarEnv as C
import qualified Clash.Data.UniqMap as UniqMap
import qualified Clash.Normalize.Primitives as NP
import Clash.Driver.Types (BindingMap)

import Clash.Core.Scoped.Convert (convScope, fromClashTerm, toClashTerm, withConvEnv)
import Clash.Core.Scoped.Evaluator (bindAllocated, unwindStack, whnf)
import Clash.Core.Scoped.Evaluator.Adapter (adaptPrim)
import Clash.Core.Scoped.Evaluator.Primitives (scopedPrimReductions)
import qualified Clash.Core.Scoped.Evaluator.Primitives.Util as U
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.Syntax (Distinct, S (VoidS), Term)
import Clash.Core.Scoped.NameMap (emptyNameMap)
import Clash.Core.Scoped.TypeEnv (TypeEnv)

-- * Evaluating

-- | Evaluate a Clash term to WHNF with the evaluator of scoped Clash Core, in
-- the scope of its free variables, with the given environment and global
-- bindings. Gives the result, bound by the bindings the evaluator allocated,
-- as Clash Core: its free variables keep their uniques.
scopedWhnf :: Env -> BindingMap -> Bool -> C.Term -> C.Term
scopedWhnf env bm isSubj t = withConvEnv tvs ids $ \cenv ->
  let m0 = initMachine (convScope cenv) emptyNameMap bm (IntMap.empty, 0) (fromClashTerm cenv t)
  in whnf env isSubj m0 $ \m -> toClashTerm (bindAllocated m (mTerm m))
 where
  tvs = C.dependencySortTyVars (List.nubBy sameUniq (Lens.toListOf C.termFreeTyVars t))
  ids = List.nubBy sameUniq (Lens.toListOf C.freeLocalIds t)
  sameUniq :: C.Var a -> C.Var a -> Bool
  sameUniq v1 v2 = C.varUniq v1 == C.varUniq v2

-- * Environments

-- | The ported reductions, and the adapter for the other primitives: the
-- environment of 'Clash.Rewrite.Scoped.Whnf.whnfRW'
portedEnv :: TypeEnv -> CE.Evaluator -> Env
portedEnv te eval = Env te (PrimReductions scopedPrimReductions (Just (adaptPrim eval)))

-- | The adapter for every primitive
adapterEnv :: TypeEnv -> CE.Evaluator -> Env
adapterEnv te eval = Env te (PrimReductions HashMap.empty (Just (adaptPrim eval)))

-- | The ported reduction of one primitive, and the adapter for the others
onlyPortedEnv :: TypeEnv -> CE.Evaluator -> Text -> PrimReduction -> Env
onlyPortedEnv te eval nm r =
  Env te (PrimReductions (HashMap.singleton nm r) (Just (adaptPrim eval)))

-- * Comparing results

-- | Unfold the top-level @letrec@ of a result a number of times, and replace
-- the variables it binds that remain by a placeholder. The evaluators name
-- and order the bindings they allocate differently, and the evaluator of
-- Clash Core keeps the value a binding was updated with, where the scoped one
-- may keep the right-hand side it allocated.
readback :: Int -> C.Term -> C.Term
readback depth t0 = case t0 of
  C.Letrec bs body ->
    let env = C.mkVarEnv bs
        fvs = C.mkInScopeSet (Lens.foldMapOf C.freeLocalVars C.unitVarSet t0 <> C.mkVarSet (map fst bs))
        unfold = C.substTm "readback" (C.extendIdSubstList (C.mkSubst fvs) bs)
        cut = C.substTm "readback" (C.extendIdSubstList (C.mkSubst fvs) [(v, cutPrim) | (v, _) <- bs])
        go 0 e = cut e
        go i e
          | not (any (`C.elemVarEnv` env) (Lens.toListOf C.freeLocalIds e)) = e
          | termSize e > 200000 = cut e
          | otherwise = go (i - 1 :: Int) (unfold e)
    in go depth body
  _ -> t0
 where
  cutPrim = C.Prim (C.PrimInfo "<heap>" (C.ConstTy C.Arrow) C.WorkNever C.SingleResult C.NoUnfolding)

-- | Make the coercion arguments of data constructors the same: those that the
-- scoped reductions build are applications of @_CO_@ to their type (see
-- 'Clash.Core.Scoped.Util.primCo'), those of Clash Core have the type in the
-- primitive.
canonicalCoercions :: C.Term -> C.Term
canonicalCoercions = go
 where
  go = \case
    C.TyApp (C.Prim p) _ | C.primName p == "_CO_" -> coPrim
    C.Prim p | C.primName p == "_CO_" -> coPrim
    C.Lam v e -> C.Lam v (go e)
    C.TyLam v e -> C.TyLam v (go e)
    C.App e1 e2 -> C.App (go e1) (go e2)
    C.TyApp e ty -> C.TyApp (go e) ty
    C.Let (C.NonRec v x) e -> C.Let (C.NonRec v (go x)) (go e)
    C.Let (C.Rec xs) e -> C.Let (C.Rec [(v, go x) | (v, x) <- xs]) (go e)
    C.Case e ty alts -> C.Case (go e) ty [(p, go a) | (p, a) <- alts]
    C.Cast e t1 t2 -> C.Cast (go e) t1 t2
    C.Tick t e -> C.Tick t (go e)
    e -> e
  coPrim = C.Prim (C.PrimInfo "_CO_" (C.ConstTy C.Arrow) C.WorkNever C.SingleResult C.NoUnfolding)

-- | Whether two results of evaluators are the same, see 'readback' and
-- 'canonicalCoercions'. Their free variables must be those of one scope.
sameResult :: C.Term -> C.Term -> Bool
sameResult t1 t2 = C.aeqTerm (norm t1) (norm t2)
 where
  norm = canonicalCoercions . readback 2

-- | Number of term nodes (forces the term, not the types)
termSize :: C.Term -> Int
termSize = go 0
 where
  go !acc = \case
    C.Lam _ e -> go (acc + 1) e
    C.TyLam _ e -> go (acc + 1) e
    C.App e1 e2 -> go (go (acc + 1) e1) e2
    C.TyApp e _ -> go (acc + 1) e
    C.Let (C.NonRec _ x) e -> go (go (acc + 1) x) e
    C.Let (C.Rec xs) e -> go (List.foldl' go (acc + 1) (map snd xs)) e
    C.Case e _ alts -> List.foldl' go (go (acc + 1) e) (map snd alts)
    C.Cast e _ _ -> go (acc + 1) e
    C.Tick _ e -> go (acc + 1) e
    _ -> acc + 1

-- * Comparing reductions

-- | The outcome of comparing a ported reduction with the reference, the
-- reduction of Clash Core through the adapter. Results are shown with
-- 'showPpr'.
data PrimOutcome
  = BothStuck
  -- ^ Neither reduces
  | Agree
  | Differ String String
  -- ^ Both reduce, to the ported and the reference result
  | OnlyPorted String
  -- ^ Only the ported reduction reduces, to this result
  | OnlyReference String
  -- ^ Only the reference reduces, to this result
  deriving (Eq, Show)

-- | The kind of an outcome, for counting
outcomeKey :: PrimOutcome -> String
outcomeKey = \case
  BothStuck -> "both stuck"
  Agree -> "agree"
  Differ {} -> "differ"
  OnlyPorted {} -> "only ported reduces"
  OnlyReference {} -> "only reference reduces"

-- | Evaluate a call of a primitive (an application of the primitive to its
-- type and term arguments, possibly under let-bindings that it can refer to)
-- to WHNF, with the ported reduction of the primitive and with the reduction
-- of Clash Core through the adapter, and compare the results. Other
-- primitives are reduced by the adapter on both sides.
comparePrimCall
  :: TypeEnv
  -> CE.Evaluator
  -- ^ With the reference reductions
  -> BindingMap
  -> Text
  -- ^ The name of the primitive
  -> PrimReduction
  -- ^ Its ported reduction
  -> Bool
  -- ^ Whether the call is the subject of a case-expression
  -> C.Term
  -> PrimOutcome
comparePrimCall te eval bm nm r isSubj call =
  case (reduced ported, reduced reference) of
    (Nothing, Nothing) -> BothStuck
    (Just p, Nothing) -> OnlyPorted (showPpr p)
    (Nothing, Just q) -> OnlyReference (showPpr q)
    (Just p, Just q)
      | sameResult p q -> Agree
      | otherwise -> Differ (showPpr p) (showPpr q)
 where
  ported = scopedWhnf (onlyPortedEnv te eval nm r) bm isSubj call
  reference = scopedWhnf (adapterEnv te eval) bm isSubj call
  -- A call that doesn't reduce evaluates to itself, with the bindings it is
  -- under allocated around it
  reduced res
    | sameResult res call || sameResult (body res) (body call) = Nothing
    | otherwise = Just res
  body = \case
    C.Letrec _ e -> body e
    e -> e

-- | A comparison of a reduction with a reference reduction, in the context of
-- a call
newtype Check = Check (forall o n. Distinct n => PrimContext o n -> PrimOutcome)

-- | Run a ported reduction and a reference reduction on the same arguments,
-- and compare their results: the term each continues with, bound by the
-- bindings each allocated, and the primitive heap. Both run in the machine of
-- the call with an empty stack, so a reduction that evaluates with a nested
-- machine is compared on the WHNF of what it evaluated.
compareReductions :: PrimReduction -> PrimReduction -> Check
compareReductions ported reference = Check $ \pc ->
  let m0 = rootMachine (pcMachine pc)
      pc0 = pc {pcMachine = m0}
      run r = case runPrimReduction r pc0 of
        Nothing -> Nothing
        Just (Next m) ->
          let m1 = unwindStack m
          in Just (toClashTerm (bindAllocated m1 (mTerm m1)), fst (mHeapPrim m1))
  in case (run ported, run reference) of
       (Nothing, Nothing) -> BothStuck
       (Just (p, _), Nothing) -> OnlyPorted (showPpr p)
       (Nothing, Just (q, _)) -> OnlyReference (showPpr q)
       (Just (p, hp), Just (q, hq))
         | sameResult p q
         , IntMap.keys hp == IntMap.keys hq
         , and (zipWith C.aeqTerm (IntMap.elems hp) (IntMap.elems hq))
         -> Agree
         | otherwise -> Differ (showPpr p) (showPpr q)
 where
  -- The machine of the call, as one of the caller: the bindings it allocated
  -- before are in its heap, but not bound around its result
  rootMachine :: Machine o n -> Machine n n
  rootMachine m = m {mAlloc = ANil, mStack = []}

-- * Recording reductions

-- | Whether a primitive was reduced by a ported reduction, or through the
-- adapter
data PrimVia = Ported | Adapted
  deriving (Eq, Ord, Show)

-- | A call of the reduction of a primitive
data PrimEvent = PrimEvent
  { peName :: Text
  , peVia :: PrimVia
  , peReduced :: Bool
  -- ^ Whether the reduction reduced the primitive
  , peCheck :: Maybe PrimOutcome
  -- ^ The comparison with the adapter of a ported reduction, lazily
  }

-- | 'portedEnv', which records every call of a reduction, latest first. If
-- asked to, it also compares every call of a ported reduction with the
-- adapter ('compareReductions').
--
-- For diagnostics only: it records with 'unsafePerformIO', when the machine
-- demands the result of a reduction.
recordingEnv
  :: IORef [PrimEvent]
  -> Bool
  -- ^ Whether to compare ported reductions with the adapter
  -> TypeEnv
  -> CE.Evaluator
  -> Env
recordingEnv ref check te eval =
  Env te (PrimReductions (HashMap.mapWithKey ported scopedPrimReductions) (Just adapted))
 where
  adapter = adaptPrim eval
  ported nm r =
    record ref (const nm) Ported (if check then Just (compareReductions r adapter) else Nothing) r
  adapted = record ref (C.primName . pcInfo') Adapted Nothing adapter
  pcInfo' :: PrimContext o n -> C.PrimInfo
  pcInfo' = pcInfo

-- | Record the calls of a reduction
record
  :: IORef [PrimEvent]
  -> (forall o n. PrimContext o n -> Text)
  -> PrimVia
  -> Maybe Check
  -> PrimReduction
  -> PrimReduction
record ref name via check r = PrimReduction $ \pc ->
  let res = runPrimReduction r pc
      outcome = fmap (\(Check c) -> c pc) check
  in unsafePerformIO $ do
       modifyIORef' ref (PrimEvent (name pc) via (isJust res) outcome :)
       pure res
{-# NOINLINE record #-}

-- * Generating calls

-- | A call of a primitive with arguments generated from its type, see
-- 'genArg'. Quantified type variables of kind @Nat@ get small numbers, those
-- of kind @Type@ get @Integer@. 'Nothing' if it can't generate an argument.
genPrimCall :: C.TyConMap -> C.PrimInfo -> Gen (Maybe C.Term)
genPrimCall = genPrimCallWith False

-- | 'genPrimCall', but with an opaque value for every argument it can't
-- generate ('opaqueArg'), e.g. a class dictionary or a polymorphic function,
-- and a free type variable for every quantified type variable of another kind
-- than @Nat@ and @Type@. A reduction can't look into those, but it can build
-- its result with them, which the reductions of e.g. @dfold@ and @traverse#@
-- do.
genOpenPrimCall :: C.TyConMap -> C.PrimInfo -> Gen C.Term
genOpenPrimCall tcm p = genPrimCallWith True tcm p >>= \case
  Just call -> pure call
  Nothing -> error "genOpenPrimCall: generated no call"

-- | 'genPrimCall', or 'genOpenPrimCall'
genPrimCallWith :: Bool -> C.TyConMap -> C.PrimInfo -> Gen (Maybe C.Term)
genPrimCallWith open tcm p =
  go (0 :: Int) (C.mkSubst C.emptyInScopeSet) [] (fst (C.splitFunForallTy (C.primType p)))
 where
  go _ _ acc [] = pure (Just (C.mkApps (C.Prim p) (reverse acc)))
  go i subst acc (Left tv : rest)
    | C.aeqType (C.varType tv) C.typeNatKind = do
        n <- frequency [(6, chooseInteger (0, 8)), (1, elements [16, 32, 64, 65])]
        let ty = C.LitTy (C.NumTy n)
        go (i + 1) (C.extendTvSubst subst tv ty) (Right ty : acc) rest
    | isLiftedTypeKind (C.varType tv) =
        go (i + 1) (C.extendTvSubst subst tv C.integerPrimTy) (Right C.integerPrimTy : acc) rest
    | open =
        let tv' = C.mkTyVar (C.substTy subst (C.varType tv)) (freeName "t" i)
            subst' = C.extendTvSubst (extendInScopeTv subst tv') tv (C.VarTy tv')
        in go (i + 1) subst' (Right (C.VarTy tv') : acc) rest
    | otherwise = pure Nothing
  go i subst acc (Right argTy : rest) = do
    let argTy' = C.substTy subst argTy
    genArg tcm argTy' >>= \case
      Just a -> go (i + 1) subst (Left a : acc) rest
      Nothing
        | open -> go (i + 1) subst (Left (opaqueArg tcm i argTy') : acc) rest
        | otherwise -> pure Nothing

  -- Free variables get uniques of their own: those of the binders of the
  -- generated arguments are small
  freeName :: Text -> Int -> C.Name a
  freeName nm i = C.mkUnsafeSystemName (nm <> Text.pack (show i)) (7000000 + fromIntegral i)

  extendInScopeTv (C.Subst is env tenv genv) tv = C.Subst (C.extendInScopeSet is tv) env tenv genv

-- | An opaque value of the given type, the argument with the given index of a
-- generated call: a function (or a type abstraction) that applies a free
-- variable to its arguments; the only constructor of a data type, e.g. of a
-- class dictionary, applied to opaque values; or 'NP.removedArg'. Arguments
-- of primitives are evaluated, so they must be values: a free variable itself
-- would make the call stuck.
opaqueArg :: C.TyConMap -> Int -> C.Type -> C.Term
opaqueArg tcm i = opaque (3 :: Int) (fromIntegral i)
 where
  -- Every opaque value has a number of its own, from which the uniques of its
  -- variables follow: the fields of the value with number @k@ get the numbers
  -- @32 * k + 1@ and up.
  opaque depth k ty
    | isFun ty = eta k 0 ty (C.Var (C.mkLocalId ty (name "arg" k 0)))
    | depth > 0
    , C.TyConApp tc tyArgs <- C.tyView ty
    , Just tyCon <- UniqMap.lookup tc tcm
    , not (C.isNewTypeTc tyCon)
    , [dc] <- C.tyConDataCons tyCon
    , null (dcExtTyVars dc)
    , Just fieldTys <- C.dataConInstArgTys dc tyArgs
    = C.mkApps (C.Data dc) $ map Right tyArgs ++
        [ Left (opaque (depth - 1) (32 * k + j) fieldTy) | (j, fieldTy) <- zip [1 ..] fieldTys ]
    | otherwise = C.TyApp (C.Prim NP.removedArg) ty

  isFun = \case
    C.ForAllTy {} -> True
    (C.tyView -> C.FunTy {}) -> True
    _ -> False

  -- Abstract over the arguments of a function type, and apply the term to them
  eta :: Integer -> Integer -> C.Type -> C.Term -> C.Term
  eta k j ty e = case ty of
    C.ForAllTy tv res -> C.TyLam tv (eta k j res (C.TyApp e (C.VarTy tv)))
    (C.tyView -> C.FunTy a r) ->
      let x = C.mkLocalId a (name "x" k (j + 1))
      in C.Lam x (eta k (j + 1) r (C.App e (C.Var x)))
    _ -> e

  -- The @j@-th variable of the opaque value with number @k@
  name :: Text -> Integer -> Integer -> C.Name a
  name nm k j =
    C.mkUnsafeSystemName (nm <> Text.pack (show k)) (fromInteger (7000000000 + 64 * k + j))

-- | Whether a kind is that of lifted types: @Type@, or @TYPE (BoxedRep
-- Lifted)@ as "Clash.GHC.GHC2Core" gives it
isLiftedTypeKind :: C.Kind -> Bool
isLiftedTypeKind k = C.aeqType k C.liftedTypeKind || case C.tyView k of
  C.TyConApp tc [C.tyView -> C.TyConApp rep [C.tyView -> C.TyConApp lev []]] ->
    occ tc == "TYPE" && occ rep == "BoxedRep" && occ lev == "Lifted"
  _ -> False
 where
  occ = snd . Text.breakOnEnd "." . C.nameOcc

-- | A value of a type: a literal of a primitive type, a dictionary of
-- @KnownNat@, a value of a sized type, a vector (of at most 4096 elements), a
-- tree ('Clash.Sized.RTree' of depth at most 6), a constructor of another data type with fields it can
-- generate, or a function that ignores its argument or returns it. 'Nothing'
-- otherwise.
genArg :: C.TyConMap -> C.Type -> Gen (Maybe C.Term)
genArg tcm = go (3 :: Int)
 where
  go depth ty = case C.tyView ty of
    C.FunTy argTy resTy -> do
      let x = C.mkLocalId argTy (C.mkUnsafeSystemName "x" 1)
      body <- if C.aeqType argTy resTy
                then frequency [(1, pure (Just (C.Var x))), (1, go (depth - 1) resTy)]
                else go (depth - 1) resTy
      pure (C.Lam x <$> body)
    C.TyConApp tc tyArgs
      | Just g <- lookup tc primLits -> Just . C.Literal <$> g
      | Just g <- sized tc tyArgs -> Just <$> g
      | suffix ".KnownNat" tc, [nTy] <- tyArgs, Just n <- natSize nTy ->
          pure (Just (C.Literal (NaturalLiteral n)))
      | suffix ".Vec" tc, [nTy, elTy] <- tyArgs, Just n <- natSize nTy, n <= 4096
      , Just vecTc <- UniqMap.lookup tc tcm, [nilDc, consDc] <- C.tyConDataCons vecTc ->
          fmap (C.mkVec nilDc consDc elTy n) . sequence
            <$> replicateM (fromInteger n) (go (depth - 1) elTy)
      | suffix ".RTree" tc, [dTy, elTy] <- tyArgs, Just d <- natSize dTy, d <= 6
      , Just treeTc <- UniqMap.lookup tc tcm, [lrDc, brDc] <- C.tyConDataCons treeTc ->
          fmap (C.mkRTree lrDc brDc elTy d) . sequence
            <$> replicateM (2 ^ d) (go (depth - 1) elTy)
      | depth > 0
      , Just tyCon <- UniqMap.lookup tc tcm
      , not (C.isNewTypeTc tyCon)
      , dcs@(_ : _) <- C.tyConDataCons tyCon ->
          oneof [ con depth dc tyArgs | dc <- dcs ]
    _ -> pure Nothing

  con depth dc tyArgs
    | null (dcExtTyVars dc)
    , Just fieldTys <- C.dataConInstArgTys dc tyArgs
    = do fields <- forM fieldTys (go (depth - 1))
         pure (C.mkApps (C.Data dc) . (map Right tyArgs ++) . map Left <$> sequence fields)
    | otherwise
    = pure Nothing

  natSize nTy = either (const Nothing) Just (runExcept (C.tyNatSize tcm nTy))
  suffix s tc = s `Text.isSuffixOf` C.nameOcc tc

  primLits :: [(C.TyConName, Gen Literal)]
  primLits =
    [ (primTc C.intPrimTy, IntLiteral <$> int 64)
    , (primTc C.int8PrimTy, Int8Literal <$> int 8)
    , (primTc C.int16PrimTy, Int16Literal <$> int 16)
    , (primTc C.int32PrimTy, Int32Literal <$> int 32)
    , (primTc C.int64PrimTy, Int64Literal <$> int 64)
    , (primTc C.wordPrimTy, WordLiteral <$> word 64)
    , (primTc C.word8PrimTy, Word8Literal <$> word 8)
    , (primTc C.word16PrimTy, Word16Literal <$> word 16)
    , (primTc C.word32PrimTy, Word32Literal <$> word 32)
    , (primTc C.word64PrimTy, Word64Literal <$> word 64)
    , (primTc C.integerPrimTy, IntegerLiteral <$> frequency [(4, int 64), (1, int 200)])
    , (primTc C.naturalPrimTy, NaturalLiteral <$> frequency [(4, word 64), (1, word 200)])
    , (primTc C.charPrimTy, CharLiteral <$> elements ['\0', 'a', 'Z', '\1114111'])
    , (primTc C.doublePrimTy, DoubleLiteral . castDoubleToWord64 <$> double)
    , (primTc C.floatPrimTy, FloatLiteral . castFloatToWord32 . realToFrac <$> double)
    ]

  primTc t = case C.tyView t of
    C.TyConApp tc _ -> tc
    _ -> error "genArg: not a primitive type"

  -- Small numbers, and the edges of the range of @bits@ bits
  int :: Int -> Gen Integer
  int bits = frequency
    [ (4, chooseInteger (-8, 8))
    , (2, chooseInteger (negate (2 ^ (bits - 1)), 2 ^ (bits - 1) - 1))
    , (1, elements [negate (2 ^ (bits - 1)), 2 ^ (bits - 1) - 1]) ]

  word :: Int -> Gen Integer
  word bits = frequency
    [ (4, chooseInteger (0, 16))
    , (2, chooseInteger (0, 2 ^ bits - 1))
    , (1, elements [2 ^ bits - 1]) ]

  double :: Gen Double
  double = frequency
    [ (4, fromInteger <$> chooseInteger (-8, 8))
    , (2, choose (-1e10, 1e10))
    , (1, elements [0 / 0, 1 / 0, -1 / 0, -0]) ]

  -- Values of the sized types are applications of their @fromInteger#@
  sized tc tyArgs = case (C.nameOcc tc, tyArgs) of
    ("Clash.Sized.Internal.BitVector.BitVector", [nTy]) | Just n <- natSize nTy -> Just $ do
      mask <- frequency [(4, pure 0), (1, chooseInteger (0, 2 ^ n - 1))]
      val <- chooseInteger (0, 2 ^ n - 1)
      pure (C.mkApps (prim (U.bvConPrim tc))
        [ Right nTy, Left (nat n), Left (nat mask), Left (C.Literal (IntegerLiteral val)) ])
    ("Clash.Sized.Internal.BitVector.Bit", []) -> Just $ do
      mask <- frequency [(4, pure 0), (1, pure 1)]
      val <- chooseInteger (0, 1)
      pure (C.mkApps (prim (U.bConPrim tc))
        [ Left (C.Literal (WordLiteral mask)), Left (C.Literal (IntegerLiteral val)) ])
    ("Clash.Sized.Internal.Signed.Signed", [nTy]) | Just n <- natSize nTy -> Just $ do
      val <- if n == 0 then pure 0 else chooseInteger (negate (2 ^ (n - 1)), 2 ^ (n - 1) - 1)
      pure (sizedValue (U.signedConPrim tc) nTy n val)
    ("Clash.Sized.Internal.Unsigned.Unsigned", [nTy]) | Just n <- natSize nTy -> Just $ do
      val <- chooseInteger (0, 2 ^ n - 1)
      pure (sizedValue (U.unsignedConPrim tc) nTy n val)
    ("Clash.Sized.Internal.Index.Index", [nTy]) | Just n <- natSize nTy, n > 0 -> Just $ do
      val <- chooseInteger (0, n - 1)
      pure (sizedValue (U.indexConPrim tc) nTy n val)
    _ -> Nothing

  sizedValue conPrim nTy n val =
    C.mkApps (prim conPrim) [Right nTy, Left (nat n), Left (C.Literal (IntegerLiteral val))]
  nat = C.Literal . NaturalLiteral
  prim :: Term 'VoidS -> C.Term
  prim = toClashTerm
