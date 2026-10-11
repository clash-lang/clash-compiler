{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Util", against "Clash.Core.Util"
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Core.Scoped.Util (tests) where

import Data.Default (def)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Util as C
import qualified Clash.Core.Var as C
import qualified Clash.Unique as C (Unique)
import Clash.Core.VarEnv (extendInScopeSet)
import Clash.Driver.Types (ClashEnv (..))
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteMonad, RewriteState)
import Clash.Util.Supply (newSupply)

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.FreeVars (teleNameBinders)
import Clash.Core.Scoped.NameMap (NameBinders)
import Clash.Core.Scoped.Subst (unsinkTerm)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Util

import Test.Clash.Rewrite
  ( addTy, brDc, clkPairTy, clockTy, consDc, enableTy, inScopeOf, intId, intTy
  , localId, lrDc, nilDc, numTy, pairTy, resetTy, rtreeTy, runRewriteTest
  , signalTy, systemDomTy, testTyConMap, tyVar, vecTy )
import Test.Clash.Scoped
  ( assertAgreesTm, assertScopedAlphaEq, assertScopedTyAlphaEq, extendSunk
  , sunkVar, withFreeVarsOf )

-- * Let-bindings

x, y, z, a, b, c :: C.Id
x = intId C.User "x" 1
y = intId C.User "y" 2
z = intId C.User "z" 3
a = intId C.User "a" 4
b = intId C.User "b" 5
c = intId C.User "c" 6

-- | A free function @f :: Int -> Int@
f :: C.Term
f = C.Var (localId C.User "f" 100 (C.mkFunTy intTy intTy))

lit :: Integer -> C.Term
lit = C.Literal . C.IntLiteral

app :: C.Id -> C.Term
app v = C.App f (C.Var v)

-- | Bindings that are in dependency order
chainOrdered :: [C.LetBinding]
chainOrdered = [(x, lit 1), (y, app x), (z, app y)]

-- | Bindings that are in reverse dependency order
chainReversed :: [C.LetBinding]
chainReversed = reverse chainOrdered

-- | Bindings with a cycle
withCycle :: [C.LetBinding]
withCycle = [(c, app a), (a, app b), (b, app a)]

-- | Like 'C.inverseTopSortLetBindings', on a let-expression
inverseTopSortC :: C.Term -> C.Term
inverseTopSortC = \case
  C.Letrec bs e -> C.Letrec (C.inverseTopSortLetBindings bs) e
  _ -> error "inverseTopSortC: not a let-expression"

-- | Like 'inverseTopSortLetBindings', on a let-expression
inverseTopSort :: Distinct n => Scope n -> Term n -> Term n
inverseTopSort sc = \case
  Let (Rec bs xs) e ->
    inverseTopSortLetBindings sc bs xs e $ \_ bs' xs' e' -> Let (Rec bs' xs') e'
  _ -> error "inverseTopSort: not a recursive let-expression"

-- | Like 'listToLets', on a let-expression
listToLetsTm :: Distinct n => Scope n -> Term n -> Term n
listToLetsTm sc = \case
  Let (Rec bs xs) e -> listToLets sc bs xs e
  _ -> error "listToLetsTm: not a recursive let-expression"

-- | The raw names of the binders of nested let-expressions
letRaws :: Term n -> [Int]
letRaws = \case
  Let (NonRec bndr _) e -> tmNameRaw (tmNameOf bndr) : letRaws e
  Let (Rec bs _) e -> teleRaws bs ++ letRaws e
  _ -> []
 where
  teleRaws :: Tele IdBinder n l -> [Int]
  teleRaws TNil = []
  teleRaws (TCons bndr rest) = tmNameRaw (tmNameOf bndr) : teleRaws rest

letTests :: TestTree
letTests = testGroup "Let-bindings"
  [ testCase "listToLets agrees" $
      sequence_
        [ assertAgreesTm (const (C.listToLets bs (C.Var x))) listToLetsTm (C.Letrec bs (C.Var x))
        | bs <- [chainOrdered, chainReversed] ]

  , testCase "listToLets on a cycle" $
      assertAgreesTm
        (const (C.Let (C.Rec [(a, app b), (b, app a)]) (C.Let (C.NonRec c (app a)) (C.Var c))))
        listToLetsTm
        (C.Letrec withCycle (C.Var c))

  , testCase "inverseTopSortLetBindings agrees" $
      sequence_
        [ assertAgreesTm inverseTopSortC inverseTopSort (C.Letrec bs (C.Var x))
        | bs <- [chainOrdered, chainReversed] ]

  , testCase "bindings in order keep their binders" $
      withFreeVarsOf [C.Letrec chainOrdered (C.Var z)] $ \env ->
        case fromClashTerm env (C.Letrec chainOrdered (C.Var z)) of
          tm@(Let (Rec _ _) _) ->
            letRaws (listToLetsTm (convScope env) tm) @?= letRaws tm
          _ -> assertFailure "unexpected shape"

  , testCase "reordered bindings get fresh binders" $
      withFreeVarsOf [C.Letrec chainReversed (C.Var z)] $ \env ->
        extendSunk env [] [] $ \env1 -> do
          let tm = sink (fromClashTerm env (C.Letrec chainReversed (C.Var z)))
              res = listToLetsTm (convScope env1) tm
          assertBool "fresh binders"
            (all (> fromIntegral (C.varUniq sunkVar)) (letRaws res))
          assertScopedAlphaEq (C.listToLets chainReversed (C.Var z)) res
  ]

-- * Vectors and trees

-- | Rewrite the coercions that 'C.primCo' makes into the form that 'primCo'
-- makes
coToTyApp :: C.Term -> C.Term
coToTyApp = go
 where
  go = \case
    C.Prim p | C.primName p == "_CO_" ->
      C.TyApp (C.Prim p {C.primType = C.undefinedTy}) (C.primType p)
    C.App e1 e2 -> C.App (go e1) (go e2)
    C.TyApp e ty -> C.TyApp (go e) ty
    C.Lam v e -> C.Lam v (go e)
    C.TyLam v e -> C.TyLam v (go e)
    C.Let (C.NonRec v rhs) e -> C.Let (C.NonRec v (go rhs)) (go e)
    C.Let (C.Rec bs) e -> C.Let (C.Rec [(v, go rhs) | (v, rhs) <- bs]) (go e)
    C.Case e ty alts -> C.Case (go e) ty [(p, go alt) | (p, alt) <- alts]
    C.Cast e t1 t2 -> C.Cast (go e) t1 t2
    C.Tick t e -> C.Tick t (go e)
    e -> e

-- | A type variable @a@, and variables of type @a@
aTv :: C.TyVar
aTv = tyVar C.User "a" 10

aTy :: C.Type
aTy = C.VarTy aTv

el1, el2, el3, el4 :: C.Term
el1 = elVar 11
el2 = elVar 12
el3 = elVar 13
el4 = elVar 14

elVar :: C.Unique -> C.Term
elVar u = C.Var (localId C.User "e" u aTy)

-- | The variables and bindings that 'C.extractElems' gives
unzipElems :: NE.NonEmpty (C.Term, NE.NonEmpty C.LetBinding) -> (NE.NonEmpty C.Term, NE.NonEmpty C.LetBinding)
unzipElems es = (NE.map fst es, sconcat (NE.map snd es))

-- | A vector @v :: Vec 3 a@ and a tree @t :: RTree 2 a@
vecVar, treeVar :: C.Term
vecVar = C.Var (localId C.User "v" 20 (vecTy (numTy 3) aTy))
treeVar = C.Var (localId C.User "t" 21 (rtreeTy (numTy 2) aTy))

-- | Assert that a scoped term, built from the conversions of the given Clash
-- terms (in the scope of their free variables) and of 'aTy', is
-- alpha-equivalent to a Clash term with the coercions of 'C.primCo'
assertBuildsLike
  :: C.Term
  -> [C.Term]
  -> (forall n. Distinct n => Scope n -> Type n -> [Term n] -> Term n)
  -> Assertion
assertBuildsLike expected inputs build =
  withFreeVarsOf (C.Var (localId C.User "dummy" 99 aTy) : inputs) $ \env ->
    assertScopedAlphaEq (coToTyApp expected)
      (build (convScope env) (fromClashType env aTy) (map (fromClashTerm env) inputs))

vecTests :: TestTree
vecTests = testGroup "Vectors and trees"
  [ testCase "mkVec agrees" $ do
      assertBuildsLike (C.mkVec nilDc consDc aTy 2 [el1, el2]) [el1, el2] $ \sc ty es ->
        mkVec sc nilDc consDc ty 2 es
      assertBuildsLike (C.mkVec nilDc consDc aTy 0 []) [] $ \sc ty es ->
        mkVec sc nilDc consDc ty 0 es

  , testCase "appendToVec agrees" $
      assertBuildsLike (C.appendToVec consDc aTy vecVar 5 [el1, el2]) [vecVar, el1, el2] $
        \sc ty es -> case es of
          [v, e1, e2] -> appendToVec sc consDc ty v 5 [e1, e2]
          _ -> error "unexpected inputs"

  , testCase "mkRTree agrees" $
      assertBuildsLike (C.mkRTree lrDc brDc aTy 2 [el1, el2, el3, el4]) [el1, el2, el3, el4] $
        \sc ty es -> mkRTree sc lrDc brDc ty 2 es

  , testCase "extractElems agrees" $ do
      supply <- newSupply
      let is0 = extendInScopeSet (inScopeOf [C.varToId vecVar]) aTv
          (vars, binds) = unzipElems (snd (C.extractElems supply is0 consDc aTy 'V' 3 vecVar))
          expected = C.Letrec (NE.init binds) (C.mkVec nilDc consDc aTy 3 (NE.toList vars))
      assertBuildsLike expected [vecVar] $ \sc ty es -> case es of
        [v] -> extractElems sc consDc ty 'V' 3 3 v $ \sc1 bs rhss vs _ ->
          Let (Rec bs rhss) (mkVec sc1 nilDc consDc (sink ty) 3 vs)
        _ -> error "unexpected inputs"

  , testCase "extractElems of fewer elements agrees" $ do
      supply <- newSupply
      let is0 = extendInScopeSet (inScopeOf [C.varToId vecVar]) aTv
          (vars, binds) = unzipElems (snd (C.extractElems supply is0 consDc aTy 'V' 3 vecVar))
      -- The head, as 'Clash.Normalize.PrimitiveReductions.reduceHead' makes it
      assertBuildsLike (C.Letrec [NE.head binds] (NE.head vars)) [vecVar] $ \sc ty es -> case es of
        [v] -> extractElems sc consDc ty 'V' 3 1 v $ \_ bs rhss vs _ ->
          case vs of
            v1:_ -> Let (Rec bs rhss) v1
            [] -> error "no elements"
        _ -> error "unexpected inputs"
      -- The tail, as the right-hand side of the binder of the tail
      assertBuildsLike (snd (binds NE.!! 1)) [vecVar] $ \sc ty es -> case es of
        [v] -> extractElems sc consDc ty 'V' 3 1 v $ \_ bs _ _ tl ->
          case unsinkTerm (teleBinders bs) tl of
            Just tl' -> tl'
            Nothing -> error "the tail refers to a binder"
        _ -> error "unexpected inputs"

  , testCase "extractElems gives fresh binders" $
      withFreeVarsOf [C.Var (localId C.User "dummy" 99 aTy), vecVar] $ \env ->
        extendSunk env [] [] $ \env1 ->
          extractElems (convScope env1) consDc (sink (fromClashType env aTy)) 'V' 3 3
            (sink (fromClashTerm env vecVar)) $ \_ bs rhss _ _ -> do
              assertBool "fresh binders"
                (all (> fromIntegral (C.varUniq sunkVar)) (teleRawsOf bs))
              assertBool "fresh pattern binders"
                (all (> fromIntegral (C.varUniq sunkVar)) (concatMap caseBinderRaws rhss))

  , testCase "extractTElems agrees" $ do
      supply <- newSupply
      let is0 = extendInScopeSet (inScopeOf [C.varToId treeVar]) aTv
          (vars, binds) = snd (C.extractTElems supply is0 lrDc brDc aTy 'T' 2 treeVar)
          expected = C.Letrec binds (C.mkRTree lrDc brDc aTy 2 vars)
      assertBuildsLike expected [treeVar] $ \sc ty es -> case es of
        [t] -> extractTElems sc lrDc brDc ty 'T' 2 t $ \sc1 bs rhss vs ->
          Let (Rec bs rhss) (mkRTree sc1 lrDc brDc (sink ty) 2 vs)
        _ -> error "unexpected inputs"
  ]
 where
  teleBinders :: Tele IdBinder n l -> NameBinders n l
  teleBinders = teleNameBinders

  teleRawsOf :: Tele IdBinder n l -> [Int]
  teleRawsOf TNil = []
  teleRawsOf (TCons bndr rest) = tmNameRaw (tmNameOf bndr) : teleRawsOf rest

  caseBinderRaws :: Term n -> [Int]
  caseBinderRaws = \case
    Case _ _ [Alt (DataPat _ tvs ids) _] -> tyRaws tvs ++ teleRawsOf ids
    _ -> []

  tyRaws :: Tele TyBinder n l -> [Int]
  tyRaws TNil = []
  tyRaws (TCons bndr rest) = tyNameRaw (tyNameOf bndr) : tyRaws rest

-- * Data constructors

-- | Run an action in the rewrite monad, with 'testTyConMap'
runTest :: RewriteMonad NormalizeState a -> IO a
runTest m = (\(a', _, _) -> a') <$> runRewriteTest env (def :: RewriteState NormalizeState) m
 where
  env = def {_clashEnv = (_clashEnv def) {envTyConMap = testTyConMap}}

-- | Assert that 'mkSelectorCase' agrees with 'C.mkSelectorCase'
assertSelectorAgrees :: C.Term -> Int -> Int -> Assertion
assertSelectorAgrees subj dcI fieldI = do
  expected <- runTest $
    C.mkSelectorCase "test" (extendInScopeSet (inScopeOf [C.varToId subj]) aTv) testTyConMap subj dcI fieldI
  assertAgreesTm (const expected)
    (\sc tm -> mkSelectorCase "test" sc testTyConMap tm dcI fieldI)
    subj

dataConTests :: TestTree
dataConTests = testGroup "Data constructors"
  [ testCase "mkSelectorCase agrees on a product" $ do
      let p = C.Var (localId C.User "p" 30 pairTy)
      assertSelectorAgrees p 1 0
      assertSelectorAgrees p 1 1

    -- Selecting the tail of a vector solves the equation @3 ~ m + 1@ for the
    -- existentially quantified @m@
  , testCase "mkSelectorCase agrees on Cons" $ do
      assertSelectorAgrees vecVar 2 1
      assertSelectorAgrees vecVar 2 2

  , testCase "mkSelectorCase gives fresh binders" $
      withFreeVarsOf [vecVar] $ \env ->
        extendSunk env [] [] $ \env1 ->
          case mkSelectorCase "test" (convScope env1) testTyConMap (sink (fromClashTerm env vecVar)) 2 2 of
            Case _ _ [Alt (DataPat _ (TCons m TNil) ids) _] ->
              assertBool "fresh binders"
                (all (> fromIntegral (C.varUniq sunkVar)) (tyNameRaw (tyNameOf m) : idRaws ids))
            _ -> assertFailure "unexpected shape"

  , testCase "substArgTys" $ do
      withFreeVarsOf [] $ \env -> do
        case substArgTys (convScope env) consDc [fromClashType env (numTy 3), fromClashType env intTy] of
          Nothing -> pure ()
          Just _ -> assertFailure "the type of a field of Cons mentions an existential"
        case substArgTys (convScope env) lrDc [fromClashType env (numTy 0), fromClashType env intTy] of
          Just [_, ty] -> assertScopedTyAlphaEq intTy ty
          _ -> assertFailure "unexpected result"
  ]
 where
  idRaws :: Tele IdBinder n l -> [Int]
  idRaws TNil = []
  idRaws (TCons bndr rest) = tmNameRaw (tmNameOf bndr) : idRaws rest

-- * Types

-- | Types to compare splitting on
exampleTypes :: [C.Type]
exampleTypes =
  [ intTy, pairTy, clockTy, resetTy, enableTy, signalTy systemDomTy intTy, clkPairTy
  , vecTy (numTy 2) clockTy, vecTy (numTy 2) intTy, aTy, numTy 3
  , addTy (numTy 2) (numTy 1), C.LitTy (C.SymTy "x") ]

typeTests :: TestTree
typeTests = testGroup "Types"
  [ testCase "splitShouldSplit" $
      withConvEnv [aTv] [] $ \env -> do
        let res = splitShouldSplit testTyConMap (convScope env) (map (fromClashType env) exampleTypes)
            expected = C.splitShouldSplit testTyConMap exampleTypes
        length res @?= length expected
        sequence_ (zipWith assertScopedTyAlphaEq expected res)
  , testCase "shouldSplit agrees on a product" $
      assertShouldSplitAgrees clkPairTy
  , testCase "shouldSplit agrees on a vector" $
      assertShouldSplitAgrees (vecTy (numTy 2) clockTy)
  ]

-- | Assert that 'shouldSplit' agrees with 'C.shouldSplit' on a type that
-- should be split: the types of the fields, constructing a value out of
-- variables, and the projections of a variable
assertShouldSplitAgrees :: C.Type -> Assertion
assertShouldSplitAgrees ty = case C.shouldSplit testTyConMap ty of
  Nothing -> assertFailure "Clash doesn't split the type"
  Just (con, C.Projections projections, tys) -> do
    let fields = [C.Var (localId C.User "f" u t) | (u, t) <- zip [40..] tys]
        subj = C.Var (localId C.User "s" 50 ty)
    projected <- runTest (projections (extendInScopeSet (inScopeOf [C.varToId subj]) aTv) subj)
    withFreeVarsOf (subj : fields) $ \env -> do
      let sc = convScope env
      case shouldSplit testTyConMap sc (fromClashType env ty) of
        Nothing -> assertFailure "not split"
        Just info -> do
          length (splitArgTys info) @?= length tys
          sequence_ (zipWith assertScopedTyAlphaEq tys (splitArgTys info))
          assertScopedAlphaEq (coToTyApp (con fields))
            (splitConstruct info sc (map (fromClashTerm env) fields))
          let projs = splitProject info sc (fromClashTerm env subj)
          length projs @?= length projected
          sequence_ (zipWith assertScopedAlphaEq projected projs)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Util"
  [ letTests
  , vecTests
  , dataConTests
  , typeTests
  ]
