{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Term", against "Clash.Core.Term"
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Core.Scoped.Term (tests) where

import GHC.Types.SrcLoc (noSrcSpan)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term

import Test.Clash.Rewrite
  (assertAlphaEq, intId, intTy, localId, parseToTermQQ, tyVar)
import Test.Clash.Scoped
  (assertAgreesTm, assertScopedAlphaEq, extendSunk, sunkVar, withFreeVarsOf)

-- | A primitive @p :: forall a. a -> a -> a@
primP :: C.Term
primP = C.Prim (C.PrimInfo "p" ty C.WorkNever C.SingleResult C.NoUnfolding)
 where
  a = tyVar C.User "a" 900
  ty = C.ForAllTy a (C.mkFunTy (C.VarTy a) (C.mkFunTy (C.VarTy a) (C.VarTy a)))

-- | @p \@Int (tick x) y@, with ticks in between the applications
appTerm :: C.Term
appTerm =
  C.Tick (C.SrcSpan noSrcSpan)
    (C.mkApps (C.Tick C.DeDup (C.TyApp primP intTy))
      [Left (C.Tick C.NoDeDup (C.Var x)), Left (C.Var y)])
 where
  x = intId C.User "x" 1
  y = intId C.User "y" 2

-- | A term with lambdas and type lambdas at its head
absTerm :: C.Term
absTerm = C.TyLam a (C.Lam x (C.TyLam b (C.Lam y (C.Var x))))
 where
  a = tyVar C.User "a" 1
  b = tyVar C.User "b" 2
  x = localId C.User "x" 3 (C.VarTy a)
  y = localId C.User "y" 4 (C.VarTy b)

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Term"
  [ testCase "collectArgs and mkApps" $
      assertAgreesTm
        (\tm -> uncurry C.mkApps (C.collectArgs tm))
        (\_ tm -> uncurry mkApps (collectArgs tm))
        appTerm

  , testCase "collectArgs agrees" $
      withFreeVarsOf [appTerm] $ \env -> do
        let (fun, args) = collectArgs (fromClashTerm env appTerm)
            (funC, argsC) = C.collectArgs appTerm
        assertScopedAlphaEq funC fun
        length args @?= length argsC
        sequence_
          [ case (a, aC) of
              (Left e, Left eC) -> assertScopedAlphaEq eC e
              (Right t, Right tC) -> assertAlphaEq tC (toClashType t)
              _ -> assertFailure "term versus type argument"
          | (a, aC) <- zip args argsC ]

  , testCase "collectArgsTicks agrees" $
      withFreeVarsOf [appTerm] $ \env -> do
        let (_, _, ticks) = collectArgsTicks (fromClashTerm env appTerm)
            (_, _, ticksC) = C.collectArgsTicks appTerm
        length ticks @?= length ticksC

  , testCase "primArg agrees" $
      withFreeVarsOf [appTerm] $ \env ->
        primArg (fromClashTerm env appTerm) @?= C.primArg appTerm

  , testCase "mkTmApps and mkTyApps agree" $
      assertAgreesTm
        (\tm -> C.mkTmApps (C.mkTyApps tm [intTy]) [tm])
        (\sc tm -> mkTmApps (mkTyApps tm [fromClashTypeClosed sc intTy]) [tm])
        [parseToTermQQ| \(x :: Int) -> x |]

  , testCase "stripTicks agrees" $
      assertAgreesTm C.stripTicks (const stripTicks) appTerm

  , testCase "stripAllTicks agrees" $
      assertAgreesTm C.stripAllTicks (const stripAllTicks) appTerm

  , testCase "collectTicks and mkTicks" $
      assertAgreesTm
        (\tm -> uncurry C.mkTicks (C.collectTicks tm))
        (\_ tm -> uncurry mkTicks (collectTicks tm))
        (C.Tick C.DeDup (C.Tick C.DeDup appTerm))

  , testCase "mkTicks removes duplicates up to alpha-equivalence" $ do
      -- Two @NameMod@ ticks with alpha-equivalent types
      let a = tyVar C.User "a" 1
          b = tyVar C.User "b" 2
          ty1 = C.ForAllTy a (C.VarTy a)
          ty2 = C.ForAllTy b (C.VarTy b)
          tm = C.Tick (C.NameMod C.PrefixName ty1)
                 (C.Tick (C.NameMod C.PrefixName ty2) (C.Var (intId C.User "x" 3)))
      assertAgreesTm
        (\t -> uncurry C.mkTicks (C.collectTicks t))
        (\_ t -> uncurry mkTicks (collectTicks t))
        tm
      withFreeVarsOf [tm] $ \env ->
        length (snd (collectTicks (uncurry mkTicks (collectTicks (fromClashTerm env tm))))) @?= 1

  , testCase "partitionTicks agrees" $
      withFreeVarsOf [appTerm] $ \env -> do
        let (_, _, ticks) = collectArgsTicks (fromClashTerm env appTerm)
            (_, _, ticksC) = C.collectArgsTicks appTerm
            (src, other) = partitionTicks ticks
            (srcC, otherC) = C.partitionTicks ticksC
        (length src, length other) @?= (length srcC, length otherC)

  , testCase "collectBndrs and mkAbstraction" $
      assertAgreesTm id (\_ tm -> collectBndrs tm mkAbsFlipped) absTerm

  , testCase "openBndrs and mkAbstraction" $
      assertAgreesTm id (\sc tm -> openBndrs sc tm (const mkAbsFlipped)) absTerm

  , testCase "collectBndrs collects as many binders as Clash" $
      withFreeVarsOf [absTerm] $ \env ->
        collectBndrs (fromClashTerm env absTerm) (\bs _ -> teleLength bs)
          @?= length (fst (C.collectBndrs absTerm))

    -- The binders of the term clash with the sunk variable
  , testCase "openBndrs renames binders that clash with the scope" $
      withFreeVarsOf [absTerm] $ \env ->
        extendSunk env [] [] $ \env1 ->
          openBndrs (convScope env1) (sink (fromClashTerm env absTerm)) $ \_ bs body -> do
            assertBool "binders are above the sunk variable"
              (all (> fromIntegral (C.varUniq sunkVar)) (teleRaws bs))
            assertScopedAlphaEq absTerm (mkAbstraction body bs)

  , testCase "mkLams and mkTyLams" $ do
      let a = tyVar C.User "a" 1
          x = localId C.User "x" 2 (C.VarTy a)
      assertAgreesTm id
        (\_ tm -> case tm of
           TyLam ab (Lam xb e) -> mkTyLams (mkLams e (TCons xb TNil)) (TCons ab TNil)
           _ -> error "unexpected shape")
        (C.TyLam a (C.Lam x (C.Var x)))

  , testCase "appendTele and splitTele" $
      assertAgreesTm id
        (\_ tm -> collectBndrs tm $ \bs body ->
           splitTele 2 bs $ \pre post -> mkAbstraction body (appendTele pre post))
        absTerm

  , testCase "walkTerm agrees" $
      withFreeVarsOf [appTerm, absTerm] $ \env -> do
        let count :: Term x -> Maybe ()
            count _ = Just ()
        length (walkTerm count (fromClashTerm env appTerm))
          @?= length (C.walkTerm (const (Just ())) appTerm)
        length (walkTerm count (fromClashTerm env absTerm))
          @?= length (C.walkTerm (const (Just ())) absTerm)
  ]
 where
  mkAbsFlipped :: Tele AbsBinder n l -> Term l -> Term n
  mkAbsFlipped bs body = mkAbstraction body bs

  teleRaws :: Tele AbsBinder n l -> [Int]
  teleRaws TNil = []
  teleRaws (TCons (AbsId b) bs) = tmNameRaw (tmNameOf b) : teleRaws bs
  teleRaws (TCons (AbsTy b) bs) = tyNameRaw (tyNameOf b) : teleRaws bs
