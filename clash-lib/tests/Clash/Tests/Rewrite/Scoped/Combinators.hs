{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Scoped.Combinators" and the context of
  "Clash.Rewrite.Scoped.Types"
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Rewrite.Scoped.Combinators (tests) where

import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.Functor.Identity (Identity, runIdentity)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped

import Test.Clash.Rewrite (globalIntVar, intId, intLit, intTy)
import Test.Clash.Scoped
  (assertScopedAlphaEq, extendSunk, runScopedClosed, somePrim, sunkVar)

-- | A transformation that changes nothing
idT :: Applicative m => ScopedTransform m
idT = ScopedTransform (\_ e -> pure e)

-- | Run a transformation in the root context of a closed term
runClosed :: ScopedTransform m -> Term 'VoidS -> m (Term 'VoidS)
runClosed t = runScopedTransform t (rootContext emptyScope)

-- | 'topdownR' for any monad: 'topdownR' itself is specific to the rewrite
-- monad, because it repeats the transformation while it signals change
topdownAny :: Monad m => ScopedTransform m -> ScopedTransform m
topdownAny r = go where go = r >-> allR go

-- | Records the kind of the innermost frame of the context of every subterm
-- it is offered
recordFrames :: ScopedTransform (Writer [[FrameKind]])
recordFrames = ScopedTransform $ \ctx e -> do
  tell [take 1 (ctxFrames ctx)]
  pure e

-- | Records the literals it is offered, in order
recordLits :: ScopedTransform (Writer [Integer])
recordLits = ScopedTransform $ \_ e -> case e of
  Literal (IntLiteral i) -> tell [i] >> pure e
  _ -> pure e

-- | Turns odd integer literals into even ones, signalling a change
incrementOdd :: ScopedRewrite ()
incrementOdd = ScopedTransform $ \_ -> \case
  Literal (IntLiteral i) | odd i -> changed (Literal (IntLiteral (i + 1)))
  e -> pure e

-- | @case l of { l -> e; ... }@ becomes @e@
caseLit :: ScopedRewrite ()
caseLit = ScopedTransform $ \_ -> \case
  Case (Literal l) _ alts
    | (e : _) <- [e | Alt (LitPat l') e <- alts, l == l'] -> changed e
  e -> pure e

-- | @let x = e in x@ becomes @e@
inlineTrivialLet :: ScopedRewrite ()
inlineTrivialLet = ScopedTransform $ \_ -> \case
  Let (NonRec b x) (Var _ _ n) | tmNameRaw n == tmNameRaw (tmNameOf b) -> changed x
  e -> pure e

x0, y1 :: C.Id
x0 = intId C.User "x" 0
y1 = intId C.User "y" 1

f :: C.Term
f = globalIntVar C.User "f" 2

-- | @\\x -> let x = f x in x@, where the right-hand side refers to the
-- lambda's @x@: the let's binder is not in scope of its own right-hand side.
shadowingNonRec :: C.Term
shadowingNonRec =
  C.Lam x0 (C.Let (C.NonRec x0 (C.App f (C.Var x0))) (C.Var x0))

-- | @f 1 (f 2 3) 4@
lits :: C.Term
lits = C.mkApps f [Left (intLit 1), Left (C.mkApps f [Left (intLit 2), Left (intLit 3)]), Left (intLit 4)]

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Scoped.Combinators"
  [ testCase "allR keeps a NonRec let non-recursive" $ do
      let tm = fromClashTermClosed shadowingNonRec
      assertScopedAlphaEq shadowingNonRec
        (runIdentity (runClosed (allR (allR (idT :: ScopedTransform Identity))) tm))

  , testCase "allR scopes a NonRec binder over the body only" $ do
      -- The right-hand side gets a LetBinding frame, the body a LetBody frame
      let tm = fromClashTermClosed (C.Let (C.NonRec x0 (intLit 1)) (C.Var x0))
      snd (runWriter (runClosed (allR recordFrames) tm))
        @?= [[KLetBinding (C.varName x0)], [KLetBody]]

  , testCase "allR gives the frames of every constructor" $ do
      -- @\\y -> case f y of { 1 -> y; _ -> y }@
      let tm = fromClashTermClosed
            (C.Lam y1 (C.Case (C.App f (C.Var y1)) intTy
              [(C.LitPat (IntLiteral 1), C.Var y1), (C.DefaultPat, C.Var y1)]))
      snd (runWriter (runClosed (topdownAny recordFrames) tm)) @?=
        [ [], [KLamBody], [KCaseScrut], [KAppFun], [KAppArg Nothing]
        , [KCaseAlt], [KCaseAlt] ]

  , testCase "allR opens binders that clash with the scope" $
      -- The lambda is converted in the empty scope, so its binder clashes
      -- with sunkVar, whose unique is larger than its name
      extendSunk emptyConvEnv [] [] $ \env -> do
        let lam = C.Lam y1 (C.Var y1)
            probe = ScopedTransform $ \ctx e -> do
              case tfPath ctx of
                PUnder (LamBody b) _ -> tell [tmNameRaw (tmNameOf b)]
                _ -> pure ()
              pure e
            (res, names) = runWriter
              (runScopedTransform (allR probe) (rootContext (convScope env))
                 (sinkClosed (fromClashTermClosed lam)))
        case names of
          [n] -> assertBool "renamed" (n > fromIntegral (C.varUniq sunkVar))
          _ -> assertFailure ("unexpected binders: " <> show names)
        assertScopedAlphaEq lam res

  , testCase "the argument's context comes from the rewritten function" $ do
      -- The function becomes a primitive; the argument then sees the AppArg
      -- frame of that primitive, applied to no arguments (like
      -- 'Clash.Core.Term.primArg' of the function)
      let tm = fromClashTermClosed (C.App f (intLit 3))
          toPrim = ScopedTransform $ \ctx e -> case (ctxFrames ctx, e) of
            (KAppFun : _, GlobalVar _) -> pure (Prim somePrim)
            (KAppArg a : _, _) -> tell [a] >> pure e
            _ -> pure e
      snd (runWriter (runClosed (allR toPrim) tm)) @?= [Just ("somePrim", 0, 0)]

  , testCase "topdown visits parents before children" $
      snd (runWriter (runClosed (topdownAny recordLits) (fromClashTermClosed lits)))
        @?= [1, 2, 3, 4]

  , testCase "bottomupR visits children before parents, left to right" $
      snd (runWriter (runClosed (bottomupR recordLits) (fromClashTermClosed lits)))
        @?= [1, 2, 3, 4]

  , testCase "repeatR, !-> and >-! signal change like their Clash Core versions" $ do
      let one = fromClashTermClosed (intLit 1)
          inc = apply "inc" incrementOdd
      (r1, n1, c1) <- runScopedClosed inc one
      (show r1, n1, c1) @?= (show (fromClashTermClosed (intLit 2)), 1, True)
      (_, n2, c2) <- runScopedClosed (repeatR inc) one
      (n2, c2) @?= (1, True)
      -- The second transformation runs after a change, but doesn't fire
      (_, n3, _) <- runScopedClosed (inc !-> inc) one
      n3 @?= 1
      -- The second transformation doesn't run after a change
      (_, n4, c4) <- runScopedClosed (inc >-! apply "never" (ScopedTransform (\_ _ -> error "ran"))) one
      (n4, c4) @?= (1, True)

  , testCase "topdownFixR reaches the fixpoint of repeatR topdownR" $ do
      -- @f (case (let x = 1 in x) of { 1 -> 2; _ -> 3 })@
      let tm = fromClashTermClosed
            (C.App f (C.Case (C.Let (C.NonRec x0 (intLit 1)) (C.Var x0)) intTy
              [(C.LitPat (IntLiteral 1), intLit 2), (C.DefaultPat, intLit 3)]))
          r = apply "caseLit" caseLit >-> apply "inlineTrivialLet" inlineTrivialLet
      (fixed, _, _) <- runScopedClosed (topdownFixR r) tm
      (reference, _, _) <- runScopedClosed (repeatR (topdownR r)) tm
      assertBool "same result" (aeqTm fixed reference)
      assertScopedAlphaEq (C.App f (intLit 2)) fixed
  ]
