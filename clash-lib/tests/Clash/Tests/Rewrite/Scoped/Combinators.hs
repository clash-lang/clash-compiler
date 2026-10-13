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

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped

import Test.Clash.Scoped
  ( TestScope (..), assertTermAlphaEq, assertTermIs, closedTerm, emptyTestScope
  , runRewriteClosed, sunkD, testPrim, withClashing, withTerm)

-- | A transformation that changes nothing
idT :: Applicative m => Transform m
idT = Transform (\_ e -> pure e)

-- | Run a transformation in the root context of a closed term
runClosed :: Transform m -> Term 'VoidS -> m (Term 'VoidS)
runClosed t = runTransform t (rootContext emptyScope)

-- | 'topdownR' for any monad: 'topdownR' itself is specific to the rewrite
-- monad, because it repeats the transformation while it signals change
topdownAny :: Monad m => Transform m -> Transform m
topdownAny r = go where go = r >-> allR go

-- | Records the kind of the innermost frame of the context of every subterm
-- it is offered
recordFrames :: Transform (Writer [[FrameKind]])
recordFrames = Transform $ \ctx e -> do
  tell [take 1 (ctxFrames ctx)]
  pure e

-- | Records the literals it is offered, in order
recordLits :: Transform (Writer [Integer])
recordLits = Transform $ \_ e -> case e of
  Literal (IntLiteral i) -> tell [i] >> pure e
  _ -> pure e

-- | Turns odd integer literals into even ones, signalling a change
incrementOdd :: Rewrite ()
incrementOdd = Transform $ \_ -> \case
  Literal (IntLiteral i) | odd i -> changed (Literal (IntLiteral (i + 1)))
  e -> pure e

-- | @case l of { l -> e; ... }@ becomes @e@
caseLit :: Rewrite ()
caseLit = Transform $ \_ -> \case
  Case (Literal l) _ alts
    | (e : _) <- [e | Alt (LitPat l') e <- alts, l == l'] -> changed e
  e -> pure e

-- | @let x = e in x@ becomes @e@
inlineTrivialLet :: Rewrite ()
inlineTrivialLet = Transform $ \_ -> \case
  Let (NonRec b x) (Var _ _ n) | tmNameRaw n == tmNameRaw (tmNameOf b) -> changed x
  e -> pure e

-- | @\\x -> let x = f x in x@, where the right-hand side refers to the
-- lambda's @x@: the let's binder is not in scope of its own right-hand side.
shadowingNonRec :: Term 'VoidS
shadowingNonRec = closedTerm "\\(x_0 :: Int) -> let { !(x_0 :: Int) = (f_G2 :: Int -> Int) x_0 } in x_0"

-- | @f 1 (f 2 3) 4@
lits :: Term 'VoidS
lits = closedTerm "(f_G2 :: Int) 1 (f_G2 2 3) 4"

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Scoped.Combinators"
  [ testCase "allR keeps a NonRec let non-recursive" $
      assertTermAlphaEq shadowingNonRec
        (runIdentity (runClosed (allR (allR (idT :: Transform Identity))) shadowingNonRec))

  , testCase "allR scopes a NonRec binder over the body only" $ do
      -- The right-hand side gets a LetBinding frame, the body a LetBody frame
      let tm = closedTerm "let { !(x_0 :: Int) = 1 } in x_0"
      snd (runWriter (runClosed (allR recordFrames) tm))
        @?= [[KLetBinding (C.mkUnsafeName C.User "x" 0)], [KLetBody]]

  , testCase "allR gives the frames of every constructor" $ do
      -- @\\y -> case f y of { 1 -> y; _ -> y }@
      let tm = closedTerm "\\(y :: Int) -> (case (f_G2 :: Int -> Int) y of { 1 -> y; _ -> y }) :: Int"
      snd (runWriter (runClosed (topdownAny recordFrames) tm)) @?=
        [ [], [KLamBody], [KCaseScrut], [KAppFun], [KAppArg Nothing]
        , [KCaseAlt], [KCaseAlt] ]

  , testCase "allR opens binders that clash with the scope" $
      -- The lambda is built in the empty scope, so its binder clashes with
      -- 'sunkD', whose raw name is larger than its name
      withTerm "\\(y :: Int) -> y" $ \ts0 lam0 -> withClashing ts0 [sunkD] lam0 $ \ts lam -> do
        let probe = Transform $ \ctx e -> do
              case tfPath ctx of
                PUnder (LamBody b) _ -> tell [tmNameRaw (tmNameOf b)]
                _ -> pure ()
              pure e
            (res, names) = runWriter (runTransform (allR probe) (rootContext (testScope ts)) lam)
        case names of
          [n] -> assertBool "renamed" (n > 1000000)
          _ -> assertFailure ("unexpected binders: " <> show names)
        assertTermAlphaEq lam res

  , testCase "the argument's context comes from the rewritten function" $ do
      -- The function becomes a primitive; the argument then sees the AppArg
      -- frame of that primitive, applied to no arguments (like
      -- 'Clash.Core.Scoped.Term.primArg' of the function)
      let tm = closedTerm "(f_G2 :: Int -> Int) 3"
          toPrim = Transform $ \ctx e -> case (ctxFrames ctx, e) of
            (KAppFun : _, GlobalVar _) -> pure (Prim (testPrim "somePrim" "Int -> Int"))
            (KAppArg a : _, _) -> tell [a] >> pure e
            _ -> pure e
      snd (runWriter (runClosed (allR toPrim) tm)) @?= [Just ("somePrim", 0, 0)]

  , testCase "topdown visits parents before children" $
      snd (runWriter (runClosed (topdownAny recordLits) lits))
        @?= [1, 2, 3, 4]

  , testCase "bottomupR visits children before parents, left to right" $
      snd (runWriter (runClosed (bottomupR recordLits) lits))
        @?= [1, 2, 3, 4]

  , testCase "repeatR, !-> and >-! signal change like their Clash Core versions" $ do
      let one = closedTerm "1"
          inc = apply "inc" incrementOdd
      (r1, n1, c1) <- runRewriteClosed inc one
      (show r1, n1, c1) @?= (show (closedTerm "2"), 1, True)
      (_, n2, c2) <- runRewriteClosed (repeatR inc) one
      (n2, c2) @?= (1, True)
      -- The second transformation runs after a change, but doesn't fire
      (_, n3, _) <- runRewriteClosed (inc !-> inc) one
      n3 @?= 1
      -- The second transformation doesn't run after a change
      (_, n4, c4) <- runRewriteClosed (inc >-! apply "never" (Transform (\_ _ -> error "ran"))) one
      (n4, c4) @?= (1, True)

  , testCase "topdownFixR reaches the fixpoint of repeatR topdownR" $ do
      -- @f (case (let x = 1 in x) of { 1 -> 2; _ -> 3 })@
      let tm = closedTerm "(f_G2 :: Int -> Int) ((case (let { !(x_0 :: Int) = 1 } in x_0) of { 1 -> 2; _ -> 3 }) :: Int)"
          r = apply "caseLit" caseLit >-> apply "inlineTrivialLet" inlineTrivialLet
      (fixed, _, _) <- runRewriteClosed (topdownFixR r) tm
      (reference, _, _) <- runRewriteClosed (repeatR (topdownR r)) tm
      assertBool "same result" (aeqTm fixed reference)
      assertTermIs emptyTestScope "(f_G2 :: Int -> Int) 2" fixed
  ]
