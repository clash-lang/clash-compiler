{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector", on
  fixed inputs.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Scoped.Alpha (aeqTy)
import Clash.Core.Scoped.Builder (TmB, appB, dataB, runClosedType)
import Clash.Core.Scoped.Subst (closeType)
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite (iHashDc, showPprU, snatDc)
import Test.Clash.Scoped.Evaluator (assertSameValue)
import Test.Clash.Scoped.Evaluator.Primitives

-- | A vector of @Integer@ literals
intVec :: [Integer] -> TmB
intVec = vec integerPrimTy . map integerLit

-- | The types of the binders of the patterns in a term, applied to a
-- function. The types live in the scopes of their binders.
patBinderTypes :: forall r n. (forall x. Type x -> r) -> Term n -> [r]
patBinderTypes f = \case
  Lam _ e -> patBinderTypes f e
  TyLam _ e -> patBinderTypes f e
  App e1 e2 -> patBinderTypes f e1 <> patBinderTypes f e2
  TyApp e _ -> patBinderTypes f e
  Let (NonRec _ x) e -> patBinderTypes f x <> patBinderTypes f e
  Let (Rec _ xs) e -> concatMap (patBinderTypes f) xs <> patBinderTypes f e
  Case e _ alts ->
    patBinderTypes f e <> concat [ patTypes p <> patBinderTypes f a | Alt p a <- alts ]
  Cast e _ _ -> patBinderTypes f e
  Tick _ e -> patBinderTypes f e
  _ -> []
 where
  patTypes :: Pat x y -> [r]
  patTypes = \case
    DataPat _ _ ids -> teleTypes ids
    _ -> []
  teleTypes :: Tele IdBinder x y -> [r]
  teleTypes = \case
    TNil -> []
    TCons b bs -> f (idBinderType b) : teleTypes bs

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.Vector"
  [ testCase "maxIndex is the length minus one" $
      -- maxIndex :: forall n a. KnownNat n => Vec n a -> Int
      let primTy =
            forAll "n_9101" natKind $ \n -> forAll "a_9102" typeKind $ \a ->
              knownNatTy n ~> vecTy n a ~> boxedIntTy
      in assertSameValue emptyScope (closed (appB (dataB iHashDc) (intLit 2)))
           (evalSubj (apps (prim "Clash.Sized.Vector.maxIndex" primTy)
              [Right (numTy 3), Right integerPrimTy, Left (natLit 3), Left (intVec [1, 2, 3])]))

  , testCase "rotateLeftS evaluates the number of its SNat with a nested machine" $
      -- rotateLeftS :: forall n a d. KnownNat n => Vec n a -> SNat d -> Vec n a
      let primTy =
            forAll "n_9101" natKind $ \n -> forAll "a_9102" typeKind $ \a ->
              forAll "d_9103" natKind $ \d ->
                knownNatTy n ~> vecTy n a ~> snatTy d ~> vecTy n a
          -- The number is only known after evaluating a let-expression.
          -- Rotating by the length gives the vector itself.
          sn = apps (dataB snatDc)
                 [Right (numTy 3), Left (letNonRec "y_8" naturalPrimTy (natLit 3) id)]
      in assertSameValue emptyScope (closed (intVec [1, 2, 3]))
           (evalSubj (apps (prim "Clash.Sized.Vector.rotateLeftS" primTy)
              [ Right (numTy 3), Right integerPrimTy, Right (numTy 3)
              , Left (natLit 3), Left (intVec [1, 2, 3]), Left sn ]))

  , testCase "dtfold binds the halves of the vector at their vector type" $
      -- dtfold :: forall p k a. KnownNat k => Proxy p -> (a -> p @@ 0)
      --   -> (forall l. SNat l -> p @@ l -> p @@ l -> p @@ (l + 1))
      --   -> Vec (2^k) a -> p @@ k
      --
      -- with every @p \@\@ _@ an @Int@, and @Vec k a@ for @Vec (2^k) a@: the
      -- types of arguments aren't checked.
      let gTy = forAll "l_9107" natKind $ \l ->
                  snatTy l ~> boxedIntTy ~> boxedIntTy ~> boxedIntTy
          primTy =
            forAll "p_9104" typeKind $ \p -> forAll "k_9105" natKind $ \k ->
              forAll "a_9102" typeKind $ \a ->
                knownNatTy k ~> p ~> (a ~> boxedIntTy) ~> gTy ~> vecTy k a ~> boxedIntTy
          -- f and g apply free variables to their arguments
          f = lam "x_12" integerPrimTy $ \x -> appB (var "fV_10") x
          g = tyLam "l_9107" natKind $ \l ->
                lam "s_13" (snatTy l) $ \s -> lam "r1_14" boxedIntTy $ \r1 ->
                  lam "r2_15" boxedIntTy $ \r2 ->
                    apps (var "gV_11") [Right l, Left s, Left r1, Left r2]
          call = apps (prim "Clash.Sized.Vector.dtfold" primTy)
            [ Right boxedIntTy, Right (numTy 1), Right integerPrimTy
            , Left (natLit 1), Left (appB (dataB iHashDc) (intLit 0))
            , Left f, Left g, Left (intVec [1, 2]) ]
      in evalOpen [("fV_10", integerPrimTy ~> boxedIntTy), ("gV_11", gTy)] True call $ \_ r -> do
           let expected = runClosedType (vecTy (numTy 1) integerPrimTy)
               binderTys = patBinderTypes (\t -> maybe False (aeqTy expected) (closeType t)) r
           assertBool ("no patterns in: " <> showPprU r) (not (null binderTys))
           assertBool ("a binder has another type than Vec 1 Integer in: " <> showPprU r)
             (and binderTys)
  ]
