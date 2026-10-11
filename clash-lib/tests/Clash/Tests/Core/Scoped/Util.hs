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

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Util as C
import qualified Clash.Core.Var as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Util

import Test.Clash.Rewrite (intId, intTy, localId)
import Test.Clash.Scoped
  (assertAgreesTm, assertScopedAlphaEq, extendSunk, sunkVar, withFreeVarsOf)

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

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Util"
  [ letTests
  ]
