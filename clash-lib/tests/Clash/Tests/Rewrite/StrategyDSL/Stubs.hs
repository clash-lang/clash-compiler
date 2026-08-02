{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Stub transformations, test terms, and strategy fragments for
  "Clash.Tests.Rewrite.StrategyDSL".
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Rewrite.StrategyDSL.Stubs where

import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (mkUnsafeSystemName)
import Clash.Core.Term
  (Bind (..), IsMultiPrim (..), Pat (..), PrimInfo (..), PrimUnfolding (..),
   Term (..), TickInfo (..), WorkInfo (..))
import Clash.Core.Type (ConstTy (..), Type (..))
import Clash.Core.Var (Id, mkLocalId, mkTyVar)
import Clash.Rewrite.StrategyDSL
import Clash.Rewrite.StrategyDSL.Compile (compile, compileReference)
import Clash.Rewrite.Types (Rewrite, RewriteMonad, TransformContext (..))
import Clash.Rewrite.Util (changed)
import Clash.Unique (Unique)

-- * Term construction helpers

someType :: Type
someType = ConstTy (TyCon (mkUnsafeSystemName "T" 0))

someId :: Unique -> Id
someId unique = mkLocalId someType (mkUnsafeSystemName "x" unique)

somePrim :: PrimInfo
somePrim = PrimInfo
  { primName = "somePrim"
  , primType = someType
  , primWorkInfo = WorkConstant
  , primMultiResult = SingleResult
  , primUnfolding = NoUnfolding
  }

integer :: Integer -> Term
integer = Literal . IntLiteral

-- * Stub transformations
--
-- Each stub is a worker plus its 'TransformSpec'; some also exist as an
-- old-style 'Rewrite' with its own constructor match and catch-all, for
-- hand-written comparisons.

-- | @f 3@ becomes @3@: fires on an application whose argument is an integer
-- literal, changing the node's constructor.
collapseApplicationWorker
  :: TransformContext -> Term -> Term -> Term -> RewriteMonad () Term
collapseApplicationWorker _ctx _node _function argument@(Literal IntLiteral{}) =
  changed argument
collapseApplicationWorker _ctx node _function _argument = pure node

collapseApplication :: TransformSpec ()
collapseApplication =
  transform "collapseApplication" (onApp collapseApplicationWorker)

collapseApplicationRewrite :: Rewrite ()
collapseApplicationRewrite _ctx (App _function argument@(Literal IntLiteral{})) =
  changed argument
collapseApplicationRewrite _ctx term = pure term

-- | Like 'collapseApplication', but as a node-receiving worker registered at
-- 'App' — the spine-transformation style.
collapseSpineWorker :: Rewrite ()
collapseSpineWorker _ctx (App _function argument@(Literal IntLiteral{})) =
  changed argument
collapseSpineWorker _ctx term = pure term

collapseSpine :: TransformSpec ()
collapseSpine = transform "collapseSpine" (onAppNode collapseSpineWorker)

-- | Increments odd integer literals (even results keep repeated traversals
-- terminating).
incrementOddWorker
  :: TransformContext -> Term -> Literal -> RewriteMonad () Term
incrementOddWorker _ctx _node (IntLiteral i) | odd i = changed (integer (i + 1))
incrementOddWorker _ctx node _literal = pure node

incrementOdd :: TransformSpec ()
incrementOdd = transform "incrementOdd" (onLiteral incrementOddWorker)

-- | Like 'incrementOdd', but shape-agnostic: a plain rewrite offered at
-- every node, with its own match.
incrementOddAnyWorker :: Rewrite ()
incrementOddAnyWorker _ctx (Literal (IntLiteral i))
  | odd i = changed (integer (i + 1))
incrementOddAnyWorker _ctx term = pure term

incrementOddAny :: TransformSpec ()
incrementOddAny = anyShape "incrementOddAny" incrementOddAnyWorker

-- | Multi-shape: increments an odd integer literal, either as the argument
-- of an application or bare. Registered for App and Literal, to observe the
-- guarded suffix.
incrementBothAppWorker
  :: TransformContext -> Term -> Term -> Term -> RewriteMonad () Term
incrementBothAppWorker _ctx _node function (Literal (IntLiteral i))
  | odd i = changed (App function (integer (i + 1)))
incrementBothAppWorker _ctx node _function _argument = pure node

incrementBothLiteralWorker
  :: TransformContext -> Term -> Literal -> RewriteMonad () Term
incrementBothLiteralWorker _ctx _node (IntLiteral i)
  | odd i = changed (integer (i + 1))
incrementBothLiteralWorker _ctx node _literal = pure node

incrementBoth :: TransformSpec ()
incrementBoth = transform "incrementBoth"
  (onApp incrementBothAppWorker <> onLiteral incrementBothLiteralWorker)

incrementBothRewrite :: Rewrite ()
incrementBothRewrite _ctx (App function (Literal (IntLiteral i)))
  | odd i = changed (App function (integer (i + 1)))
incrementBothRewrite _ctx (Literal (IntLiteral i))
  | odd i = changed (integer (i + 1))
incrementBothRewrite _ctx term = pure term

-- | Inlines a let binder into its body when the body is exactly that
-- variable: @let x = e in x@ becomes @e@. Fires on 'Let'.
inlineTrivialLetWorker
  :: TransformContext -> Term -> Bind Term -> Term -> RewriteMonad () Term
inlineTrivialLetWorker _ctx node bind body = case (bind, body) of
  (NonRec i boundTerm, Var v) | v == i -> changed boundTerm
  (Rec [(i, boundTerm)], Var v) | v == i -> changed boundTerm
  _ -> pure node

inlineTrivialLet :: TransformSpec ()
inlineTrivialLet = transform "inlineTrivialLet" (onLet inlineTrivialLetWorker)

inlineTrivialLetRewrite :: Rewrite ()
inlineTrivialLetRewrite _ctx node@(Let bind body) = case (bind, body) of
  (NonRec i boundTerm, Var v) | v == i -> changed boundTerm
  (Rec [(i, boundTerm)], Var v) | v == i -> changed boundTerm
  _ -> pure node
inlineTrivialLetRewrite _ctx term = pure term

-- | Replaces variable references with a string literal spelling out the full
-- 'Clash.Core.Term.CoreContext' path, exposing every context push of the
-- descent arms to the equivalence tests.
reifyContextWorker
  :: TransformContext -> Term -> Id -> RewriteMonad () Term
reifyContextWorker ctx _node _i =
  changed (Literal (StringLiteral (show (tfContext ctx))))

reifyContext :: TransformSpec ()
reifyContext = transform "reifyContext" (onVar reifyContextWorker)

-- | Rewrites references to one specific variable to a primitive, so the
-- 'Clash.Core.Term.AppArg' context a sibling argument sees is computed from
-- an already rewritten function.
functionToPrimWorker
  :: TransformContext -> Term -> Id -> RewriteMonad () Term
functionToPrimWorker _ctx node i
  | i == someId 2 = changed (Prim somePrim)
  | otherwise = pure node

functionToPrim :: TransformSpec ()
functionToPrim = transform "functionToPrim" (onVar functionToPrimWorker)

-- * Test terms

-- | @(let x = f 3 in x) (let y = 5 in y)@ — exercises App, Let (both binder
-- and body positions), and literals at several depths.
nestedTerm :: Term
nestedTerm =
  App
    (Let (NonRec x (App (Var f) (integer 3))) (Var x))
    (Let (Rec [(y, integer 5)]) (Var y))
 where
  x = someId 0
  y = someId 1
  f = someId 2

-- | A term covering every recursive constructor: Lam, TyLam, App, TyApp,
-- Let, Case (subject and alternatives), Cast, and Tick.
allConstructorsTerm :: Term
allConstructorsTerm =
  Lam lamBinder
    (TyLam tyVariable
      (Tick DeDup
        (Case
          (Cast (App (Var lamBinder) subject) someType someType)
          someType
          [ (DefaultPat, integer 3)
          , (LitPat (IntLiteral 0), TyApp (Var lamBinder) someType)
          ])))
 where
  lamBinder = someId 3
  tyVariable = mkTyVar someType (mkUnsafeSystemName "a" 100)
  subject = Let (NonRec (someId 5) (integer 7)) (Var (someId 5))

simpleApplication :: Term
simpleApplication = App (Var (someId 6)) (integer 3)

-- | The function position rewrites to a primitive; the argument's probed
-- context must show the 'Clash.Core.Term.AppArg' of that primitive.
appArgProbeTerm :: Term
appArgProbeTerm = App (Var (someId 2)) (Var (someId 7))

-- * Strategy fragments compared across all traversals

shapeChangingStep :: Step ()
shapeChangingStep = chain [collapseApplication, incrementBoth, inlineTrivialLet]

literalStep :: Step ()
literalStep = chain [incrementOdd]

contextProbeStep :: Step ()
contextProbeStep = chain [reifyContext]

appArgProbeStep :: Step ()
appArgProbeStep = chain [functionToPrim, reifyContext]

noOpStep :: Step ()
noOpStep = chain [inlineTrivialLet]

anyShapeStep :: Step ()
anyShapeStep = chain [collapseApplication, incrementOddAny]

nodeEntryStep :: Step ()
nodeEntryStep = chain [collapseSpine, incrementBoth]

-- | A cross-shape conditional between chain segments inside one traversal —
-- the @flatten@ strategy's structure.
conditionalStep :: Step ()
conditionalStep =
  (one collapseApplication !-> one incrementOdd) >-> chain [inlineTrivialLet]

-- | A whole nested traversal embedded as one node action — the @conSpec@
-- strategy's structure.
nestedTraversalStep :: Step ()
nestedTraversalStep =
  (one collapseApplication !-> nested (bottomup incrementOdd))
  >-! one incrementBoth

-- | Every fused traversal of a 'Step' next to its unfused reference:
-- @[(name, fast, reference)]@.
allTraversals :: Step () -> [(String, Rewrite (), Rewrite ())]
allTraversals step =
  [ (name, compile strat, compileReference strat)
  | (name, strat) <-
      [ ("topdown", TopDown step)
      , ("bottomup", BottomUp step)
      , ("topdownFix", TopDownFix step)
      , ("topdownSuc", TopDownSuc step)
      , ("innerMost", InnerMost step)
      ]
  ]
