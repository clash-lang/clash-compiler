{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Stub transformations of scoped Clash Core, test terms, and strategy
  fragments for "Clash.Tests.Rewrite.Scoped.StrategyDSL": the scoped
  counterparts of "Clash.Tests.Rewrite.StrategyDSL.Stubs". They live in their
  own module because Template Haskell splices can only evaluate imported
  values.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Tests.Rewrite.Scoped.StrategyDSL.Stubs where

import Language.Haskell.TH (Exp, Name, Q, listE, stringE)

import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import Clash.Unique (Unique)

import Clash.Core.Scoped.Convert (fromClashTermClosed)
import Clash.Core.Scoped.FreeVars (teleNameBinders)
import Clash.Core.Scoped.Subst (unsinkTerm)
import Clash.Core.Scoped.Syntax hiding (Name)
import Clash.Rewrite.Scoped
  (ScopedRewrite, ScopedTransform (..), TransformContext, changed, ctxFrames)
import Clash.Rewrite.StrategyDSL
import Clash.Rewrite.StrategyDSL.TH
  (compileScopedStrategy, compileScopedStrategyReference)
import Clash.Rewrite.Types (RewriteMonad)

import Test.Clash.Scoped (somePrim)

-- * Term construction helpers

someType :: C.Type
someType = C.ConstTy (C.TyCon (C.mkUnsafeSystemName "T" 0))

someId :: Unique -> C.Id
someId unique = C.mkLocalId someType (C.mkUnsafeSystemName "x" unique)

-- | The global @f@, which 'functionToPrim' rewrites to a primitive
fGlobal :: C.Id
fGlobal = C.mkGlobalId someType (C.mkUnsafeSystemName "f" 2)

integer :: Integer -> Term n
integer = Literal . IntLiteral

cInteger :: Integer -> C.Term
cInteger = C.Literal . IntLiteral

-- * Stub transformations

-- | @f 3@ becomes @3@
collapseApplicationWorker
  :: TransformContext n -> Term n -> Term n -> Term n -> RewriteMonad () (Term n)
collapseApplicationWorker _ctx _node _function argument@(Literal IntLiteral{}) =
  changed argument
collapseApplicationWorker _ctx node _function _argument = pure node

collapseApplication :: Transformation
collapseApplication =
  toScopedTransformation "collapseApplication" (onApp 'collapseApplicationWorker)

collapseApplicationRewrite :: ScopedRewrite ()
collapseApplicationRewrite = ScopedTransform $ \_ctx -> \case
  App _function argument@(Literal IntLiteral{}) -> changed argument
  term -> pure term

-- | Like 'collapseApplication', as a node-receiving worker
collapseSpineWorker :: TransformContext n -> Term n -> RewriteMonad () (Term n)
collapseSpineWorker _ctx = \case
  App _function argument@(Literal IntLiteral{}) -> changed argument
  term -> pure term

collapseSpine :: Transformation
collapseSpine =
  toScopedTransformation "collapseSpine" (onAppNode 'collapseSpineWorker)

-- | Increments odd integer literals
incrementOddWorker
  :: TransformContext n -> Term n -> Literal -> RewriteMonad () (Term n)
incrementOddWorker _ctx _node (IntLiteral i) | odd i = changed (integer (i + 1))
incrementOddWorker _ctx node _literal = pure node

incrementOdd :: Transformation
incrementOdd = toScopedTransformation "incrementOdd" (onLiteral 'incrementOddWorker)

-- | Like 'incrementOdd', but constructor-agnostic
incrementOddAnyWorker :: TransformContext n -> Term n -> RewriteMonad () (Term n)
incrementOddAnyWorker _ctx = \case
  Literal (IntLiteral i) | odd i -> changed (integer (i + 1))
  term -> pure term

incrementOddAny :: Transformation
incrementOddAny = scopedAnyConstructor "incrementOddAny" 'incrementOddAnyWorker

-- | Increments an odd integer literal, either as the argument of an
-- application or bare
incrementBothAppWorker
  :: TransformContext n -> Term n -> Term n -> Term n -> RewriteMonad () (Term n)
incrementBothAppWorker _ctx _node function (Literal (IntLiteral i))
  | odd i = changed (App function (integer (i + 1)))
incrementBothAppWorker _ctx node _function _argument = pure node

incrementBothLiteralWorker
  :: TransformContext n -> Term n -> Literal -> RewriteMonad () (Term n)
incrementBothLiteralWorker _ctx _node (IntLiteral i)
  | odd i = changed (integer (i + 1))
incrementBothLiteralWorker _ctx node _literal = pure node

incrementBoth :: Transformation
incrementBoth = toScopedTransformation "incrementBoth"
  (onApp 'incrementBothAppWorker <> onLiteral 'incrementBothLiteralWorker)

incrementBothRewrite :: ScopedRewrite ()
incrementBothRewrite = ScopedTransform $ \_ctx -> \case
  App function (Literal (IntLiteral i)) | odd i -> changed (App function (integer (i + 1)))
  Literal (IntLiteral i) | odd i -> changed (integer (i + 1))
  term -> pure term

-- | @let x = e in x@ becomes @e@
inlineTrivialLetWorker
  :: TransformContext n -> Term n -> Bind n l -> Term l -> RewriteMonad () (Term n)
inlineTrivialLetWorker _ctx node bind body = case (bind, body) of
  (NonRec b boundTerm, Var _ _ v) | tmNameRaw v == tmNameRaw (tmNameOf b) -> changed boundTerm
  (Rec bs@(TCons b TNil) [boundTerm], Var _ _ v)
    | tmNameRaw v == tmNameRaw (tmNameOf b)
    , Just boundTerm' <- unsinkTerm (teleNameBinders bs) boundTerm
    -> changed boundTerm'
  _ -> pure node

inlineTrivialLet :: Transformation
inlineTrivialLet =
  toScopedTransformation "inlineTrivialLet" (onLet 'inlineTrivialLetWorker)

inlineTrivialLetRewrite :: ScopedRewrite ()
inlineTrivialLetRewrite = ScopedTransform $ \ctx -> \case
  node@(Let bind body) -> inlineTrivialLetWorker ctx node bind body
  term -> pure term

-- | Replaces variable references with a string literal spelling out the
-- frames of the context
reifyContextWorker :: TransformContext n -> Term n -> RewriteMonad () (Term n)
reifyContextWorker ctx _node =
  changed (Literal (StringLiteral (show (ctxFrames ctx))))

reifyContext :: Transformation
reifyContext = toScopedTransformation "reifyContext" (onVarNode 'reifyContextWorker)

-- | Rewrites references to 'fGlobal' to a primitive, so the
-- 'Clash.Rewrite.Scoped.Types.AppArg' frame of a sibling argument is computed
-- from an already rewritten function
functionToPrimWorker :: TransformContext n -> Term n -> RewriteMonad () (Term n)
functionToPrimWorker _ctx = \case
  GlobalVar g | globalIdUniq g == C.varUniq fGlobal -> changed (Prim somePrim)
  node -> pure node

functionToPrim :: Transformation
functionToPrim = toScopedTransformation "functionToPrim" (onVarNode 'functionToPrimWorker)

-- * Test terms, as Clash Core

-- | @(let x = f 3 in x) (letrec y = 5 in y)@
nestedTermC :: C.Term
nestedTermC =
  C.App
    (C.Let (C.NonRec x (C.App (C.Var fGlobal) (cInteger 3))) (C.Var x))
    (C.Let (C.Rec [(y, cInteger 5)]) (C.Var y))
 where
  x = someId 0
  y = someId 1

nestedTerm :: Term 'VoidS
nestedTerm = fromClashTermClosed nestedTermC

-- | A term covering every recursive constructor
allConstructorsTermC :: C.Term
allConstructorsTermC =
  C.Lam lamBinder
    (C.TyLam tyVariable
      (C.Tick C.DeDup
        (C.Case
          (C.Cast (C.App (C.Var lamBinder) subject) someType someType)
          someType
          [ (C.DefaultPat, cInteger 3)
          , (C.LitPat (IntLiteral 0), C.TyApp (C.Var lamBinder) someType)
          ])))
 where
  lamBinder = someId 3
  tyVariable = C.mkTyVar someType (C.mkUnsafeSystemName "a" 100)
  subject = C.Let (C.NonRec (someId 5) (cInteger 7)) (C.Var (someId 5))

allConstructorsTerm :: Term 'VoidS
allConstructorsTerm = fromClashTermClosed allConstructorsTermC

simpleApplicationC :: C.Term
simpleApplicationC = C.App (C.Var fGlobal) (cInteger 3)

simpleApplication :: Term 'VoidS
simpleApplication = fromClashTermClosed simpleApplicationC

-- | The function position rewrites to a primitive; the argument's probed
-- context must show the AppArg frame of that primitive
appArgProbeTerm :: Term 'VoidS
appArgProbeTerm = fromClashTermClosed
  (C.Lam (someId 7) (C.App (C.Var fGlobal) (C.Var (someId 7))))

-- * Strategy fragments compared across all traversals

constructorChangingStep :: Step
constructorChangingStep =
  chain [collapseApplication, incrementBoth, inlineTrivialLet]

literalStep :: Step
literalStep = chain [incrementOdd]

contextProbeStep :: Step
contextProbeStep = chain [reifyContext]

appArgProbeStep :: Step
appArgProbeStep = chain [functionToPrim, reifyContext]

noOpStep :: Step
noOpStep = chain [inlineTrivialLet]

anyWorkerStep :: Step
anyWorkerStep = chain [collapseApplication, incrementOddAny]

nodeWorkerStep :: Step
nodeWorkerStep = chain [collapseSpine, incrementBoth]

conditionalStep :: Step
conditionalStep =
  (one collapseApplication !-> one incrementOdd) >-> chain [inlineTrivialLet]

nestedTraversalStep :: Step
nestedTraversalStep =
  (one collapseApplication !-> nested (bottomup incrementOdd))
  >-! one incrementBoth

callStrategyStrat :: Name -> Strat
callStrategyStrat callee = callStrategy callee >-> topdown collapseApplication

-- | Every fused traversal of a 'Step' next to its unfused reference
allTraversals :: Step -> Q Exp
allTraversals step = listE
  [ [| ($(stringE name), $(compileScopedStrategy strat), $(compileScopedStrategyReference strat)) |]
  | (name, strat) <-
      [ ("topdown", topdown step)
      , ("bottomup", bottomup step)
      , ("topdownFix", topdownFix step)
      , ("topdownSuc", topdownSuc step)
      , ("innerMost", innerMost step)
      ]
  ]
