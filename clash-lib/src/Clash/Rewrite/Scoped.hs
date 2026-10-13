{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Writing transformations of scoped Clash Core ("Clash.Core.Scoped.Syntax").
  This module re-exports what a transformation needs from the rewrite
  machinery; read this header before writing one.

  = The transformation type

  A transformation works on a term in /any/ scope @n@, in its context:

  @
  newtype 'Transform' m = 'Transform'
    { 'runTransform' :: forall n. Distinct n => 'TransformContext' n -> Term n -> m (Term n) }
  type 'Rewrite' extra = 'Transform' ('Clash.Rewrite.Types.RewriteMonad' extra)
  @

  The rewrite monad, its state ('Clash.Rewrite.Types.bindings',
  'Clash.Rewrite.Types.tcCache', ...) and the change flag ('changed',
  'setChanged') are in "Clash.Rewrite.Types" and "Clash.Rewrite.Util". The
  unique supply is not needed for local binders: fresh binders come from the
  scope ('Clash.Core.Scoped.Subst.withFreshIdBinder' and friends).

  = Workers

  Transformations are declared in the strategy DSL
  ("Clash.Rewrite.StrategyDSL"), with
  'Clash.Rewrite.StrategyDSL.toScopedTransformation'. The shapes of the
  workers:

  * @onCase 'w@: @w :: Distinct n => TransformContext n -> Term n -> Term n
    -> Type n -> [Alt n] -> m (Term n)@, getting the node and its fields.
    Fields under binders come with their existential scope: for @onLam@,
    @w :: Distinct n => TransformContext n -> Term n -> IdBinder n l -> Term l
    -> m (Term n)@; for @onLet@, @Bind n l -> Term l@, where the right-hand
    side of a 'Clash.Core.Scoped.Syntax.NonRec' is in @n@ and those of a
    'Clash.Core.Scoped.Syntax.Rec' are in @l@. A worker that wants to look under such
    a binder must open it first ('underLam', 'underLet', 'underAlt', or
    'Clash.Core.Scoped.Subst.openIdBinder' and friends when the context isn't
    needed): until then, @l@ is not known to be 'Distinct'.

  * @onAppNode 'w@ (and the other @on*Node@ builders, and
    'Clash.Rewrite.StrategyDSL.scopedAnyConstructor'):
    @w :: Distinct n => TransformContext n -> Term n -> m (Term n)@, getting
    the node only. @onVarNode@ fires at both local ('Var') and global
    ('GlobalVar') variables. There is no @onVar@ for scoped Core.

  * 'Clash.Rewrite.StrategyDSL.scopedPass' takes a function of that same type,
    which traverses the term itself (with "Clash.Rewrite.Scoped.Combinators").

  A worker that doesn't fire returns the node it was given, unchanged. A
  worker that changes the term says so with 'changed'. With
  @-fclash-debug-invariants@, 'applyDebug' reports a change without notice
  (up to alpha-equivalence); free variable and shadowing checks aren't
  needed, as the type checker enforces them.

  = The context

  'TransformContext' @n@ is the scope ('tfScope', to create fresh binders) and
  the path to the subterm ('tfPath'). Inspect it with 'ctxFrames', which lists
  the kinds of its frames ('FrameKind') from the subterm outwards. E.g.
  @reduceConst@'s check that an application is an inner node of a spine:

  @
  case 'ctxFrames' ctx of
    'KAppFun' : _ -> pure e
    _ -> ...
  @

  'mkDerivedName' gives the name of a new binder derived from the closest
  let-binder. Its unique is irrelevant: binders get their names from the
  scope.

  = Utilities

  The utilities on terms (creating binders, inlining, lifting and removing
  let-bindings, ...) are in "Clash.Rewrite.Scoped.Util". Read and create
  global bindings, which are closed terms, with
  "Clash.Rewrite.Scoped.Globals".

  = Evaluation

  'whnfRW' evaluates a term to WHNF, using the let-bindings of the context,
  and continues in the scope extended with the bindings the evaluator
  allocated:

  @
  'whnfRW' True ctx subj $ \\ctx1 subj1 -> case subj1 of
    Literal l -> caseConOn ctx1 (Literal l) (sink ty) (sink1 alts)
    _ -> pure (sink e)  -- doesn't fire: return the original node
  @

  The evaluator is that of scoped Clash Core ("Clash.Core.Scoped.Evaluator").
  The let-bindings of the context are those of its frames.

  = A worked example

  'Clash.Normalize.Scoped.Transformations.Letrec.deadCode', in
  "Clash.Normalize.Scoped.Transformations.Letrec", removes the let-bindings
  that aren't used:

  @
  deadCode :: Transformation
  deadCode = toScopedTransformation \"deadCode\" (onLet 'deadCodeWorker)

  deadCodeWorker
    :: Distinct n
    => TransformContext n -> Term n -> Bind n l -> Term l
    -> NormalizeSession (Term n)
  deadCodeWorker ctx e bind body =
    case removeUnusedBinders (tfScope ctx) bind body of
      Just t -> changed t
      Nothing -> return e
  @

  where 'Clash.Rewrite.Scoped.Util.removeUnusedBinders' moves the body out of
  the scope of unused binders with 'Clash.Core.Scoped.Subst.unsinkIdBinder',
  and opens a recursive let-binding ('Clash.Core.Scoped.Subst.openBind')
  before it looks at its variables.

  The strategies ("Clash.Normalize.Strategy.Spec") refer to the scoped
  transformations, and the strategy compiler
  ("Clash.Rewrite.StrategyDSL.TH", 'Clash.Rewrite.StrategyDSL.TH.compileScopedStrategy')
  compiles them into fused traversals that open the binders they go under.

  Tests: "Test.Clash.Scoped" has test scopes to write the inputs and the
  expected results, and runs scoped transformations in them (@runScopedIn@,
  @runScopedClosed@).

  = Common type errors

  * __@Could not deduce (Distinct l)@__: a value under a binder that wasn't
    opened. Open it, which gives @DExt n l'@.

  * __@Couldn't match type l1 with l@ / @l is a rigid type variable@__: a
    term was used in another scope. Move it into an extension with
    'Clash.Core.Scoped.Syntax.sink', out of the scope of binders with
    'Clash.Core.Scoped.Subst.unsinkTerm' (which checks that they don't
    occur), or into any other scope by substitution.

  * __An existential scope escapes__: a continuation such as that of
    'underLam' returns a value whose type mentions the scope it bound. Return
    something in the outer scope, e.g. rebuild the binder around it.
-}

module Clash.Rewrite.Scoped
  ( module Clash.Rewrite.Scoped.Types
  , module Clash.Rewrite.Scoped.Combinators
  , module Clash.Rewrite.Scoped.Apply
  , whnfRW
  , changed
  , setChanged
  ) where

import Clash.Rewrite.Scoped.Apply
import Clash.Rewrite.Scoped.Combinators
import Clash.Rewrite.Scoped.Types
import Clash.Rewrite.Scoped.Whnf (whnfRW)
import Clash.Rewrite.Util (changed, setChanged)
