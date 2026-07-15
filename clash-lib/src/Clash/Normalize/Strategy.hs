{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                (C) 2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformation process for normalization
-}

{-# LANGUAGE CPP #-}

module Clash.Normalize.Strategy where

import Clash.Core.Term (Term(..))
import Clash.Normalize.Transformations
import Clash.Normalize.Types
import Clash.Rewrite.Combinators
import Clash.Rewrite.Types
import Clash.Rewrite.Util

-- [Note: bottomup traversal reduceConst]
--
-- 2-May-2019: There is a bug in the evaluator where all data constructors are
-- considered lazy, even though their declaration says they have strict fields.
-- This causes some reductions to fail because the term under the constructor is
-- not in WHNF, which is what some of the evaluation rules for certain primitive
-- operations expect. Using a bottom-up traversal works around this bug by
-- ensuring that the values under the constructor are in WHNF.
--
-- Using a bottomup traversal ensures that constants are reduced to NF, even if
-- constructors are lazy, thus ensuring more sensible/smaller generated HDL.

-- | Normalisation transformation
normalization :: NormRewrite
normalization =
  rmDeadcode >-> multPrim >-> constantPropagation >-> rmUnusedExpr >-!-> anf >-!-> rmDeadcode >->
  bindConst >-> letTL
  >-> evalConst
  >-!-> cse >-!-> cleanup >->
  elimCaseBigNum >->  -- see [Note] late elimCaseBigNum
  xOptim >-> rmDeadcode >->
  cleanup >-> bindSimIO >-> recLetRec >-> splitArgs
  where
    -- The shape gates (onApp, onCase, ...) skip transformations on nodes
    -- their entry patterns cannot match; see each transformation's entry
    -- clauses.
    multPrim   = topdownR (onPrim (apply "setupMultiResultPrim" setupMultiResultPrim))
    anf        = topdownR (onApp (apply "nonRepANF" nonRepANF)) >-> apply "ANF" makeANF >-> topdownR (onCase (apply "caseCon" caseCon))
    letTL      = topdownSucR (apply "topLet" topLet)
    recLetRec  = apply "recToLetRec" recToLetRec
    -- removeUnusedExpr fires on Prim- and Data-headed spines and on
    -- single-alternative case expressions
    rmUnusedExpr = bottomupR rmUnusedExprStep
    rmUnusedExprStep ctx e =
      let go = apply "removeUnusedExpr" removeUnusedExpr ctx e
      in case e of
           Prim {} -> go
           App {} -> go
           TyApp {} -> go
           Tick {} -> go
           Case {} -> go
           _ -> pure e
    rmDeadcode = bottomupR (onLet (apply "deadcode" deadCode))
    bindConst  = topdownR (onLet (apply "bindConstantVar" bindConstantVar))
    -- See [Note] bottomup traversal reduceConst:
    evalConst  = bottomupR (onApp (apply "reduceConst" reduceConst))
    cse        = topdownR (onLet (apply "CSE" simpleCSE))
    elimCaseBigNum = topdownR (onCase (apply "elimCaseBigNum" elimCaseBigNumInternals))
    xOptim     = bottomupR (onCase (apply "xOptimize" xOptimize))
    cleanup    = topdownR (onVarSpine (apply "etaExpandSyn" etaExpandSyn)) >->
                 topdownSucR (onLet (apply "inlineCleanup" inlineCleanup)) !->
                 innerMost cleanupStep
                 >-> rmDeadcode >-> letTL
    -- Entry shapes: caseCon: Case; bindConstantVar, letFlat: Let.
    cleanupStep ctx e = case e of
      Case {} -> apply "caseCon" caseCon ctx e
      Let {} -> (apply "bindConstantVar" bindConstantVar >->
                 apply "letFlat" flattenLet) ctx e
      _ -> pure e
    -- separateArguments fires on lambdas and on Var-headed spines
    splitArgs  = topdownR separateArgsStep !->
                 bottomupR (onCase (apply "caseCon" caseCon))
    separateArgsStep ctx e =
      let go = apply "separateArguments" separateArguments ctx e
      in case e of
           Lam {} -> go
           Var {} -> go
           App {} -> go
           TyApp {} -> go
           Tick {} -> go
           _ -> pure e
    bindSimIO  = topdownR (onLet (apply "bindSimIO" inlineSimIO))


constantPropagation :: NormRewrite
constantPropagation =
  inlineAndPropagate >->
  caseFlattening >->
  etaTL >->
  dec >->
  spec >->
  dec >->
  conSpec
  where
    etaTL              = apply "etaTL" etaExpansionTL !-> topdownR (apply "applicationPropagation" appProp)
    -- The outer repeatR is still needed: inlineNR is a full traversal whose
    -- results can only be processed by re-running the top-down bundle from the
    -- new root.
    inlineAndPropagate = repeatR (topdownFixR propInlineStep >-> inlineNR)
    -- Entry shapes: typeSpec: TyApp; nonRepSpec: App; zeroWidthSpec: Lam.
    -- See Note [zeroWidthSpec enabling transformations]
    spec               = bottomupR specStep
    specStep :: NormRewrite
    specStep ctx e = case e of
      TyApp {} -> apply "typeSpec" typeSpec ctx e
      App {} -> apply "nonRepSpec" nonRepSpec ctx e
      Lam {} -> apply "zeroWidthSpec" zeroWidthSpec ctx e
      _ -> pure e
    caseFlattening     = topdownFixR (onCase (apply "caseFlat" caseFlat))
    dec                = topdownFixR (onCase (apply "DEC" disjointExpressionConsolidation))
    conSpec            = bottomupR  ((apply "appPropCS" appProp !->
                                     bottomupR (apply "constantSpec" constantSpec)) >-!
                                     apply "constantSpec" constantSpec)

    -- The propagate-and-inline bundle, dispatched on the node constructor so
    -- only transformations whose entry patterns can match the node are
    -- attempted. The relative order of transformations within each group is
    -- significant. A rewrite that changes a node's constructor is
    -- re-attempted by topdownFixR's repeatR and then dispatched on its new
    -- shape.
    --
    -- Entry shapes: applicationPropagation: App/TyApp; bindConstantVar,
    -- bindOrLiftNonRep, splitCastWork, inlineCast: Let; caseLet, caseCase,
    -- caseCon, elimExistentials, caseEliminateNonReachable: Case;
    -- removeUnusedExpr: Prim/Data-headed spines (Prim/App/TyApp/Tick) and
    -- single-alternative Case;
    -- inlineWorkFree, inlineSmall: Var-headed spines (Var/App/TyApp/Tick);
    -- reduceNonRepPrim, argCastSpec: App; caseCast, letCast, elimCastCast:
    -- Cast.
    propInlineStep :: NormRewrite
    propInlineStep ctx e = case e of
      App {} -> piApp ctx e
      TyApp {} -> piTyApp ctx e
      Case {} -> piCase ctx e
      Let {} -> piLet ctx e
      Var {} -> piVar ctx e
      Cast {} -> piCast ctx e
      Prim {} -> apply "removeUnusedExpr" removeUnusedExpr ctx e
      Tick {} -> piTick ctx e
      _ -> pure e

    piApp =
      apply "applicationPropagation" appProp >->
      apply "removeUnusedExpr" removeUnusedExpr >->
      -- inlineWorkFree and inlineSmall can safely be applied in a top-down
      -- traversal as they themselves check whether the to-be-inlined binder
      -- is recursive or not.
      apply "inlineWorkFree" inlineWorkFree >->
      apply "inlineSmall" inlineSmall >->
      apply "reduceNonRepPrim" reduceNonRepPrim >->
      apply "argCastSpec" argCastSpec

    piTyApp =
      apply "applicationPropagation" appProp >->
      apply "removeUnusedExpr" removeUnusedExpr >->
      apply "inlineWorkFree" inlineWorkFree >->
      apply "inlineSmall" inlineSmall

    piCase =
      apply "caseLet" caseLet >->
      apply "caseCase" caseCase >->
      apply "caseCon" caseCon >->
      apply "elimExistentials" elimExistentials >->
      apply "caseEliminateNonReachable" caseEliminateNonReachable >->
      -- removeUnusedExpr also fires on single-alternative case expressions
      apply "removeUnusedExpr" removeUnusedExpr

    piLet =
      apply "bindConstantVar" bindConstantVar >->
      apply "bindOrLiftNonRep" inlineOrLiftNonRep >-> -- See: [Note] bindNonRep before liftNonRep
                                                      -- See: [Note] bottom-up traversal for liftNonRep
      apply "splitCastWork" splitCastWork >->
      apply "inlineCast" inlineCast

    piVar =
      apply "inlineWorkFree" inlineWorkFree >->
      apply "inlineSmall" inlineSmall

    piCast =
      apply "caseCast" caseCast >->
      apply "letCast" letCast >->
      apply "elimCastCast" elimCastCast

    piTick =
      apply "removeUnusedExpr" removeUnusedExpr >->
      apply "inlineWorkFree" inlineWorkFree >->
      apply "inlineSmall" inlineSmall

    -- InlineNonRep cannot be applied in a top-down traversal, as the non-representable
    -- binder might be recursive. The idea is, is that if the recursive
    -- non-representable binder is inlined once, we can get rid of the recursive
    -- aspect using the case-of-known-constructor
    --
    -- Note that we first do a dead code removal pass, which makes sure that
    -- unused let-bindings get cleaned up. Only if no dead code is removed
    -- 'inlineNonRep' is executed. We do this for two reasons:
    --
    --   1. 'deadCode' is an expensive operation and is therefore left out of
    --      the hot loop 'transPropagateAndInline'.
    --
    --   2. In various situations 'transPropagateAndInline' can do more work
    --      after 'deadCode' was successful. This work in turn might remove a
    --      a construct 'inlineNonRep' would fire on - saving the compiler work.
    --
    inlineNR :: NormRewrite
    inlineNR =
          bottomupR (apply "deadCode" deadCode)
      >-! apply "inlineNonRep" inlineNonRep

{-
[Note] late elimCaseBigNum

elimCaseBigNum is placed fairly late in the pipeline, after any constant folding and after caseCon.
Because Naturals also hide inside SNat/Nat/KnownNats, and those can be bigger then 64bits.
But they're ultimately monomorphic and thus static and will be evaluated.
We just have to make sure that evaluation happens before we do elimCaseBigNum.

For an example of where this can happen see foldableSNat in tests/shouldwork/Issues/T3157_IntegerNaturalInternals.hs
-}

{-
Note [zeroWidthSpec enabling transformations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When zeroWidthSpec fires, it can lead to better results in normalization, but
this is somewhat incidental. The extra transformations which fire are typically
from

  * calls to transformations like caseCon which occur after constantPropagation
    (e.g. caseCon run after ANF conversion).

  * flattening / inlining which happens late in normalization (after regular
    normalization has occurred)

  * normalizing another function due to being marked NOINLINE

If we consider the following:

    data AB = A | B

    ab :: KnownNat n => Index n -> AB -> AB
    ab n A = if n >  0 then A else B
    ab n B = if n == 0 then B else A
    {-# NOINLINE ab #-}

    topEntity = ab @1
    {-# NOINLINE topEntity #-}

The zeroWidthSpec transformation fires on the topEntity, giving a
post-normalization topEntity of

    \(x :: Index 1) ->
      \(y :: AB) ->
        letrec result :: AB = ab' y in result

where

    ab' = ab (fromInteger# 0)

The extra transformations which fire happen later when ab' is normalized.
Removing the NOINLINE from ab gives the same result, but the extra
transformations fire in flattening instead.
-}

{- [Note] bottom-up traversal for liftNonRep
We used to say:

"The liftNonRep transformation must be applied in a topDown traversal because
of what Clash considers tail calls in its join-point analysis."

Consider:

> let fail = \x -> ...
> in  case ... of
>       A -> let fail1 = \y -> case ... of
>                                 X -> fail ...
>                                 Y -> ...
>            in case ... of
>                 P -> fail1 ...
>                 Q -> ...
>       B -> fail ...

under "normal" tail call rules, the local 'fail' functions is not a join-point
because it is used in a let-binding. However, we apply "special" tail call rules
in Clash. Because 'fail' is used in a TC position within 'fail1', and 'fail1' is
only used in a TC position, in Clash, we consider 'tail' also only to be used
in a TC position.

Now image we apply 'liftNonRep' in a bottom up traversal, we will end up with:

> fail1 = \fail y -> case ... of
>   X -> fail ...
>   Y -> ...

> let fail = \x -> ...
> in  case ... of
>       A -> case ... of
>                 P -> fail1 fail ...
>                 Q -> ...
>       B -> fail ...

Suddenly, 'fail' ends up in an argument position, because it occurred as a
_locally_ bound variable within 'fail1'. And because of that 'fail' stops being
a join-point.

However, when we apply 'liftNonRep' in a top down traversal we end up with:

> fail = \x -> ...
>
> fail1 = \y -> case ... of
>   X -> fail ...
>   Y -> ...
>
> let ...
> in  case ... of
>       A -> let
>            in case ... of
>                 P -> fail1 ...
>                 Q -> ...
>       B -> fail ...

and all is well with the world.

UPDATE:
We can now just perform liftNonRep in a bottom-up traversal again, because
liftNonRep no longer checks that if the binding that is lifted is a join-point.
However, for this to work, bindNonRep must always have been exhaustively applied
before liftNonRep. See also: [Note] bindNonRep before liftNonRep.
-}

{- [Note] bindNonRep before liftNonRep
The combination of liftNonRep and nonRepSpec can lead to non-termination in an
unchecked rewrite system (without termination measures in place) on the
following:

> main = f not
> f    = \a x -> (a x) && (f a x)

nonRepSpec will lead to:

> main = f'
> f    = \a x -> (a x) && (f a x)
> f'   = (\a x -> (a x) && (f a x)) not

then lamApp leads to:

> main = f'
> f    = \a x -> (a x) && (f a x)
> f'   = let a = not in (\x -> (a x) && (f a x))

then liftNonRep leads to:

> main = f'
> f    = \a x -> (a x) && (f a x)
> f'   = \x -> (g x) && (f g x)
> g    = not

and nonRepSepc leads to:

> main = f'
> f    = \a x -> (a x) && (f a x)
> f'   = \x -> (g x) && (f'' g x)
> g    = not
> f''  = (\a x -> (a x) && (f a x)) g

This cycle continues indefinitely, as liftNonRep creates a new global variable,
which is never alpha-equivalent to the previous global variable introduced by
liftNonRep.

That is why bindNonRep must always be applied before liftNonRep. When we end up
in the situation after lamApp:

> main = f'
> f    = \a x -> (a x) && (f a x)
> f'   = let a = not in (\x -> (a x) && (f a x))

bindNonRep will now lead to:

> main = f'
> f    = \a x -> (a x) && (f a x)
> f'   = \x -> (not x) && (f not x)

Because `f` has already been specialized on the alpha-equivalent-to-itself `not`
function, liftNonRep leads to:

> main = f'
> f    = \a x -> (a x) && (f a x)
> f'   = \x -> (not x) && (f' x)

And there is no non-terminating rewriting cycle.

That is why bindNonRep must always be exhaustively applied before we apply
liftNonRep.
-}

-- | Topdown traversal, stops upon first success
topdownSucR :: Rewrite extra -> Rewrite extra
topdownSucR r = r >-! (allR (topdownSucR r))
{-# INLINE topdownSucR #-}

innerMost :: Rewrite extra -> Rewrite extra
innerMost = let go r = bottomupR (r !-> innerMost r) in go
{-# INLINE innerMost #-}

applyMany :: [(String,Rewrite extra)] -> Rewrite extra
applyMany = foldr1 (>->) . map (uncurry apply)
{-# INLINE applyMany #-}
