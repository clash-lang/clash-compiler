{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                (C) 2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  The normalization strategies, as pure "Clash.Rewrite.StrategyDSL" values.
  "Clash.Normalize.Strategy" compiles 'normalizationSpec' and
  'constantPropagationSpec' into the executable 'Clash.Normalize.Strategy.normalization'
  and 'Clash.Normalize.Strategy.constantPropagation'; "Clash.Normalize"
  compiles 'flattenSpec' into the flattening strategy of @flattenCallTree@.
-}


module Clash.Normalize.Strategy.Spec
  ( normalizationSpec
  , constantPropagationSpec
  , flattenSpec
  ) where


import Clash.Normalize.Transformations
import Clash.Normalize.Types (NormRewrite, NormStep, NormStrat)
import Clash.Rewrite.StrategyDSL

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

-- | Normalisation transformation. The argument is the compiled
-- 'Clash.Normalize.Strategy.constantPropagation' strategy.
normalizationSpec :: NormRewrite -> NormStrat
normalizationSpec constantPropagation =
  rmDeadcode >-> multPrim >-> callStrategy constantPropagation >->
  rmUnusedExpr >-!-> anf >-!-> rmDeadcode >->
  bindConst >-> letTL
  >-> evalConst
  >-!-> cse >-!-> cleanup >->
  elimCaseBigNum >->  -- see [Note] late elimCaseBigNum
  xOptim >-> rmDeadcode >->
  cleanup >-> bindSimIO >-> recLetRec >-> splitArgs
  where
    multPrim   = topdown setupMultiResultPrim
    anf        = topdown nonRepANF >-> pass "ANF" makeANF >-> topdown caseCon
    letTL      = topdownSuc topLet
    recLetRec  = pass "recToLetRec" recToLetRec
    rmUnusedExpr = bottomup removeUnusedExpr
    rmDeadcode = bottomup (named "deadcode" deadCode)
    bindConst  = topdown bindConstantVar
    -- See [Note] bottomup traversal reduceConst:
    evalConst  = bottomup reduceConst
    cse        = topdown simpleCSE
    elimCaseBigNum = topdown elimCaseBigNumInternals
    xOptim     = bottomup xOptimize
    cleanup    = topdown etaExpandSyn >->
                 topdownSuc inlineCleanup !->
                 innerMost (chain [ caseCon
                                  , bindConstantVar
                                  , named "letFlat" flattenLet
                                  ])
                 >-> rmDeadcode >-> letTL
    splitArgs  = topdown separateArguments !->
                 bottomup caseCon
    bindSimIO  = topdown inlineSimIO


constantPropagationSpec :: NormStrat
constantPropagationSpec =
  inlineAndPropagate >->
  caseFlattening >->
  etaTL >->
  dec >->
  spec >->
  dec >->
  conSpec
  where
    etaTL              = pass "etaTL" etaExpansionTL !-> topdown appProp
    -- The outer repeatR is still needed: inlineNR is a full traversal whose
    -- results can only be processed by re-running the top-down chain from the
    -- new root.
    inlineAndPropagate = repeatR (topdownFix transPropagateAndInline >-> inlineNR)
    spec               = bottomup specTransformations
    caseFlattening     = topdownFix caseFlat
    dec                = topdownFix disjointExpressionConsolidation
    conSpec            = bottomup ((one (named "appPropCS" appProp) !->
                                    nested (bottomup constantSpec)) >-!
                                   one constantSpec)

    transPropagateAndInline :: NormStep
    transPropagateAndInline = chain
      [ appProp
      , bindConstantVar
      , caseLet
      , caseCase
      , caseCon
      , elimExistentials
      , caseEliminateNonReachable
      , removeUnusedExpr
      -- These transformations can safely be applied in a top-down traversal as
      -- they themselves check whether the to-be-inlined binder is recursive or not.
      , inlineWorkFree
      , inlineSmall
      , inlineOrLiftNonRep -- See: [Note] bindNonRep before liftNonRep
                           -- See: [Note] bottom-up traversal for liftNonRep
      , reduceNonRepPrim

      , caseCast
      , letCast
      , splitCastWork
      , argCastSpec
      , inlineCast
      , elimCastCast
      ]

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
    inlineNR :: NormStrat
    inlineNR =
          bottomup deadCode
      >-! pass "inlineNonRep" inlineNonRep

    specTransformations :: NormStep
    specTransformations = chain
      [ typeSpec
      , nonRepSpec
      , zeroWidthSpec
        -- See Note [zeroWidthSpec enabling transformations]
      ]

-- | The flattening strategy of 'Clash.Normalize.flattenCallTree'.
flattenSpec :: NormStrat
flattenSpec =
  -- topdownFix reaches a fixpoint for the top-down propagation chain.
  -- Keep flattenLet in the outer fixed-point loop: flattening can expose
  -- fresh propagation redexes for the next top-down pass.
  repeatR (topdownFix (chain [ named "appProp" appProp
                             , bindConstantVar
                             , caseCon
                             ] >->
                       (one reduceConst !-> one (named "deadcode" deadCode)) >->
                       chain [ reduceNonRepPrim
                             , removeUnusedExpr
                             ]) >->
           bottomup flattenLet) !->
  letTL >->
  -- See [Note] relation `collapseRHSNoops` and `inlineCleanup`
  -- Note that we do this as the very last step, after all constant propagation
  -- has been done to avoid #3036.
  topdownSuc collapseRHSNoops >->
  topdownSuc inlineCleanup >->
  bottomup caseCon >-> -- https://github.com/clash-lang/clash-compiler/issues/3159 / #3204
  bottomup flattenLet >-> -- https://github.com/clash-lang/clash-compiler/issues/3185
  bottomup bindConstantVar >-> -- https://github.com/clash-lang/clash-compiler/issues/3041
  letTL
  where
    letTL = topdownSuc topLet

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
