{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                (C) 2021,      QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Transformation process for normalization
-}

{-# LANGUAGE CPP #-}

module Clash.Normalize.Strategy where

import Clash.Core.Term (Term(..), stripTicks)
import Clash.Normalize.Transformations
import Clash.Normalize.Types
import Clash.Normalize.Util (termFeaturesOf)
import Clash.Rewrite.Combinators
import Clash.Rewrite.Types
import Clash.Rewrite.Util

-- [Note: bottomup traversal evalConst]
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
  xOptim >-> rmDeadcode >->
  cleanup >-> bindSimIO >-> recLetRec >-> splitArgs
  where
    multPrim   = topdownFixR (apply "setupMultiResultPrim" setupMultiResultPrim)
    anf        = topdownFixR (apply "nonRepANF" nonRepANF) >-> apply "ANF" makeANF >-> topdownFixR (apply "caseCon" caseCon)
    letTL      = topdownSucR (apply "topLet" topLet)
    recLetRec  = apply "recToLetRec" recToLetRec
    rmUnusedExpr = bottomupR (apply "removeUnusedExpr" removeUnusedExpr)
    rmDeadcode = bottomupR (apply "deadcode" deadCode)
    bindConst  = whenFeatures tfHasLet $
                 topdownFixR (apply "bindConstantVar" bindConstantVar)
    -- See [Note] bottomup traversal evalConst:
    evalConst  = bottomupR (apply "evalConst" reduceConst)
    cse        = whenFeatures tfHasLet $
                 topdownFixR (apply "CSE" simpleCSE)
    xOptim     = whenFeatures tfHasCase $
                 bottomupR (apply "xOptimize" xOptimize)
    cleanup    = topdownFixR (apply "etaExpandSyn" etaExpandSyn) >->
                 whenFeatures tfHasLet
                   (topdownSucR (apply "inlineCleanup" inlineCleanup) !->
                    innerMost (applyMany [("caseCon"        , caseCon)
                                         ,("bindConstantVar", bindConstantVar)
                                         ,("letFlat"        , flattenLet)]))
                 >-> rmDeadcode >-> letTL
    splitArgs  = topdownFixR (apply "separateArguments" separateArguments) !->
                 bottomupR (apply "caseCon" caseCon)
    bindSimIO  = whenFeatures tfHasLet $
                 topdownFixR (apply "bindSimIO" inlineSimIO)


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
    etaTL              = apply "etaTL" etaExpansionTL !-> topdownFixR (apply "applicationPropagation" appProp)
    -- Use topdownFixR instead of repeatR . topdownR: when a child change
    -- exposes a new redex at the parent, topdownFixR re-applies the
    -- transformation locally (bubble-up) rather than restarting the full
    -- traversal from the global root. See Note [topdownFixR] in
    -- Clash.Rewrite.Combinators.
    --
    -- The outer repeatR for inlineAndPropagate is still needed because
    -- inlineNR is a full traversal whose results can only be processed by
    -- re-running topdownFixR from the (new) root.
    inlineAndPropagate = repeatR (topdownFixR propagateAndInline >-> inlineNR)
    spec               = bottomupR (applyMany specTransformations)
    -- caseFlattening: the outer repeatR is eliminated because topdownFixR
    -- handles all cascading caseFlat opportunities through bubble-up.
    caseFlattening     = whenFeatures tfHasMultiAltCase $
                         topdownFixR (apply "caseFlat" caseFlat)
    -- dec: topdownFixR already runs to a global fixpoint (equivalent to
    -- repeatR (topdownR r)), so the outer repeatR is redundant.
    dec                = whenFeatures tfHasApp $
                         topdownFixR (apply "DEC" disjointExpressionConsolidation)
    conSpec            = bottomupR  ((apply "appPropCS" appProp !->
                                     bottomupR (apply "constantSpec" constantSpec)) >-!
                                     apply "constantSpec" constantSpec)

    -- The hot propagation loop used to try every rewrite on every visited node.
    -- Most of those attempts were impossible based on the root constructor
    -- alone. Dispatching by term shape cuts that failed-attempt volume while
    -- relying on the existing repeatR/topdownFixR fixpoint machinery to pick up
    -- newly exposed redexes after a successful rewrite changes the term shape.
    propagateAndInline :: NormRewrite
    propagateAndInline ctx term =
      case term of
        Var {}   -> appLike ctx term
        App {}   -> appLike ctx term
        TyApp {} -> appLike ctx term
        Case {}  -> caseLike ctx term
        Letrec {} -> letLike ctx term
        Cast {}  -> castLike ctx term
        Tick _ _ -> case stripTicks term of
          Var {}   -> appLike ctx term
          App {}   -> appLike ctx term
          TyApp {} -> appLike ctx term
          _        -> pure term
        _ -> pure term

    appLike :: NormRewrite
    appLike =
      applyMany
        [ ("applicationPropagation", appProp)
        , ("removeUnusedExpr"      , removeUnusedExpr)
        -- These transformations can safely be applied in a top-down traversal
        -- as they themselves check whether the to-be-inlined binder is
        -- recursive or not.
        , ("inlineWorkFree"        , inlineWorkFree)
        , ("inlineSmall"           , inlineSmall)
        , ("reduceNonRepPrim"      , reduceNonRepPrim)
        , ("argCastSpec"           , argCastSpec)
        ]

    caseLike :: NormRewrite
    caseLike =
      applyMany
        [ ("caseLet"              , caseLet)
        , ("caseCase"             , caseCase)
        , ("caseCon"              , caseCon)
        , ("elimExistentials"     , elimExistentials)
        , ("caseElemNonReachable" , caseElemNonReachable)
        , ("removeUnusedExpr"     , removeUnusedExpr)
        ]

    letLike :: NormRewrite
    letLike =
      applyMany
        [ ("bindConstantVar"      , bindConstantVar)
        , ("bindOrLiftNonRep"     , inlineOrLiftNonRep) -- See: [Note] bindNonRep before liftNonRep
                                                        -- See: [Note] bottom-up traversal for liftNonRep
        , ("splitCastWork"        , splitCastWork)
        , ("inlineCast"           , inlineCast)
        ]

    castLike :: NormRewrite
    castLike =
      applyMany
        [ ("caseCast"             , caseCast)
        , ("letCast"              , letCast)
        , ("elimCastCast"         , elimCastCast)
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
    inlineNR :: NormRewrite
    inlineNR =
          bottomupR (apply "deadCode" deadCode)
      >-! apply "inlineNonRep" inlineNonRep

    specTransformations :: [(String,NormRewrite)]
    specTransformations =
      [ ("typeSpec"    , typeSpec)
      , ("nonRepSpec"  , nonRepSpec)
      , ("zeroWidthSpec", zeroWidthSpec)
        -- See Note [zeroWidthSpec enabling transformations]
      ]

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

whenFeatures :: (TermFeatures -> Bool) -> NormRewrite -> NormRewrite
whenFeatures predicate rw ctx term = do
  features <- termFeaturesOf term
  if predicate features
    then rw ctx term
    else pure term
{-# INLINE whenFeatures #-}

applyMany :: [(String,Rewrite extra)] -> Rewrite extra
applyMany = foldr1 (>->) . map (uncurry apply)
{-# INLINE applyMany #-}
