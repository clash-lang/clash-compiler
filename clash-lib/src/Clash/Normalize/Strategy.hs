{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Transformation process for normalization
-}

module Clash.Normalize.Strategy where

import Clash.Normalize.Transformations
import Clash.Normalize.Types
import Clash.Rewrite.Combinators
import Clash.Rewrite.Types
import Clash.Rewrite.Util

-- | Normalisation transformation
normalization :: NormRewrite
normalization = rmDeadcode >-> constantPropgation >-> etaTL >-> rmUnusedExpr >-!-> anf >-!-> rmDeadcode >->
                bindConst >-> letTL >-> evalConst >-!-> cse >-!-> cleanup >-> recLetRec
  where
    etaTL      = apply "etaTL" etaExpansionTL !-> innerMost (apply "applicationPropagation" appProp)
    anf        = topdownR (apply "nonRepANF" nonRepANF) >-> apply "ANF" makeANF
    letTL      = topdownSucR (apply "topLet" topLet)
    recLetRec  = apply "recToLetRec" recToLetRec
    rmUnusedExpr = bottomupR (apply "removeUnusedExpr" removeUnusedExpr)
    rmDeadcode = bottomupR (apply "deadcode" deadCode)
    bindConst  = topdownR (apply "bindConstantVar" bindConstantVar)
    evalConst  = topdownR (apply "evalConst" reduceConst)
    cse        = topdownR (apply "CSE" simpleCSE)
    cleanup    = topdownSucR (apply "inlineCleanup" inlineCleanup) !->
                 innerMost (applyMany [("caseCon"        , caseCon)
                                      ,("bindConstantVar", bindConstantVar)
                                      ,("letFlat"        , flattenLet)])
                 >-> rmDeadcode >-> letTL


constantPropgation :: NormRewrite
constantPropgation = propagate >-> repeatR inlineAndPropagate >->
                     caseFlattening >-> dec >-> spec >-> dec >->
                     conSpec
  where
    propagate          = innerMost (applyMany transPropagate)
    inlineAndPropagate = (topdownR (applyMany transInlineSafe) >-> inlineNR)
                         !-> propagate
    spec               = bottomupR (applyMany specTransformations)
    caseFlattening     = repeatR (topdownR (apply "caseFlat" caseFlat))
    dec                = repeatR (topdownR (apply "DEC" disjointExpressionConsolidation))
    conSpec            = bottomupR (apply "constantSpec" constantSpec)

    transPropagate :: [(String,NormRewrite)]
    transPropagate =
      [ ("applicationPropagation", appProp        )
      , ("bindConstantVar"       , bindConstantVar)
      , ("caseLet"               , caseLet        )
      , ("caseCase"              , caseCase       )
      , ("caseCon"               , caseCon        )
      ]

    -- These transformations can safely be applied in a top-down traversal as
    -- they themselves check whether the to-be-inlined binder is recursive or not.
    transInlineSafe :: [(String,NormRewrite)]
    transInlineSafe =
       [ ("inlineWorkFree"  , inlineWorkFree)
       , ("inlineSmall"     , inlineSmall)
       , ("bindOrLiftNonRep", inlineOrLiftNonRep) -- See: [Note] bindNonRep before liftNonRep
                                                  -- See: [Note] bottom-up traversal for liftNonRep
       , ("reduceNonRepPrim", reduceNonRepPrim)


       , ("caseCast"        , caseCast)
       , ("letCast"         , letCast)
       , ("splitCastWork"   , splitCastWork)
       , ("argCastSpec"     , argCastSpec)
       , ("inlineCast"      , inlineCast)
       , ("eliminateCastCast",eliminateCastCast)
       ]

    -- InlineNonRep cannot be applied in a top-down traversal, as the non-representable
    -- binder might be recursive. The idea is, is that if the recursive
    -- non-representable binder is inlined once, we can get rid of the recursive
    -- aspect using the case-of-known-constructor
    inlineNR :: NormRewrite
    inlineNR = bottomupR (apply "inlineNonRep" inlineNonRep)

    specTransformations :: [(String,NormRewrite)]
    specTransformations =
      [ ("typeSpec"    , typeSpec)
      , ("nonRepSpec"  , nonRepSpec)
      ]

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

Because `f` has already been specialised on the alpha-equivalent-to-itself `not`
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
topdownSucR r = r >-! (allR True (topdownSucR r))

innerMost :: Rewrite extra -> Rewrite extra
innerMost r = bottomupR (r !-> innerMost r)

applyMany :: [(String,Rewrite extra)] -> Rewrite extra
applyMany = foldr1 (>->) . map (uncurry apply)
