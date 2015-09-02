-- | Transformation process for normalization
module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Types
import CLaSH.Rewrite.Util

-- | Normalisation transformation
normalization :: NormRewrite
normalization = etaTL >-> constantPropgation >-> anf >-> rmDeadcode >-> bindConst >-> letTL >-> evalConst >-> cse >-> recLetRec
  where
    etaTL      = apply "etaTL" etaExpansionTL
    anf        = topdownR (apply "nonRepANF" nonRepANF) >-> apply "ANF" makeANF
    letTL      = topdownSucR (apply "topLet" topLet)
    recLetRec  = apply "recToLetRec" recToLetRec
    rmDeadcode = topdownR (apply "deadcode" deadCode)
    bindConst  = topdownR (apply "bindConstantVar" bindConstantVar)
    evalConst  = topdownR (apply "evalConst" reduceConst)
    cse        = topdownR (apply "CSE" simpleCSE)

constantPropgation :: NormRewrite
constantPropgation = propagate >-> repeatR inlineAndPropagate >-> lifting >-> spec
  where
    inlineAndPropagate = inlining >-> propagate
    propagateAndInline = propagate >-> inlining
    propagate = innerMost (applyMany transInner)
    inlining  = topdownR (applyMany transBUP !-> propagateAndInline)
    lifting   = topdownR (apply "liftNonRep" liftNonRep) -- See: [Note] Topdown traversal for liftNonRep
    spec      = bottomupR (applyMany specRws)

    transInner :: [(String,NormRewrite)]
    transInner = [ ("applicationPropagation", appProp        )
                 , ("bindConstantVar"       , bindConstantVar)
                 , ("caseLet"               , caseLet        )
                 , ("caseCase"              , caseCase       )
                 , ("caseCon"               , caseCon        )
                 ]

    transBUP :: [(String,NormRewrite)]
    transBUP = [ ("inlineClosed", inlineClosed)
               , ("inlineSmall" , inlineSmall)
               , ("inlineNonRep", inlineNonRep)
               , ("bindNonRep"  , bindNonRep)
               ]

    specRws :: [(String,NormRewrite)]
    specRws = [ ("typeSpec"    , typeSpec)
              , ("constantSpec", constantSpec)
              , ("nonRepSpec"  , nonRepSpec)
              ]

{- [Note] Topdown traversal for liftNonRep
The liftNonRep transformation must be applied in a topDown traversal because
of what CLaSH considers tail calls in its join-point analysis.

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
in CLaSH. Because 'fail' is used in a TC position within 'fail1', and 'fail1' is
only used in a TC position, in CLaSH, we consider 'tail' also only to be used
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
-}

-- | Topdown traversal, stops upon first success
topdownSucR :: (Functor m, Monad m) => Rewrite m -> Rewrite m
topdownSucR r = r >-! (allR True (topdownSucR r))

innerMost :: (Functor m, Monad m) => Rewrite m -> Rewrite m
innerMost r = bottomupR (r !-> innerMost r)

applyMany :: (Functor m, Monad m) => [(String,Rewrite m)] -> Rewrite m
applyMany = foldr1 (>->) . map (uncurry apply)
