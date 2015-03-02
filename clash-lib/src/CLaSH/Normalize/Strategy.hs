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
    lifting   = bottomupR (apply "liftNonRep" liftNonRep)
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

-- | Topdown traversal, stops upon first success
topdownSucR :: (Functor m, Monad m) => Rewrite m -> Rewrite m
topdownSucR r = r >-! (allR True (topdownSucR r))

innerMost :: (Functor m, Monad m) => Rewrite m -> Rewrite m
innerMost r = bottomupR (r !-> innerMost r)

applyMany :: (Functor m, Monad m) => [(String,Rewrite m)] -> Rewrite m
applyMany = foldr1 (>->) . map (uncurry apply)
