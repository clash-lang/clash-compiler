-- | Transformation process for normalization
module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Types
import CLaSH.Rewrite.Util

-- | Normalisation transformation
normalization :: NormRewrite
normalization = etaTL >-> constantPropgation >-> anf >-> rmDeadcode >-> bindConst >-> letTL >-> recLetRec
  where
    etaTL      = apply "etaTL" etaExpansionTL
    anf        = topdownR (apply "nonRepANF" nonRepANF) >-> apply "ANF" makeANF
    letTL      = topdownSucR (apply "topLet" topLet)
    recLetRec  = apply "recToLetRec" recToLetRec
    rmDeadcode = topdownR (apply "deadcode" deadCode)
    bindConst  = topdownR (apply "bindConstantVar" bindConstantVar)

constantPropgation :: NormRewrite
constantPropgation = propagate
  where
    propagate = innerMost (applyMany transInner) >-> inlining
    inlining  = bottomupR (applyMany transBUP) !-> propagate

    transInner :: [(String,NormRewrite)]
    transInner = [ ("inlineClosed"          , inlineClosed   )
                 , ("applicationPropagation", appProp        )
                 , ("bindConstantVar"       , bindConstantVar)
                 , ("bindNonRep"            , bindNonRep     )
                 , ("caseLet"               , caseLet        )
                 , ("caseCase"              , caseCase       )
                 , ("caseCon"               , caseCon        )
                 ]

    transBUP :: [(String,NormRewrite)]
    transBUP = [ ("typeSpec"    , typeSpec)
               , ("constantSpec", constantSpec)
               , ("nonRepSpec"  , nonRepSpec)
               , ("inlineNonRep", inlineNonRep)
               -- , ("inlineHO"    , inlineHO)
               , ("liftNonRep"  , liftNonRep)
               ]

-- | Topdown traversal, stops upon first success
topdownSucR :: (Functor m, Monad m) => Rewrite m -> Rewrite m
topdownSucR r = r >-! (allR True (topdownSucR r))

innerMost :: (Functor m, Monad m) => Rewrite m -> Rewrite m
innerMost r = bottomupR (r !-> innerMost r)

applyMany :: (Functor m, Monad m) => [(String,Rewrite m)] -> Rewrite m
applyMany = foldr1 (>->) . map (uncurry apply)
