-- | Transformation process for normalization
module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Util

-- | Normalisation transformation
normalization :: NormRewrite
normalization = etaTL >-> constantPropgation >-> anf >-> letTL >-> recLetRec
  where
    etaTL      = apply "etaTL" etaExpansionTL
    anf        = topdownR (apply "nonRepANF" nonRepANF) >-> apply "ANF" makeANF
    letTL      = topdownSucR (apply "topLet" topLet)
    recLetRec  = apply "recToLetRec" recToLetRec

constantPropgation :: NormRewrite
constantPropgation = upDownR   (repeatR $ foldr1 (>->) $ map (uncurry apply) transformationsDown) >->
                     bottomupR (foldr1 (>->) $ map (uncurry apply) transformationsUp)
  where
    transformationsDown :: [(String,NormRewrite)]
    transformationsDown = [ ("inlineClosed"          , inlineClosed   )
                          , ("inlineNonRep"          , inlineNonRep   )
                          , ("applicationPropagation", appProp        )
                          , ("bindConstantVar"       , bindConstantVar)
                          , ("liftNonRep"            , liftNonRep     )
                          , ("caseLet"               , caseLet        )
                          , ("caseCase"              , caseCase       )
                          , ("caseCon"               , caseCon        )
                          , ("deadcode"              , deadCode       )
                          ]

    transformationsUp :: [(String,NormRewrite)]
    transformationsUp = [ ("typeSpec"    , typeSpec)
                        , ("constantSpec", constantSpec)
                        , ("nonRepSpec"  , nonRepSpec)
                        ]
