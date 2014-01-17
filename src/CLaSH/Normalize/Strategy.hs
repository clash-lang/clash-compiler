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

--normalization = representable >-> simplification >-> apply "recToLetrec" recToLetRec

-- | Simple cleanup transformation, currently only inlines \"Wrappers\"
cleanup :: NormRewrite
cleanup = repeatR (apply "inlineTLWrapper" inlineTLWrapper) >->
          (topdownR (apply "inlineWrapper" inlineWrapper) !-> normalization)
--cleanup _ e = return e

-- | Unsure that functions have representable arguments, results, and let-bindings
representable :: NormRewrite
representable = propagation >-> specialisation
  where
    propagation = repeatR ( upDownR  (apply "propagation" appProp) >->
                            repeatBottomup [ ("bindNonRep"   , bindNonRep )
                                           , ("liftNonRep"   , liftNonRep )
                                           , ("caseLet"      , caseLet    )
                                           , ("caseCase"     , caseCase   )
                                           , ("caseCon"      , caseCon    )
                                           ]
                            >->
                            bottomupR (apply "inlineNonRep" inlineNonRep)
                          )
    specialisation = repeatR (bottomupR (apply "typeSpec" typeSpec)) >->
                     repeatR (bottomupR (apply "nonRepSpec" nonRepSpec))

-- | Brings representable function in the desired normal form:
--
-- * Only top-level lambda's
--
-- * Single Lambda-bound top-level Let-binding, where the body is a variable reference
--
-- * Modified ANF (constants are not let-bound, non-representable arguments to primitives are not let-bound)
--
-- * All let-bindings are representable
simplification :: NormRewrite
simplification = etaTL >-> constSimpl >-> anf >-> deadCodeRemoval >-> letTL

  where
    etaTL           = apply "etaTL" etaExpansionTL

    constSimpl      = repeatR ( upDownR (apply "propagation" appProp) >->
                                bottomupR inlineClosedT >->
                                repeatBottomup  [ ("nonRepANF"       , nonRepANF       )
                                                , ("bindConstantVar" , bindConstantVar )
                                                , ("constantSpec"    , constantSpec    )
                                                , ("caseCon"         , caseCon         )
                                                ]
                              )

    anf             = apply "ANF" makeANF >-> topdownR (apply "bindConstantVar" bindConstantVar)

    deadCodeRemoval = bottomupR (apply "deadcode" deadCode)

    letTL           = bottomupR (apply "topLet" topLet)

    inlineClosedT   = apply "inlineClosedTerm" (inlineClosedTerm
                                                  "normalization"
                                                  normalization
                                               )

-- | Repeatedly apply a set of transformation in a bottom-up traversal
repeatBottomup :: [(String,NormRewrite)] -> NormRewrite
repeatBottomup
  = repeatR
  . foldl1 (>->)
  . map (bottomupR . uncurry apply)
