-- | Transformation process for normalization
module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Util

-- | Normalisation transformation
normalization :: NormRewrite
normalization = representable >-> simplification >-> apply "recToLetrec" recToLetRec

-- | Simple cleanup transformation, currently only inlines \"Wrappers\"
cleanup :: NormRewrite
cleanup = repeatR $ topdownR (apply "inlineWrapper" inlineWrapper)

-- | Unsure that functions have representable arguments, results, and let-bindings
representable :: NormRewrite
representable = propagagition >-> specialisation
  where
    propagagition = repeatR ( upDownR  (apply "propagation" appProp) >->
                              repeatBottomup [ ("bindNonRep"   , bindNonRep )
                                             , ("liftNonRep"   , liftNonRep )
                                             , ("caseLet"      , caseLet    )
                                             , ("caseCase"     , caseCase   )
                                             , ("caseCon"      , caseCon    )
                                             ]
                              >->
                              doInline "inlineNonRep" inlineNonRep
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
                                bottomupR inlineClosed >->
                                repeatBottomup  [ ("nonRepANF"       , nonRepANF       )
                                                , ("bindConstantVar" , bindConstantVar )
                                                , ("constantSpec"    , constantSpec    )
                                                , ("caseCon"         , caseCon         )
                                                ]
                              )

    anf             = apply "ANF" makeANF

    deadCodeRemoval = bottomupR (apply "deadcode" deadCode)

    letTL           = bottomupR (apply "topLet" topLet)

    inlineClosed    = apply "inlineClosedTerm" (inlineClosedTerm
                                                  "normalization"
                                                  normalization
                                               )

-- | Perform an inlining transformation using a bottomup traversal, and commit
-- inlined function names to the inlining log/cachce
doInline :: String -> NormRewrite -> NormRewrite
doInline n t = bottomupR (apply n t) >-> commitNewInlined

-- | Repeatedly apply a set of transformation in a bottom-up traversal
repeatBottomup :: [(String,NormRewrite)] -> NormRewrite
repeatBottomup
  = repeatR
  . foldl1 (>->)
  . map (bottomupR . uncurry apply)
