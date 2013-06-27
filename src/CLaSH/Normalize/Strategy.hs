module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Util

normalization :: NormRewrite
normalization = (repeatR $ clsOpRes >-> representable) >-> simplification
  where
    clsOpRes = (unsafeBottomupR $ apply "classOpResolution" classOpResolution) >->
               (unsafeBottomupR $ apply "inlineSingularDFun" inlineSingularDFun)

cleanup :: NormRewrite
cleanup = (repeatR $ closedTerms >-> wrappers >-> propConstants) >->
          (unsafeBottomupR (apply "letFlat" letFlat)) >->
          (apply "bindConstantVar" bindConstantVar)
  where
    wrappers      = unsafeTopdownR $ (apply "inlineWrapper" inlineWrapper) !-> propConstants

    closedTerms   = unsafeTopdownR $ (apply "inlineClosedTerm" inlineClosedTerm) !-> propConstants

    propConstants = repeatTopdown steps
    steps         = [ ("propagation"     , appProp)
                    , ("bindConstantVar" , bindConstantVar)
                    , ("constantSpec"    , constantSpec)
                    , ("caseCon"         , caseCon    )
                    ]

representable :: NormRewrite
representable = propagagition >-> specialisation
  where
    propagagition = repeatR ( repeatBottomup [ ("propagation"  , appProp    )
                                             , ("bindNonRep"   , bindNonRep )
                                             , ("liftNonRep"   , liftNonRep )
                                             , ("caseLet"      , caseLet    )
                                             , ("caseCon"      , caseCon    )
                                             , ("caseCase"     , caseCase   )
                                             ]
                              >->
                              doInline "inlineNonRep" inlineNonRep
                            )
    specialisation = repeatR (unsafeBottomupR (apply "typeSpec" typeSpec)) >->
                     repeatR (unsafeBottomupR (apply "nonRepSpec" nonRepSpec))

simplification :: NormRewrite
simplification = (apply "etaTL" etaExpansionTL) >->
                 (repeatR $ unsafeTopdownR $ apply "propagation" appProp) >->
                 (bottomupR (apply "nonRepANF" nonRepANF)) >->
                 (apply "ANF" makeANF) >->
                 repeatBottomup steps
  where
    steps = [ ("nonRepANF", nonRepANF)
            , ("topLet"   , topLet)
            , ("bodyVar"  , bodyVar)
            , ("deadcode" , deadCode)
            ]

doInline :: String -> NormRewrite -> NormRewrite
doInline n t = unsafeBottomupR (apply n t) >-> commitNewInlined

repeatBottomup :: [(String,NormRewrite)] -> NormRewrite
repeatBottomup
  = repeatR
  . foldl1 (>->)
  . map (unsafeBottomupR . uncurry apply)

repeatTopdown :: [(String,NormRewrite)] -> NormRewrite
repeatTopdown
  = repeatR
  . foldl1 (>->)
  . map (unsafeTopdownR . uncurry apply)
