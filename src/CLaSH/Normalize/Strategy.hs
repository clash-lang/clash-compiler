module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Util

normalization :: NormRewrite
normalization = representable >-> simplification

cleanup :: NormRewrite
cleanup = repeatR $ unsafeTopdownR (apply "inlineWrapper" inlineWrapper)

representable :: NormRewrite
representable = (clsOpRes >-> propagagition >-> specialisation) !->
                repeatR (clsOpRes !-> (propagagition >-> specialisation))
  where
    clsOpRes = (unsafeBottomupR $ apply "classOpResolution"  classOpResolution) >->
               (unsafeBottomupR $ apply "inlineSingularDFun" inlineSingularDFun)

    propagagition = repeatR ( unsafeUpDownR  (apply "propagation" appProp) >->
                              repeatBottomup [ ("bindNonRep"   , bindNonRep )
                                             , ("liftNonRep"   , liftNonRep )
                                             , ("caseLet"      , caseLet    )
                                             , ("caseCase"     , caseCase   )
                                             , ("caseCon"      , caseCon    )
                                             ]
                              >->
                              doInline "inlineNonRep" inlineNonRep
                            )
    specialisation = repeatR (unsafeBottomupR (apply "typeSpec" typeSpec)) >->
                     repeatR (unsafeBottomupR (apply "nonRepSpec" nonRepSpec))

simplification :: NormRewrite
simplification = etaTL >-> constSimpl >-> anf >-> deadCodeRemoval >-> letTL

  where
    etaTL           = apply "etaTL" etaExpansionTL

    constSimpl      = repeatR ( unsafeUpDownR   (apply "propagation" appProp) >->
                                unsafeBottomupR (apply "inlineClosedTerm" (inlineClosedTerm !-> representable)) >->
                                repeatBottomup  [ ("nonRepANF"       , nonRepANF       )
                                                , ("bindConstantVar" , bindConstantVar )
                                                , ("constantSpec"    , constantSpec    )
                                                , ("caseCon"         , caseCon         )
                                                ]
                              )

    anf             = apply "ANF" makeANF

    deadCodeRemoval = unsafeBottomupR (apply "deadcode" deadCode)

    letTL           = unsafeBottomupR (apply "topLet" topLet)

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
