module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Util

normalization :: NormRewrite
normalization = representable >-> simplification >-> apply "recToLetrec" recToLetRec

cleanup :: NormRewrite
cleanup = repeatR $ topdownR (apply "inlineWrapper" inlineWrapper)

representable :: NormRewrite
representable = (clsOpRes >-> propagagition >-> specialisation) !->
                repeatR (clsOpRes !-> (propagagition >-> specialisation))
  where
    clsOpRes = bottomupR (apply "classOpResolution"  classOpResolution) >->
               bottomupR (apply "inlineSingularDFun" inlineSingularDFun)

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

simplification :: NormRewrite
simplification = etaTL >-> constSimpl >-> anf >-> deadCodeRemoval >-> letTL

  where
    etaTL           = apply "etaTL" etaExpansionTL

    constSimpl      = repeatR ( upDownR   (apply "propagation" appProp) >->
                                bottomupR (apply "inlineClosedTerm" (inlineClosedTerm !-> representable)) >->
                                repeatBottomup  [ ("nonRepANF"       , nonRepANF       )
                                                , ("bindConstantVar" , bindConstantVar )
                                                , ("constantSpec"    , constantSpec    )
                                                , ("caseCon"         , caseCon         )
                                                ]
                              )

    anf             = apply "ANF" makeANF

    deadCodeRemoval = bottomupR (apply "deadcode" deadCode)

    letTL           = bottomupR (apply "topLet" topLet)

doInline :: String -> NormRewrite -> NormRewrite
doInline n t = bottomupR (apply n t) >-> commitNewInlined

repeatBottomup :: [(String,NormRewrite)] -> NormRewrite
repeatBottomup
  = repeatR
  . foldl1 (>->)
  . map (bottomupR . uncurry apply)

repeatTopdown :: [(String,NormRewrite)] -> NormRewrite
repeatTopdown
  = repeatR
  . foldl1 (>->)
  . map (topdownR . uncurry apply)
