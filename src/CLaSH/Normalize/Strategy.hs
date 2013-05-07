module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Util

normalization :: NormRewrite
normalization = (repeatR $ clsOpRes >-> representable) >-> simplification
  where
    clsOpRes = (bottomupR $ apply "classOpResolution" classOpResolution) >->
               (bottomupR $ apply "inlineSingularDFun" inlineSingularDFun)

cleanupTD :: NormRewrite
cleanupTD = repeatBottomup steps
  where
    steps = [ ("inlineClosedTerm", inlineClosedTerm)
            , ("lamApp"          , lamApp )
            , ("repANF"          , repANF )
            , ("letFlat"         , letFlat)
            , ("bindConstant"    , bindConstant)
            , ("inlineVar"       , inlineVar)
            , ("constantSpec"    , constantSpec)
            ]

cleanupBU :: NormRewrite
cleanupBU = repeatR $ topdownR (apply "inlineWrapper" inlineWrapper)

representable :: NormRewrite
representable = propagagition >-> specialisation
  where
    propagagition = repeatR ( repeatBottomup [ ("lamApp"       ,lamApp      )
                                             , ("letApp"       ,letApp      )
                                             , ("caseApp"      ,caseApp     )
                                             , ("tauReduction" ,tauReduction)
                                             , ("letTyApp"     ,letTyApp    )
                                             , ("caseTyApp"    ,caseTyApp   )
                                             , ("bindNonRep"   ,bindNonRep  )
                                             , ("liftNonRep"   ,liftNonRep  )
                                             , ("caseLet"      , caseLet    )
                                             , ("caseCon"      , caseCon    )
                                             , ("caseCase"     , caseCase   )
                                             ]
                              >->
                              doInline "inlineNonRep" inlineNonRep
                            )
    specialisation = repeatR (bottomupR (apply "typeSpec" typeSpec)) >->
                     repeatR (bottomupR (apply "nonRepSpec" nonRepSpec))

simplification :: NormRewrite
simplification = repeatBottomup steps
  where
    steps = [ ("deadcode"        , deadCode  )
            , ("caseCon"         , caseCon    )
            , ("lamApp"          , lamApp )
            , ("letApp"          , letApp )
            , ("caseApp"         , caseApp )
            , ("repANF"          , repANF )
            , ("nonRepANF"       , nonRepANF )
            , ("subjLet"         , subjLet)
            , ("altLet"          , altLet)
            , ("bodyVar"         , bodyVar)
            , ("letFlat"         , letFlat)
            , ("topLet"          , topLet)
            , ("etaExpansion"    , etaExpansion)
            ]

doInline :: String -> NormRewrite -> NormRewrite
doInline n t = bottomupR (apply n t) >-> commitNewInlined

repeatBottomup :: [(String,NormRewrite)] -> NormRewrite
repeatBottomup
  = repeatR
  . foldl1 (>->)
  . map (bottomupR . uncurry apply)
