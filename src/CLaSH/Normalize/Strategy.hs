module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Util

normalization :: NormRewrite
normalization = repeatR $ clsOpRes >-> representable >-> simplification
  where
    clsOpRes = bottomupR $ apply "classOpResolution" classOpResolution

representable :: NormRewrite
representable = propagagition >-> specialisation
  where
    propagagition = repeatR ( repeatTopdown [ ("lamApp"       ,lamApp      )
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
simplification = repeatTopdown steps
  where
    steps = [ ("deadcode"    , deadCode  )
            , ("lamApp"      , lamApp )
            , ("letApp"      , letApp )
            , ("caseApp"     , caseApp )
            , ("repANF"      , repANF )
            , ("nonRepANF"   , nonRepANF )
            , ("subjLet"     , subjLet)
            , ("altLet"      , altLet)
            , ("bodyVar"     , bodyVar)
            , ("letFlat"     , letFlat)
            , ("topLet"      , topLet)
            , ("etaExpansion", etaExpansion)
            , ("inlineVar"   , inlineVar)
            ]

doInline :: String -> NormRewrite -> NormRewrite
doInline n t = bottomupR (apply n t) >-> commitNewInlined

repeatTopdown :: [(String,NormRewrite)] -> NormRewrite
repeatTopdown
  = repeatR
  . foldl1 (>->)
  . map (repeatR . topdownR . uncurry apply)
