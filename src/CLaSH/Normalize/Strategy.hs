module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Util

normalization :: NormRewrite
normalization = repeatR $ clsOpRes >-> representable >-> simplification
                -- >-> retVarStep
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
            , ("funANF"      , funANF )
            , ("conPrimANF"  , conPrimANF )
            , ("subjLet"     , subjLet)
            -- , ("altLet"      , altLet)
            , ("bodyVar"     , bodyVar)
            , ("letFlat"     , letFlat)
            , ("topLet"      , topLet)
            , ("etaExpansion", etaExpansion)
            ]

-- simplification :: NormRewrite
-- simplification = (repeatTopdown steps) >->
--                  simpleSpecialization
--   where
--     steps = [ ("inlineSimple", inlineSimple)
--             , ("lamApp"   , lamApp    )
--             , ("letApp"   , letApp    )
--             , ("caseApp"  , caseApp   )
--             , ("deadcode" , deadCode  )
--             , ("etaExpand", etaExpand )
--             , ("appSimpl" , appSimpl  )
--             , ("bindSimple", bindSimple)
--             , ("inlineVar", inlineVar )
--             -- , ("inlineWrapper", inlineWrapper)
--             ]
--
--     simpleSpecialization = repeatR $ bottomupR (apply "simpleSpec" simpleSpec)
--
-- retVarStep :: NormRewrite
-- retVarStep = repeatTopdown [ ("retLet" , retLet  )
--                            , ("retLam" , retLam  )
--                            , ("retVar" , retVar  )
--                            , ("letFlat", letFlat )
--                            ]

doInline :: String -> NormRewrite -> NormRewrite
doInline n t = bottomupR (apply n t) >-> commitNewInlined

repeatTopdown :: [(String,NormRewrite)] -> NormRewrite
repeatTopdown
  = repeatR
  . foldl1 (>->)
  . map (repeatR . topdownR . uncurry apply)
