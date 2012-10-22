module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Util

normalization :: NormRewrite
normalization = (apply "inlinleWrapper" inlineWrapper)
                >-> (repeatR $ clsOpRes >-> representable >-> simplification)
                >-> retVarStep
  where
    clsOpRes = bottomupR $ apply "classOpResolution" classOpResolution

representable :: NormRewrite
representable = monomorphization >-> defunctionalization

monomorphization :: NormRewrite
monomorphization = monomorphization' >-> typeSpecialization
  where
    monomorphization' :: NormRewrite
    monomorphization' = repeatTopdown steps

    typeSpecialization :: NormRewrite
    typeSpecialization = repeatR $ bottomupR (apply "typeSpec" typeSpec)

    steps :: [(String,NormRewrite)]
    steps = [ ("lamApp"    , lamApp    )
            , ("letApp"    , letApp    )
            , ("caseApp"   , caseApp   )
            , ("iotaReduce", iotaReduce)
            , ("letTyApp"  , letTyApp  )
            , ("caseTyApp" , caseTyApp )
            , ("bindPoly"  , bindPoly  )
            , ("liftPoly"  , liftPoly  )
            ]

defunctionalization :: NormRewrite
defunctionalization = repeatR . foldl1 (>->) $ concat
    [ [doInline "inlineBox" inlineBox]
    , map (topdownR . uncurry apply) steps
    , [doInline "inlineHO" inlineHO]
    ]
  where
    steps :: [(String,NormRewrite)]
    steps = [ ("lamApp"   , lamApp    )
            , ("letApp"   , letApp    )
            , ("caseApp"  , caseApp   )
            , ("caseLet"  , caseLet   )
            , ("caseCon"  , caseCon   )
            , ("caseCase" , caseCase  )
            , ("bindBox"  , bindBox   )
            , ("liftFun"  , liftFun   )
            ]

simplification :: NormRewrite
simplification = repeatTopdown steps
  where
    steps = [ ("inlineSimple", inlineSimple)
            , ("lamApp"   , lamApp    )
            , ("letApp"   , letApp    )
            , ("caseApp"  , caseApp   )
            , ("deadcode" , deadCode  )
            , ("etaExpand", etaExpand )
            , ("appSimpl" , appSimpl  )
            , ("retLet"   , retLet    )
            , ("retLam"   , retLam    )
            , ("letFlat"  , letFlat   )
            , ("inlineVar", inlineVar )
            ]

retVarStep :: NormRewrite
retVarStep = topdownR (apply "retVar" retVar)

doInline :: String -> NormRewrite -> NormRewrite
doInline n t = bottomupR (apply n t) >-> commitNewInlined

repeatTopdown :: [(String,NormRewrite)] -> NormRewrite
repeatTopdown
  = repeatR
  . foldl1 (>->)
  . map (topdownR . uncurry apply)
