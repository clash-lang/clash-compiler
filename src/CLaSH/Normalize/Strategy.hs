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
defunctionalization = defunctionalization' >-> boxSpecialization
  where
    defunctionalization' :: NormRewrite
    defunctionalization' =
      repeatR . foldl1 (>->) $ concat
        [ [doInline "inlineBox" inlineBox]
        , map (topdownR . uncurry apply) steps
        , [doInline "inlineHO" inlineHO]
        ]

    boxSpecialization :: NormRewrite
    boxSpecialization = repeatR $ bottomupR (apply "boxSpec" boxSpec)

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
simplification = (repeatTopdown steps) >->
                 simpleSpecialization
  where
    steps = [ ("inlineSimple", inlineSimple)
            , ("lamApp"   , lamApp    )
            , ("letApp"   , letApp    )
            , ("caseApp"  , caseApp   )
            , ("deadcode" , deadCode  )
            , ("etaExpand", etaExpand )
            , ("appSimpl" , appSimpl  )
            , ("bindSimple", bindSimple)
            , ("inlineVar", inlineVar )
            , ("inlineWrapper", inlineWrapper)
            ]

    simpleSpecialization = repeatR $ bottomupR (apply "simpleSpec" simpleSpec)

retVarStep :: NormRewrite
retVarStep = repeatTopdown [ ("retLet" , retLet  )
                           , ("retLam" , retLam  )
                           , ("retVar" , retVar  )
                           , ("letFlat", letFlat )
                           ]

doInline :: String -> NormRewrite -> NormRewrite
doInline n t = bottomupR (apply n t) >-> commitNewInlined

repeatTopdown :: [(String,NormRewrite)] -> NormRewrite
repeatTopdown
  = repeatR
  . foldl1 (>->)
  . map (topdownR . uncurry apply)
