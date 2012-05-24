module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Normalize.Util
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Util

normalization :: NormRewrite
normalization = monomorphization >-> defunctionalization >-> simplification

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
defunctionalization = defunctionalization' >-> functionSpecialization
  where
    defunctionalization' :: NormRewrite
    defunctionalization'
      = repeatR
      $ foldl1 (>->)
      $ (doInlineBox:(map (topdownR . uncurry apply) steps))

    steps :: [(String,NormRewrite)]
    steps = [ ("lamApp"   , lamApp    )
            , ("letApp"   , letApp    )
            , ("caseApp"  , caseApp   )
            , ("caseLet"  , caseLet   )
            , ("caseCon"  , caseCon   )
            , ("caseCase" , caseCase  )
            , ("bindFun"  , bindFun   )
            , ("liftFun"  , liftFun   )
            ]

    doInlineBox :: NormRewrite
    doInlineBox = bottomupR (apply "inlineBox" inlineBox) >->
                  commitNewInlined

    functionSpecialization :: NormRewrite
    functionSpecialization = repeatR $ bottomupR (apply "funSpec" funSpec)

simplification :: NormRewrite
simplification = repeatTopdown steps
  where
    steps = [ ("lamApp"   , lamApp    )
            , ("letApp"   , letApp    )
            , ("caseApp"  , caseApp   )
            , ("deadcode" , deadCode  )
            , ("etaExpand", etaExpand )
            , ("appSimpl" , appSimpl  )
            , ("retLet"   , retLet    )
            , ("letFlat"  , letFlat   )
            , ("inlineVar", inlineVar )
            ]

repeatTopdown :: [(String,NormRewrite)] -> NormRewrite
repeatTopdown
  = repeatR
  . foldl1 (>->)
  . map (topdownR . uncurry apply)
