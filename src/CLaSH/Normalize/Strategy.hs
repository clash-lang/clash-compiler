module CLaSH.Normalize.Strategy where

import CLaSH.Normalize.Transformations
import CLaSH.Normalize.Types
import CLaSH.Rewrite.Combinators
import CLaSH.Rewrite.Util

normalization :: NormRewrite
normalization = monomorphization >-> simplification

monomorphization :: NormRewrite
monomorphization = monomorphization' >-> typeSpecialization
  where
    monomorphization' = repeatTopdown steps

    typeSpecialization = bottomupR (apply "typeSpec" typeSpec)

    steps = [ ("lamApp"    , lamApp    )
            , ("letApp"    , letApp    )
            , ("caseApp"   , caseApp   )
            , ("iotaReduce", iotaReduce)
            , ("letTyApp"  , letTyApp  )
            , ("caseTyApp" , caseTyApp )
            , ("bindPoly"  , bindPoly  )
            , ("liftPoly"  , liftPoly  )
            ]

simplification :: NormRewrite
simplification = repeatTopdown steps
  where
    steps = [ ("lamApp"   , lamApp    )
            , ("letApp"   , letApp    )
            , ("caseApp"  , caseApp   )
            , ("deadcode" , deadCode  )
            , ("etaExpand", etaExpand )
            , ("appSimpl" , appSimpl  )
            , ("letFlat"  , letFlat   )
            , ("inlineVar", inlineVar )
            ]

repeatTopdown :: [(String,NormRewrite)] -> NormRewrite
repeatTopdown
  = repeatR
  . foldl1 (>->)
  . map (topdownR . uncurry apply)
