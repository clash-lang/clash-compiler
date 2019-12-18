{-# LANGUAGE CPP #-}

module Language.Haskell.TH.Compat where
import           Language.Haskell.TH

-- | Compatibility helper to create TySynInstD
mkTySynInstD :: Name -> [Type] -> Type -> Dec
mkTySynInstD tyConNm tyArgs rhs =
#if MIN_VERSION_template_haskell(2,15,0)
        TySynInstD (TySynEqn Nothing
                     (foldl AppT (ConT tyConNm) tyArgs)
                     rhs)
#else
        TySynInstD tyConNm
                   (TySynEqn tyArgs
                             rhs)
#endif
