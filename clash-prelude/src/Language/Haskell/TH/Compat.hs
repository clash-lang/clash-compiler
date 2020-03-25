{-# LANGUAGE CPP #-}

module Language.Haskell.TH.Compat where
import           Language.Haskell.TH.Syntax

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

-- | Compatibility helper to create (total) tuple expressions
mkTupE :: [Exp] -> Exp
mkTupE = TupE
#if MIN_VERSION_template_haskell(2,16,0)
         . map Just
#endif

#if MIN_VERSION_template_haskell(2,16,0)
liftTypedFromUntyped :: Lift a => a -> Q (TExp a)
liftTypedFromUntyped = unsafeTExpCoerce . lift
#endif
