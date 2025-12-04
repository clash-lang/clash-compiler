{-# LANGUAGE CPP #-}

module Language.Haskell.TH.Compat where
import           Language.Haskell.TH.Syntax

-- | Compatibility helper to create TySynInstD
mkTySynInstD :: Name -> [Type] -> Type -> Dec
mkTySynInstD tyConNm tyArgs rhs =
        TySynInstD (TySynEqn Nothing
                     (foldl AppT (ConT tyConNm) tyArgs)
                     rhs)

-- | Compatibility helper to create (total) tuple expressions
mkTupE :: [Exp] -> Exp
mkTupE = TupE
         . map Just

liftTypedFromUntyped :: (Lift a, Quote m) => a -> Code m a
liftTypedFromUntyped = unsafeCodeCoerce . lift
