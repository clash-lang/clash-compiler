{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Promoted.Symbol
  (SSymbol (..), ssymbol, ssymbolToString)
where

import Data.Data
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | Singleton value for a type-level string @s@
data SSymbol (s :: Symbol) = KnownSymbol s => SSymbol (Proxy s)

instance Show (SSymbol s) where
  show (SSymbol s) = symbolVal s

instance (Typeable n,KnownSymbol n) => Data (SSymbol n) where
  gfoldl _ z (SSymbol n) = z (SSymbol n)
  toConstr (SSymbol _)   = ssymbolConstr
  gunfold _ z c          = case constrIndex c of
                             1 -> z (SSymbol Proxy)
                             _ -> error "Data.Data.gunfold(SSymbol n)"
  dataTypeOf _           = ssymbolDataType

ssymbolConstr :: Constr
ssymbolConstr = mkConstr ssymbolDataType "SSymbol" [] Prefix

ssymbolDataType :: DataType
ssymbolDataType = mkDataType "CLaSH.Promoted.Symbol.SSymbol" [ssymbolConstr]

{-# INLINE ssymbol #-}
-- | Create a singleton literal for a type-level natural number
ssymbol :: KnownSymbol s => SSymbol s
ssymbol = SSymbol Proxy

{-# INLINE ssymbolToString #-}
-- | Reify the type-level 'Symbol' @s@ to it's term-level 'String'
-- representation.
ssymbolToString :: SSymbol s -> String
ssymbolToString (SSymbol s) = symbolVal s
