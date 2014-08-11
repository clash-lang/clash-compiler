{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module CLaSH.Promoted.Symbol where

import Data.Proxy   (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | Singleton value for a type-level string @s@
data SSymbol (s :: Symbol) = KnownSymbol s => SSymbol (Proxy s)

instance Show (SSymbol s) where
  show (SSymbol p) = symbolVal p

-- | Create a singleton literal for a type-level natural number
ssymbol :: KnownSymbol s => SSymbol s
ssymbol = SSymbol Proxy
