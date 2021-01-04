{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Promoted.Symbol
  (SSymbol (..), ssymbolProxy, ssymbolToString)
where

import Language.Haskell.TH.Syntax
import GHC.Show     (appPrec)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | Singleton value for a type-level string @s@
data SSymbol (s :: Symbol) where
  SSymbol :: KnownSymbol s => SSymbol s

instance KnownSymbol s => Lift (SSymbol (s :: Symbol)) where
--  lift :: t -> Q Exp
  lift t = pure (AppTypeE (ConE 'SSymbol) tt)
    where
      tt = LitT (StrTyLit (ssymbolToString t))

#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

instance Show (SSymbol s) where
  showsPrec d s@SSymbol = showParen (d > appPrec) $
    showString "SSymbol @" . shows (ssymbolToString s)

{-# INLINE ssymbolProxy #-}
-- | Create a singleton symbol literal @'SSymbol' s@ from a proxy for
-- /s/
ssymbolProxy :: KnownSymbol s => proxy s -> SSymbol s
ssymbolProxy _ = SSymbol

{-# INLINE ssymbolToString #-}
-- | Reify the type-level 'Symbol' @s@ to it's term-level 'String'
-- representation.
ssymbolToString :: SSymbol s -> String
ssymbolToString s@SSymbol = symbolVal s
