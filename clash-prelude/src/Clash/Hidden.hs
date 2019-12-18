{-|
Copyright  :  (C) 2018     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Hidden arguments
-}

{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE Trustworthy #-}

module Clash.Hidden
  ( Hidden
  , expose
  -- * OverloadedLabels
  , fromLabel
  )
where

import qualified GHC.Classes
import GHC.TypeLits
import Unsafe.Coerce

-- | A value reflected to, or /hiding/ at, the /Constraint/ level
--
-- e.g. a function:
--
-- @
-- f :: Hidden "foo" Int
--   => Bool
--   -> Int
-- f = ...
-- @
--
-- has a /normal/ argument of type @Bool@, and a /hidden/ argument called \"foo\"
-- of type @Int@. In order to apply the @Int@ argument we have to use the
-- 'expose' function, so that the /hidden/ argument becomes a normal argument
-- again.
--
-- === __Original implementation__
--
-- 'Hidden' used to be implemented by:
--
-- @
-- class Hidden (x :: Symbol) a | x -> a where
--   hidden :: a
-- @
--
-- which is equivalent to /IP/, except that /IP/ has magic inference rules
-- bestowed by GHC so that there's never any ambiguity. We need these magic
-- inference rules so we don't end up in type inference absurdity where asking
-- for the type of an type-annotated value results in a /no-instance-in-scope/
-- error.
type Hidden (x :: Symbol) a = GHC.Classes.IP x a

newtype Secret x a r = Secret (Hidden x a => r)

-- | Expose a 'Hidden' argument so that it can be applied normally, e.g.
--
-- @
-- f :: Hidden "foo" Int
--   => Bool
--   -> Int
-- f = ...
--
-- g :: Int -> Bool -> Int
-- g = 'expose' \@\"foo" f
-- @
expose
  :: forall x a r
   . (Hidden x a => r)
  -- ^ Function with a 'Hidden' argument
  -> (a -> r)
  -- ^ Function with the 'Hidden' argument exposed
expose k = unsafeCoerce (Secret @x @a @r k)
{-# INLINE expose #-}

-- | Using /-XOverloadedLabels/ and /-XRebindableSyntax/, we can turn any
-- value into a /hidden/ argument using the @#foo@ notation, e.g.:
--
-- @
-- f :: Int -> Bool -> Int
-- f = ...
--
-- g :: Hidden "foo" Bool
--   => Int -> Int
-- g i = f i #foo
-- @
fromLabel :: forall x a . Hidden x a => a
fromLabel = GHC.Classes.ip @x
{-# INLINE fromLabel #-}
