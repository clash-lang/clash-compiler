{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE Trustworthy #-}

module Clash.Sized.Unsigned
  ( Unsigned
    -- * Type-level error messages
  , UnsignedPositiveLiteralError
  )
where

import Clash.Sized.Internal.Unsigned
