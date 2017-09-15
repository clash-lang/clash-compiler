{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE MagicHash #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Sized.BitVector
  ( -- * Datatypes
    BitVector
  , Bit
    -- * Accessors
    -- ** Length information
  , size#
  , maxIndex#
    -- * Construction
    -- ** Initialisation
  , high
  , low
  , bLit
    -- ** Concatenation
  , (++#)
  )
where

import Clash.Sized.Internal.BitVector
