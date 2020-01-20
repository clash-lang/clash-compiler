{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Sized.BitVector
  ( -- * Bit
    Bit
    -- ** Construction
    -- *** Initialisation
  , high
  , low
    -- * BitVector
  , BitVector
    -- ** Accessors
    -- *** Length information
  , size#
  , maxIndex#
    -- ** Construction
  , bLit
    -- ** Concatenation
  , (++#)
    -- ** Pattern matching
  , bitPattern
  )
where

import Clash.Sized.Internal.BitVector
