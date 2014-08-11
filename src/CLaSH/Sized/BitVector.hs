{-# LANGUAGE MagicHash #-}

module CLaSH.Sized.BitVector
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
    -- ** Concatenation
  , (#>)
  , (<#)
  , (++#)
  )
where

import CLaSH.Sized.Internal.BitVector
