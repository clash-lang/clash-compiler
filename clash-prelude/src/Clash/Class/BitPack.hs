
{-|
Copyright  :  (C) 2013-2016, University of Twente
                  2016-2017, Myrtle Software Ltd
                       2021, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}

module Clash.Class.BitPack
  ( BitPack (..)
  , isLike
  , bitCoerce
  , bitCoerceMap
  , boolToBV
  , boolToBit
  , bitToBool
  , packXWith

  -- * Bit Indexing
  , (!)
  , slice
  , split
  , replaceBit
  , setSlice
  , msb
  , lsb

  -- * Bit Reduction
  , reduceAnd
  , reduceOr
  , reduceXor
  )
where

import Clash.Class.BitPack.Internal
import Clash.Class.BitPack.BitIndex
import Clash.Class.BitPack.BitReduction
