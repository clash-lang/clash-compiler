{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeOperators    #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Sized.Index
  (Index, bv2i)
where

import GHC.TypeLits               (KnownNat, type (^))

import CLaSH.Sized.BitVector      (BitVector)
import CLaSH.Sized.Internal.Index

bv2i :: KnownNat (2^n) => BitVector n -> Index (2^n)
bv2i = unpack#
