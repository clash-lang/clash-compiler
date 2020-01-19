{-|
Copyright  :  (C) 2018, Google Inc
                  2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Generic clock related utilities.
-}

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Clocks (Clocks(..)) where

import Data.Kind (Constraint)

import Clash.Signal.Internal
import Clash.Clocks.Deriving (deriveClocksInstances)

class Clocks t where
  type ClocksCxt t :: Constraint

  clocks
    :: (KnownDomain domIn, ClocksCxt t)
    => Clock domIn
    -> Reset domIn
    -> t

deriveClocksInstances 16
