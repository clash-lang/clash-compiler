{-|
Copyright  :  (C) 2018, Google Inc
                  2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Generic clock related utilities.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Clocks (Clocks, clocks) where

import Clash.Signal.Internal
import Clash.Clocks.Deriving (deriveClocksInstances)

class Clocks t where
  clocks
    :: Clock domIn
    -> Reset domIn
    -> t

deriveClocksInstances 16
