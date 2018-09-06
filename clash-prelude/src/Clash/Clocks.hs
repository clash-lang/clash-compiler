{-|
Copyright  :  (C) 2018, Google Inc
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Generic clock related utilities.
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clash.Clocks (Clocks, clocks) where

import Clash.Signal.Internal
import Clash.Clocks.Deriving (deriveClocksInstances)

class Clocks t where
  clocks
    :: Clock pplIn 'Source
    -> Reset pplIn 'Asynchronous
    -> t

deriveClocksInstances 16
