{-|
  Copyright   :  (C) 2012-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Clash.Core.Term where

import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)
import Clash.Core.Name (Name)

data Term

data TickInfo

type TmName = Name Term

instance Generic Term

data PrimInfo

data NameMod
instance Show NameMod
instance NFData NameMod

data WorkInfo
instance Show WorkInfo
instance NFData WorkInfo

data IsMultiPrim
instance Show IsMultiPrim
instance NFData IsMultiPrim
