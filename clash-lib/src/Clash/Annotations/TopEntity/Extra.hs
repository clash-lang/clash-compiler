{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module Clash.Annotations.TopEntity.Extra where

import Clash.Annotations.TopEntity (PortName, TopEntity)
import Clash.Netlist.Types (TopEntityT)
import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Language.Haskell.TH.Syntax (
  ModName,
  Name,
  NameFlavour,
  NameSpace,
  OccName,
  PkgName,
 )

instance Binary TopEntityT
instance Binary TopEntity
instance Binary PortName

instance Binary Name
instance Binary OccName
instance Binary NameFlavour
instance Binary ModName
instance Binary NameSpace
instance Binary PkgName

instance Hashable TopEntityT
instance Hashable TopEntity
instance Hashable PortName

instance Hashable ModName
instance Hashable Name
instance Hashable NameFlavour
instance Hashable NameSpace
instance Hashable PkgName
instance Hashable OccName

instance NFData TopEntityT
instance NFData TopEntity
instance NFData PortName

instance NFData ModName
instance NFData Name
instance NFData NameFlavour
instance NFData NameSpace
instance NFData PkgName
instance NFData OccName
