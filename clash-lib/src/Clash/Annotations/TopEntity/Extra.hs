{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Clash.Annotations.TopEntity.Extra where

import Clash.Annotations.TopEntity (TopEntity, PortName)
import Language.Haskell.TH.Syntax
  (ModName, Name, NameFlavour, NameSpace, PkgName, OccName)
import Data.Hashable               (Hashable)

instance Hashable TopEntity
instance Hashable PortName

instance Hashable ModName
instance Hashable Name
instance Hashable NameFlavour
instance Hashable NameSpace
instance Hashable PkgName
instance Hashable OccName
