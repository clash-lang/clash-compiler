{-|
Copyright  :  (C) 2013-2016, University of Twente
                  2021,      QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.Prelude.BitReduction {-# DEPRECATED "Use Clash.Class.BitPack instead. This module will be removed in Clash 1.8." #-}
  ( reduceAnd
  , reduceOr
  , reduceXor ) where

import Clash.Class.BitPack.BitReduction
