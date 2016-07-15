{-|
  Copyright   :  (C) 2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Extra where

import Control.DeepSeq
import SrcLoc (SrcSpan)

instance NFData SrcSpan where
  rnf x = x `seq` ()
