{-|
  Copyright   :  (C) 2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.Extra where

#if !(MIN_VERSION_GLASGOW_HASKELL(8,0,2,0))
import Control.DeepSeq
import SrcLoc (SrcSpan)

instance NFData SrcSpan where
  rnf x = x `seq` ()
#endif
