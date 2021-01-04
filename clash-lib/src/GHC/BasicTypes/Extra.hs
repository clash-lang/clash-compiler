{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.BasicTypes.Extra where

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Basic
#else
import BasicTypes
#endif
import Control.DeepSeq
import Data.Binary
import GHC.Generics

deriving instance Generic InlineSpec
instance NFData InlineSpec
instance Binary InlineSpec
