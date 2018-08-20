{-|
  Copyright   :  (C) 2017, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.BasicTypes.Extra where

import BasicTypes
import Control.DeepSeq
import Data.Binary
import GHC.Generics

deriving instance Generic InlineSpec
instance NFData InlineSpec
instance Binary InlineSpec
