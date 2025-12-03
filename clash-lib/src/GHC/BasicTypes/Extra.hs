{-|
  Copyright   :  (C) 2017, Google Inc.
                     2023, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.BasicTypes.Extra where

import GHC.Types.Basic
import Control.DeepSeq
import Data.Binary
import GHC.Generics

import GHC.Types.SourceText

#if MIN_VERSION_ghc(9,8,0)
import Data.ByteString
import GHC.Data.FastString
import Unsafe.Coerce
#endif

deriving instance Generic InlineSpec
instance NFData InlineSpec
instance Binary InlineSpec

#if MIN_VERSION_ghc(9,8,0)
deriving instance Generic FastString
instance Binary FastString
instance Binary FastZString where
  put = put . fastZStringToByteString
  get = unsafeCoerce (get :: Get ByteString)
#endif

deriving instance Generic SourceText
#if !MIN_VERSION_ghc(9,8,0)
instance NFData SourceText
#endif
instance Binary SourceText

-- | Determine whether given 'InlineSpec' is NOINLINE or more strict (OPAQUE)
isNoInline :: InlineSpec -> Bool
isNoInline NoInline{} = True
isNoInline Opaque{} = True
isNoInline _ = False

-- | Determine whether given 'InlineSpec' is OPAQUE. If this function is used on
-- a GHC that does not support OPAQUE yet (<9.4), it will return 'True' if given
-- 'InlineSpec' is NOINLINE instead.
isOpaque :: InlineSpec -> Bool
isOpaque Opaque{} = True
isOpaque _ = False
