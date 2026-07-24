{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Data.FastString.Extra where

import GHC.Data.FastString
  (FastString (..), FastZString, bytesFS, fastZStringToByteString, mkFastStringByteList)
import Data.Binary (Binary (..), Get)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable(hashWithSalt))
import Unsafe.Coerce (unsafeCoerce)

instance Hashable FastString where
  hashWithSalt salt fs = hashWithSalt salt (uniq fs)

instance Binary FastString where
  put str = put $ bytesFS str
  get = mkFastStringByteList <$> get

instance Binary FastZString where
  put = put . fastZStringToByteString
  get = unsafeCoerce (get :: Get ByteString)
