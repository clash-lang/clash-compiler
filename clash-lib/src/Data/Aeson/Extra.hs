{-|
  Copyright   :  (C) 2015-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module Data.Aeson.Extra where

import Data.Aeson           (FromJSON, Result (..), fromJSON, json)
import Data.Attoparsec.Lazy (Result (..), parse)
import Data.ByteString.Lazy (ByteString)
import Clash.Util           (traceIf)

-- | Parse a ByteString according to the given JSON template. Prints failures
-- on @stdout@, and returns 'Nothing' if parsing fails.
decodeAndReport :: (FromJSON a)
                => ByteString -- ^ Bytestring to parse
                -> Maybe a
decodeAndReport s =
  case parse json s of
    Done _ v -> case fromJSON v of
                    Success a -> Just a
                    Error msg -> traceIf True msg Nothing
    Fail _ _ msg -> traceIf True msg Nothing
