module CLaSH.Primitives.Util where

import           Data.Aeson           (FromJSON,fromJSON,json,Result(..))
import qualified Data.Attoparsec.Lazy as L
import           Data.ByteString.Lazy (ByteString)

import CLaSH.Util

decodeAndReport ::
  (FromJSON a)
  => ByteString
  -> Maybe a
decodeAndReport s =
  case L.parse json s of
    L.Done _ v -> case fromJSON v of
                    Success a -> Just a
                    Error msg -> traceIf True msg Nothing
    L.Fail _ _ msg -> traceIf True msg Nothing
