module Clash.Hedgehog.Annotations.SynthesisAttributes where

import Clash.Annotations.SynthesisAttributes (Attr(..))

import Hedgehog
import qualified Hedgehog.Gen as Gen

genAttr :: forall m. MonadGen m => Range Int -> m (Attr String)
genAttr range =
  Gen.choice
    [ BoolAttr <$> genAlphaNum <*> Gen.bool
    , IntegerAttr <$> genAlphaNum <*> genInteger
    , StringAttr <$> genAlphaNum <*> genAlphaNum
    , Attr <$> genAlphaNum
    ]
 where
  genAlphaNum = Gen.string range Gen.alphaNum
  genInteger  = toInteger <$> Gen.integral range
