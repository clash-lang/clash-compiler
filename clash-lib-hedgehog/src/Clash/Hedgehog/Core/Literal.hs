{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random type-directed generation of literals.
-}

module Clash.Hedgehog.Core.Literal
  ( genLiteralFrom
  ) where

import Data.Binary.IEEE754 (doubleToWord, floatToWord)
import qualified Data.Primitive.ByteArray as BA (byteArrayFromList)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Core.Literal
import Clash.Core.Pretty (showPpr)
import Clash.Core.Subst (aeqType)
import Clash.Core.Type (Type)
import Clash.Core.TysPrim

-- | Generate a 'Literal' with the specified core type. If the type does not
-- correspond to a known 'PrimTyCon' (as defined in "Clash.Core.TysPrim") then
-- an error is returned.
--
genLiteralFrom
  :: forall m
   . MonadGen m
  => Type
  -- ^ The type of the literal to generate
  -> m Literal
genLiteralFrom ty
  | aeqType ty integerPrimTy = genIntegerLiteral
  | aeqType ty intPrimTy = genIntLiteral
  | aeqType ty wordPrimTy = genWordLiteral
  | aeqType ty int64PrimTy = genInt64Literal
  | aeqType ty word64PrimTy = genWord64Literal
  | aeqType ty stringPrimTy = genStringLiteral
  | aeqType ty floatPrimTy = genFloatLiteral
  | aeqType ty doublePrimTy = genDoubleLiteral
  | aeqType ty charPrimTy = genCharLiteral
  | aeqType ty naturalPrimTy = genNaturalLiteral
  | aeqType ty byteArrayPrimTy = genByteArrayLiteral
  | otherwise =
      error $ unlines
        [ "genLiteralFrom: No constructors for " <> showPpr ty
        , "Check that this type is a primitive, and is not a void type."
        ]

-- TODO It would be nice to pass ranges into these types instead of just
-- guessing using some default range. However, that makes 'genLiteralFrom'
-- slightly more involved to write.
--
-- Without passing ranges to these, they may bias towards unrealistic values
-- which makes generating entire random programs less realistic.

genIntegerLiteral :: forall m. MonadGen m => m Literal
genIntegerLiteral =
  fmap IntegerLiteral . Gen.sized $ \size ->
    let upper = 2 ^ Range.unSize size
        lower = negate upper
     in Gen.integral (Range.linear lower upper)

genIntLiteral :: forall m. MonadGen m => m Literal
genIntLiteral =
  IntLiteral <$> (toInteger <$> Gen.int Range.linearBounded)

genWordLiteral :: forall m. MonadGen m => m Literal
genWordLiteral =
  WordLiteral <$> (toInteger <$> Gen.word Range.linearBounded)

genInt64Literal :: forall m. MonadGen m => m Literal
genInt64Literal =
  Int64Literal <$> (toInteger <$> Gen.int64 Range.linearBounded)

genWord64Literal :: forall m. MonadGen m => m Literal
genWord64Literal =
  Word64Literal <$> (toInteger <$> Gen.word64 Range.linearBounded)

genStringLiteral :: forall m. MonadGen m => m Literal
genStringLiteral =
  StringLiteral <$> Gen.string (Range.linear 10 50) Gen.unicode

genFloatLiteral :: forall m. MonadGen m => m Literal
genFloatLiteral =
  let range = Range.linearFrac 1.17549435e-38 3.40282347e+38
   in FloatLiteral <$> (floatToWord <$> Gen.float range)

genDoubleLiteral :: forall m. MonadGen m => m Literal
genDoubleLiteral =
  let range = Range.linearFrac 2.2250738585072014e-308 1.7976931348623157e+308
   in DoubleLiteral <$> (doubleToWord <$> Gen.double range)

genCharLiteral :: forall m. MonadGen m => m Literal
genCharLiteral =
  CharLiteral <$> Gen.ascii

genNaturalLiteral :: forall m. MonadGen m => m Literal
genNaturalLiteral =
  fmap NaturalLiteral . Gen.sized $ \size ->
    let upper = 2 ^ Range.unSize size
     in Gen.integral (Range.linear 0 upper)

genByteArrayLiteral :: forall m. MonadGen m => m Literal
genByteArrayLiteral = do
  bytes <- Gen.list (Range.linear 0 16) (Gen.word8 Range.linearBounded)
  pure (ByteArrayLiteral (BA.byteArrayFromList bytes))
