{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}

module Clash.Sized.Internal.CheckedLiterals where

import Data.Kind (Type)
import GHC.TypeError
  ( ErrorMessage (ShowType, Text, (:$$:), (:<>:))
  , TypeError
  )

type CheckedLiteralUncheckedFix =
  'Text "Possible fix: use 'uncheckedLiteral' from 'CheckedLiterals' to bypass this check."

type AddConstraintFix required actual =
  'Text "Possible fix: add a constraint: "
    ':<>: 'ShowType required
    ':<>: 'Text " <= "
    ':<>: 'ShowType actual
    ':<>: 'Text "."

type UnsignedBounds (typ :: Type) maxVal =
  'ShowType typ
    ':<>: 'Text " has bounds: [0 .. "
    ':<>: 'ShowType maxVal
    ':<>: 'Text "]."

type SignedBounds (typ :: Type) minVal maxVal =
  'ShowType typ
    ':<>: 'Text " has bounds: [-"
    ':<>: 'ShowType minVal
    ':<>: 'Text " .. "
    ':<>: 'ShowType maxVal
    ':<>: 'Text "]."

type IntegerBitsNote bits =
  'Text "Note: integer part needs at least "
    ':<>: 'ShowType bits
    ':<>: 'Text " bit(s)."

type SignedIntegerBitsNote bits =
  'Text "Note: integer part needs at least "
    ':<>: 'ShowType bits
    ':<>: 'Text " bit(s), including sign bit."

type FractionalBitsNote bits =
  'Text "The fractional part needs at least "
    ':<>: 'ShowType bits
    ':<>: 'Text " bit(s)."

type OutOfBounds strLit details =
  TypeError
    ( 'Text "Literal "
        ':<>: strLit
        ':<>: 'Text " is out of bounds."
        ':$$: details
        ':$$: CheckedLiteralUncheckedFix
    )

type PotentiallyOutOfBounds strLit details required actual =
  TypeError
    ( 'Text "Literal "
        ':<>: strLit
        ':<>: 'Text " is (potentially) out of bounds."
        ':$$: details
        ':$$: AddConstraintFix required actual
        ':$$: CheckedLiteralUncheckedFix
    )

type OutOfBoundsBecause strLit reason =
  TypeError
    ( 'Text "Literal "
        ':<>: strLit
        ':<>: 'Text " is out of bounds, because "
        ':<>: reason
        ':$$: CheckedLiteralUncheckedFix
    )

type NotExactlyRepresentable strLit (typ :: Type) reason =
  TypeError
    ( 'Text "Literal "
        ':<>: strLit
        ':<>: 'Text " cannot be represented exactly by "
        ':<>: 'ShowType typ
        ':<>: 'Text "."
        ':$$: reason
        ':$$: CheckedLiteralUncheckedFix
    )

type NotExactlyRepresentableWithConstraint strLit (typ :: Type) reason required actual =
  TypeError
    ( 'Text "Literal "
        ':<>: strLit
        ':<>: 'Text " cannot be represented exactly by "
        ':<>: 'ShowType typ
        ':<>: 'Text "."
        ':$$: reason
        ':$$: AddConstraintFix required actual
        ':$$: CheckedLiteralUncheckedFix
    )
