{-
Module for comparing (binary) compatibility between different types.

Some examples:

```
(a,(b,(c,d))) ~ (a,b,c,d)
(a,(b,(c,()))) ~ (a,b,c)
("x":a,"y":b) ~ (a,b)
```

aka:
a = a
(a,b) = (c,d) if a=c, b=d
    as well as other tuple sizes
Label x a = a
(a,b) = a if b=()
(a,b) = b if a=()

and maybe

Vec 3 a = (a,a,a)
    and other sizes
Nil = ()

This cannot be done through a coercible class, as the values exist in runtime only.

-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Signal.Internal.Trace.CompatibleTypes (
  module Clash.Signal.Internal.Trace.DataType,
  compatible,
  constructRecord, constructLabel
) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

import Data.AnonRecords
import Clash.Signal.Internal.Trace.DataType

pattern LABEL       :: String
pattern LABEL       = $(litP $ StringL $              simpleTyConName @(:=)   )
pattern JOIN        :: String
pattern JOIN        = $(litP $ StringL $              simpleTyConName @(:&:)  )
pattern UNIT        :: String
pattern UNIT        = $(litP $ StringL $              simpleTyConName @()     )
--pattern TUP2        :: String
--pattern TUP2        = $(litP $ StringL $              simpleTyConName @((),()))
pattern TUP_BASE    :: String
pattern TUP_BASE    = $(litP $ StringL $ init       $ simpleTyConName @((),()))
pattern SYMBOL_BASE :: String
pattern SYMBOL_BASE = $(litP $ StringL $ init .init $ simpleTyConName @""     )

-- | Check whether two data types are compatible.
-- This occurs when both types are the same,
-- or when they have a compatible tuple/anonymous record structure.
--
-- The following rules apply:
-- * Nested tuple structure are flattened.
-- * Unit types ('()') are removed.
-- * Labeled unit types are also removed (including structures that reduce to unit).
-- * Two labels must have the same field name, but labeled data can match unlabeled data.
--
-- For example:
-- * @(a,(b,c),())@ is compatible with @((a,b),c)@.
-- * @(a:=x :&: b:=() :&: c:=y)@ is compatible with @(x,y)@ or even @(x,c:=y)@.
-- * @"a":=x@ and @"_a":=x@ are not compatible.
--
-- This function is aimed at marking types with the same @BitPack@ structure.
-- Custom bit packings of tuples/anonymous records may result in erroneous compatibility approval.
compatible :: DataType -> DataType -> Bool
compatible (DT a     ra   ) (DT b     rb   ) | a == b
                                             = and $ zipWith compatible ra rb
compatible (DT LABEL _    ) (DT LABEL _    ) = False -- Different labels.
compatible (DT LABEL [_,a]) b                = compatible a b -- If only one side has a label,
compatible a                (DT LABEL [_,b]) = compatible a b -- ignore it.
compatible a                b = case (flattenDTM a, flattenDTM b) of
  (Nothing, Nothing) -> False -- Nothing changed by flattening
  (a0, b0) -> compatibles (fromMaybe [a] a0) (fromMaybe [b] b0)
 where
  flattenDTM :: DataType -> Maybe [DataType]
  flattenDTM (DT JOIN [x,y] ) = Just (flattenDT x <> flattenDT y)
  flattenDTM (DT tup args   ) | Just _ <- stripPrefix TUP_BASE tup
                              = Just (concatMap flattenDT args)
  flattenDTM (DT UNIT _     ) = Just [] -- Removed unit types (0-tuples)
  flattenDTM (DT LABEL [_,x]) = case flattenDT x of
                                 [] -> Just [] -- Labeled units are also empty.
                                 _  -> Nothing
  flattenDTM _                = Nothing

  flattenDT :: DataType -> [DataType]
  flattenDT x = case flattenDTM x of
    Just xs -> xs
    _       -> [x]

  compatibles as bs = length as == length bs && (and $ zipWith compatible as bs)

-- | Turn a list of fields and their types into an anonymous record.
-- @
-- [("a",DT "Bool" []),("b",DT "Int" [])] ~ (a := Bool :&: b := Int)
-- @
constructRecord :: [(String,DataType)] -> DataType
constructRecord [] = DT UNIT []
constructRecord [(field,ty)] = constructLabel field ty
constructRecord ((field,ty):rest) =
  DT JOIN [constructLabel field ty, constructRecord rest]

-- | Create an anonymous record label from the type.
constructLabel :: String -> DataType -> DataType
constructLabel l t = DT LABEL [DT (SYMBOL_BASE<>show l) [],t]
