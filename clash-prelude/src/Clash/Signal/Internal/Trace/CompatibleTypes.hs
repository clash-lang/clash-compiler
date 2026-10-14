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

module Clash.Signal.Internal.Trace.CompatibleTypes (
  module Clash.Signal.Internal.Trace.DataType,
  compatible,
  constructRecord, constructLabel
) where

import Data.List (stripPrefix)
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
pattern TUP2        :: String
pattern TUP2        = $(litP $ StringL $              simpleTyConName @((),()))
pattern TUP_BASE    :: String
pattern TUP_BASE    = $(litP $ StringL $ init       $ simpleTyConName @((),()))
pattern SYMBOL_BASE :: String
pattern SYMBOL_BASE = $(litP $ StringL $ init .init $ simpleTyConName @""     )

-- | Check whether two data types are compatible.
-- This occurs when both types are the same,
-- or when they have a compatible tuple/anonymous record structure.
-- For example, @(a,(b,c)) ~ (a,b,c) ~ (a,(b,(c,()))) ~ (L a :&: L b :&: L c)@.
-- Records must have the same field labels.
compatible :: DataType -> DataType -> Bool
compatible (DT a     ra   ) (DT b     rb    ) | a == b
                                              = and $ zipWith compatible ra rb
compatible (DT LABEL _    ) (DT LABEL _     ) = False -- cut off before removing labels
compatible (DT TUP2 [a,b] ) (DT TUP2 [c,d]  ) = compatible a c && compatible b d
compatible x0               y0                = compatible0 x0 y0 || compatible0 y0 x0
                                                -- ^ asymmetric reductions
 where
  -- a:&:b ~ (a,b)
  compatible0 (DT JOIN [a,b] ) y = compatible (DT TUP2 [a,b]) y
  -- (a,()) ~ a
  compatible0 (DT TUP2 [a,DT UNIT []]) b = compatible a b
  -- (a,b,c,d) ~ (a,(b,c,d))
  compatible0 (DT tup (a0:args)) y | Just n0 <- stripPrefix TUP_BASE tup
                                   , n <- read n0
                                   , n>2 =
    let tupSmaller = TUP_BASE <> show (n - 1 :: Int)
    in compatible (DT TUP2 [a0, DT tupSmaller args]) y

  -- l := a ~ a
  compatible0 (DT LABEL [_,a]) b = compatible a b

  compatible0 _ _ = False


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
