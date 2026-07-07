{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Clash.Signal.Simulation.DataType where

{-

Module to handle data types in a dynamic way, where some coercions are possible

Basic data type representation is:

```
data DataType = DT{type::String, subtypes::[DataType]}
```

and is derived from the TypeRep.

Some examples of compatible types:

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
import Type.Reflection
import GHC.Generics
import Data.Aeson
import Data.Typeable
import Data.List (stripPrefix)


data DataType = DT String [DataType] deriving (Show,Generic,ToJSON,FromJSON)

-- | Get a @DataType@ representation of a type.
-- This amounts to the toplevel type, and a list of all its type arguments.
-- Note that this does not directly specify anything about the data type.
typeRep :: forall a. Typeable a => DataType
typeRep = pretty $ Data.Typeable.typeRep $ Proxy @a
 where
  pretty :: SomeTypeRep -> DataType
  pretty (SomeTypeRep tr) =
    case splitApps tr of
      (tc, args) ->
        DT (name tc) (map pretty args)
   where
    name tc = tyConModule tc ++ "$" ++ tyConName tc

-- #define LABEL "Clash.Data.AnonRecord$(:=)"
-- #define JOIN "Clash.Data.AnonRecord$(:&:)"
-- #define UNIT "GHC.Tuple$Unit"
-- #define TUP2 "GHC.Tuple$Tuple2"
-- #define TUP_BASE "GHC.Tuple$Tuple"

pattern LABEL = "Clash.Data.AnonRecords$(:=)"
pattern JOIN = "Clash.Data.AnonRecord$(:&:)"
pattern UNIT = "GHC.Tuple$Unit"
pattern TUP2 = "GHC.Tuple$Tuple2"
pattern TUP_BASE = "GHC.Tuple$Tuple"
pattern SYMBOL_BASE = "GHC.DataKind$"

-- | Check whether two data types are compatible.
-- This occurs when both types are the same,
-- or when they have a compatible tuple/anonymous record structure.
-- For example, @(a,(b,c)) ~ (a,b,c) ~ (a,(b,(c,()))) ~ (L a :&: L b :&: L c)@.
compatible :: DataType -> DataType -> Bool
compatible (DT a     ra   ) (DT b     rb    ) | a == b
                                              = and $ zipWith compatible ra rb
compatible (DT LABEL _    ) (DT LABEL _     ) = False -- cut off before removing labels
compatible (DT TUP2 [a,b] ) (DT TUP2 [c,d]  ) = compatible a c && compatible b d
compatible x'               y'                = compatible' x' y' || compatible' y' x' -- asymmetric reductions
 where
  -- a:&:b ~ (a,b)
  compatible' (DT JOIN [a,b] ) y = compatible (DT TUP2 [a,b]) y
  -- (a,()) ~ a
  compatible' (DT TUP2 [a,DT UNIT []]) b = compatible a b
  -- (a,b,c,d) ~ (a,(b,c,d))
  compatible' (DT tup (a0:args)) y | Just n' <- stripPrefix TUP_BASE tup, n <- read n', n>2 =
    let tupN = TUP_BASE <> show (n - 1 :: Int)
    in compatible (DT TUP2 [a0, DT tupN args]) y
  
  -- l := a ~ a
  compatible' (DT LABEL [_,a]) b = compatible a b

  compatible' _ _ = False


-- | Turn a list of fields and their types into an anonymous record.
-- @
-- [("a",DT "Bool" []),("b",DT "Int" [])] ~ (a := Bool :&: b := Int)
-- @
constructRecord :: [(String,DataType)] -> DataType
constructRecord [] = DT UNIT []
constructRecord [(field,ty)] = constructLabel field ty
constructRecord ((field,ty):rest) = DT JOIN [constructLabel field ty, constructRecord rest]

-- | Create an anonymous record label from the type.
constructLabel :: String -> DataType -> DataType
constructLabel l t = DT LABEL [DT (SYMBOL_BASE<>show l) [],t]
