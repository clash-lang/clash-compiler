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
import Data.Aeson
import Data.List (stripPrefix)
import Data.Typeable
import GHC.Generics
import Type.Reflection
import Data.Char (isAlphaNum)

data DataType = DT String [DataType] deriving (Show,Generic,ToJSON,FromJSON)

-- | Get a @DataType@ representation of a type.
-- This amounts to the toplevel type, and a list of all its type arguments.
-- Note that this does not directly specify anything about the data type.
simpleTypeRep :: forall a. Typeable a => DataType
simpleTypeRep = pretty $ Data.Typeable.typeRep $ Proxy @a
 where
  pretty :: SomeTypeRep -> DataType
  pretty (SomeTypeRep tr) =
    case splitApps tr of
      (tc, args) ->
        DT (name tc) (map pretty args)
   where
    name tc = tyConModule tc <> "." <>
      (if isAlphaNum c || c `elem` "'\"" then tcn else "(" <> tcn <> ")")
     where
      tcn = tyConName tc
      c = tcn !! 0

simpleTyConName :: forall a. Typeable a -> String
simpleTyConName = let DT n _ = simpleTypeRep @a in n

pattern LABEL       = $(litE $ StringL $              simpleTyConName @(:=)   )
pattern JOIN        = $(litE $ StringL $              simpleTyConName @(:&:)  )
pattern UNIT        = $(litE $ StringL $              simpleTyConName @()     )
pattern TUP2        = $(litE $ StringL $              simpleTyConName @((),()))
pattern TUP_BASE    = $(litE $ StringL $ init       $ simpleTyConName @((),()))
pattern SYMBOL_BASE = $(litE $ StringL $ init .init $ simpleTyConName @""     )

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
