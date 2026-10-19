{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Signal.Internal.Trace.DataType (
  DataType(..),
  simpleTypeRep,
  simpleTyConName,
) where

{-

Module to handle data types in a dynamic way, where some coercions are possible

Basic data type representation is:

```
data DataType = DT{type::String, subtypes::[DataType]}
```

and is derived from the TypeRep.


See 'Clash.Signal.Internal.Trace.CompatibleTypes' for more.

-}

import Data.Aeson
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

-- | Get the type constructor name from a type
simpleTyConName :: forall a. Typeable a => String
simpleTyConName = let DT n _ = simpleTypeRep @a in n
