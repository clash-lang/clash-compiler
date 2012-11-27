{-# LANGUAGE DeriveDataTypeable #-}
module CLaSH.Primitives.Types where

import Data.Aeson.TH        (deriveJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy       (Text)
import Data.Data
import Data.HashMap.Lazy    (HashMap)

type PrimMap = HashMap ByteString Primitive

data Primitive
  = BlackBox
  { name      :: ByteString
  , template  :: Text
  , templateI :: Text
  }
  | Primitive
  { name     :: ByteString
  , primType :: PrimType
  } deriving (Typeable, Data, Show)

data PrimType = Function | Constructor | Dictionary | DFun
  deriving (Typeable, Data, Show)

$(fmap concat $ mapM (deriveJSON id) [''PrimType,''Primitive])
