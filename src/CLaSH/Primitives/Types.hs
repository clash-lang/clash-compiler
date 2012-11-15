{-# LANGUAGE DeriveDataTypeable #-}
module CLaSH.Primitives.Types where

import Data.Aeson.TH        (deriveJSON)
import Data.ByteString.Lazy as LB (ByteString)
import Data.ByteString      as SB (ByteString)
import Data.Data
import Data.HashMap.Lazy    (HashMap)

type PrimMap = HashMap LB.ByteString Primitive

data Primitive
  = BlackBox
  { name      :: LB.ByteString
  , inputs    :: Int
  , litInputs :: Int
  , funInputs :: Int
  , template  :: SB.ByteString
  , templateI :: SB.ByteString
  }
  | Primitive
  { name     :: LB.ByteString
  , primType :: PrimType
  } deriving (Typeable, Data, Show)

data PrimType = Function | Constructor | Dictionary | DFun
  deriving (Typeable, Data, Show)

$(fmap concat $ mapM (deriveJSON id) [''PrimType,''Primitive])
