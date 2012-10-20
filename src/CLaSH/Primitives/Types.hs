module CLaSH.Primitives.Types where

import Data.Aeson.TH        (deriveJSON)
import Data.ByteString.Lazy as LB (ByteString)
import Data.ByteString      as SB (ByteString)
import Data.HashMap.Lazy    (HashMap)

type PrimMap = HashMap LB.ByteString Primitive

data Primitive
  = BlackBox
  { name      :: LB.ByteString
  , inputs    :: Int
  , litInputs :: Int
  , template  :: SB.ByteString
  }
  | Primitive
  { name     :: LB.ByteString
  , primType :: PrimType
  } deriving Show

data PrimType = Function | Constructor | Dictionary | DFun
  deriving Show

$(fmap concat $ mapM (deriveJSON id) [''PrimType,''Primitive])
