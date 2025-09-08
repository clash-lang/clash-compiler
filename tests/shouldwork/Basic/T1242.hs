{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module T1242 where

import           Clash.Prelude
import           Control.DeepSeq   (NFData)
import           GHC.Generics      (Generic)

data RepKind = AppRep | WireRep
  deriving (Generic, Show, Eq, NFData, NFDataX)

newtype U16 (t :: RepKind) = U16 (Unsigned 16)

deriving instance Generic (U16 a)
deriving instance Eq (U16 a)
deriving instance Show (U16 a)
deriving instance NFData (U16 a)
deriving instance NFDataX (U16 a)
deriving instance BitPack (U16 'AppRep)

class WireApp w a where
    toWire :: a -> w
    toApp :: w -> a

instance WireApp (U16 'WireRep) (U16 'AppRep) where
    toWire v = case v of U16 x -> U16 x
    toApp v = case v of U16 x -> U16 x

instance BitPack (U16 'WireRep) where
  type BitSize (U16 'WireRep) = BitSize (U16 'AppRep)
  pack x = bv
    where
        bv :: BitVector (BitSize (U16 'WireRep))
        bv = pack app
        app :: U16 'AppRep
        app = toApp $ x
  unpack x = toWire app
    where
        app :: U16 'AppRep
        app = unpack x

data Record
  = Record
  { f1 :: U16 'WireRep
  , f2 :: U16 'WireRep
  } deriving (Generic, NFData, Show, Eq, BitPack, NFDataX)

topEntity :: Signal System Bool -> Signal System Bool
topEntity _ = pure (unpack 0 == Record (U16 0) (U16 0))
