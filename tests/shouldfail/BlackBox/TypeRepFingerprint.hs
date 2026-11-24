module TypeRepFingerprint where

import Clash.Explicit.Prelude
import Data.Word (Word64)
import GHC.Fingerprint.Type (Fingerprint(..))
import Type.Reflection (typeRep)
import Type.Reflection.Unsafe (typeRepFingerprint)

-- Both words of the fingerprint are observable at the output port.
topEntity :: Bool -> Word64
topEntity x = if x then a else b
 where
  Fingerprint a b = typeRepFingerprint (typeRep @Bool)
