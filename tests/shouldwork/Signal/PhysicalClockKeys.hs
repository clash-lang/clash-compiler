module PhysicalClockKeys where

import Clash.Explicit.Prelude
import qualified Clash.Driver.Manifest as Manifest
import Data.Kind (Type)
import qualified Data.Text as Text
import qualified Prelude as P
import System.Environment (getArgs)
import System.FilePath ((</>))
import Type.Reflection (Typeable)

data Key (t :: Type)
data One
newtype Two = Two One

instance Typeable t => KnownClockDomain (Key t) where
  type ClockPeriod (Key t) = 10000
  type ClockActiveEdge (Key t) = 'Rising
  type ClockResetKind (Key t) = 'Asynchronous
  type ClockInitBehavior (Key t) = 'Defined
  type ClockResetPolarity (Key t) = 'ActiveHigh

type instance ClockDomainOf (Key t) = Key t
instance Typeable t => KnownDomain (Key t)

topEntity :: Clock (Key One) -> Clock (Key Two) -> Bool -> Bool
topEntity _ _ = not

mainVerilog :: IO ()
mainVerilog = do
  [root] <- getArgs
  Just manifest <- Manifest.readManifest (root </> show 'topEntity </> Manifest.manifestFilename)
  let clocks = [key | p <- Manifest.ports manifest, Manifest.mpIsClock p,
                      Just key <- [Manifest.mpDomain p]]
      names = P.map (Text.pack . vName) (P.foldr (:) [] (Manifest.domains manifest))
  case clocks of
    [a, b] | a /= b && a `P.elem` names && b `P.elem` names -> pure ()
    _ -> fail "Expected two distinct physical clock domains"
