{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}

module PhysicalClockKeys where

import Clash.Explicit.Prelude
import Data.Kind (Type)
import qualified Data.Proxy as Proxy
import Type.Reflection (Typeable)

#ifdef OUTPUTTEST
import qualified Clash.Driver.Manifest as Manifest
import Data.List (nub)
import qualified Data.Text as Text
import qualified Prelude as P
import System.Environment (getArgs)
import System.FilePath ((</>))
#endif

data Key (t :: k)
data One
newtype Two = Two One
data Proxy a
data family Family (a :: Type)
data instance Family Bool

instance (Typeable k, Typeable (t :: k)) => KnownClockDomain (Key t) where
  type ClockPeriod (Key t) = 10000
  type ClockActiveEdge (Key t) = 'Rising
  type ClockResetKind (Key t) = 'Asynchronous
  type ClockInitBehavior (Key t) = 'Defined
  type ClockResetPolarity (Key t) = 'ActiveHigh

type instance ClockDomainOf (Key t) = Key t
instance (Typeable k, Typeable (t :: k)) => KnownDomain (Key t)

instance KnownClockDomain (Family Bool) where
  type ClockPeriod (Family Bool) = 10000
  type ClockActiveEdge (Family Bool) = 'Rising
  type ClockResetKind (Family Bool) = 'Asynchronous
  type ClockInitBehavior (Family Bool) = 'Defined
  type ClockResetPolarity (Family Bool) = 'ActiveHigh

type instance ClockDomainOf (Family Bool) = Family Bool
instance KnownDomain (Family Bool)

data Tagged a
type instance ClockDomainOf (Tagged a) = a
instance KnownClockDomain a => KnownDomain (Tagged a)

topEntity
  :: Clock (Key One) -> Clock (Key Two)
  -> Clock (Key (Proxy Bool)) -> Clock (Key (Proxy.Proxy Bool))
  -> Clock (Key "x\ny") -> Clock (Key "x\\ny")
  -> Clock (Key 42) -> Clock (Key "42") -> Clock (Key 'x')
  -> Clock (Key 'True) -> Clock (Key ('Just 'True))
  -> Clock (Key (Bool -> One)) -> Clock (Key Maybe)
  -> Clock (Key (Proxy.Proxy :: Type -> Type)) -> Clock (Key (Proxy.Proxy :: Bool -> Type))
  -> Clock (Key (Family Bool)) -> Clock (Family Bool) -> Clock (Tagged (Key One))
  -> Bool -> Bool
topEntity _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = not

#ifdef OUTPUTTEST
mainVerilog :: IO ()
mainVerilog = do
  [root] <- getArgs
  Just manifest <- Manifest.readManifest (root </> show 'topEntity </> Manifest.manifestFilename)
  let clocks = [key | p <- Manifest.ports manifest, Manifest.mpIsClock p,
                      Just key <- [Manifest.mpDomain p]]
      names = P.map (Text.pack . vName) (P.foldr (:) [] (Manifest.domains manifest))
      expected = P.map (Text.pack . vName)
        [ knownVClockDomain @(Key One), knownVClockDomain @(Key Two)
        , knownVClockDomain @(Key (Proxy Bool)), knownVClockDomain @(Key (Proxy.Proxy Bool))
        , knownVClockDomain @(Key "x\ny"), knownVClockDomain @(Key "x\\ny")
        , knownVClockDomain @(Key 42), knownVClockDomain @(Key "42"), knownVClockDomain @(Key 'x')
        , knownVClockDomain @(Key 'True), knownVClockDomain @(Key ('Just 'True))
        , knownVClockDomain @(Key (Bool -> One)), knownVClockDomain @(Key Maybe)
        , knownVClockDomain @(Key (Proxy.Proxy :: Type -> Type)), knownVClockDomain @(Key (Proxy.Proxy :: Bool -> Type))
        , knownVClockDomain @(Key (Family Bool)), knownVClockDomain @(Family Bool)
        , knownVClockDomain @(Tagged (Key One))
        ]
  if clocks == expected && P.all (`P.elem` names) expected
    then pure ()
    else fail ("Runtime and manifest clock names differ:\n" <> show (P.zip expected clocks))
  if P.length (nub expected) == P.length expected - 1 && P.head expected == P.last expected
    then pure ()
    else fail "Distinct physical markers must have distinct names; a logical wrapper must share its physical clock's name"
#endif
