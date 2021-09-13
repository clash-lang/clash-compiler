{-# LANGUAGE CPP
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , LambdaCase
           , AllowAmbiguousTypes
           , ApplicativeDo
           , StandaloneDeriving #-}

module T1921 where

import Clash.Prelude
import Control.Lens
import Data.Default
#if MIN_VERSION_singletons(3,0,0)
import Prelude.Singletons
import GHC.TypeLits.Singletons as TL
#else
import Data.Singletons.Prelude
import Data.Singletons.TypeLits as TL
#endif

topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Unsigned 8)
topEntity = exposeClockResetEnable fibonacciLFSR8

-- Straightforward newtype
newtype LFSRState n = LFSRState { runLFSRState :: BitVector n }
  deriving newtype (NFDataX, AutoReg)
instance KnownNat n => Default (LFSRState n) where
  def = LFSRState (fromIntegral 1)

fibonacciLFSR8 :: HiddenClockResetEnable dom => Signal dom (Unsigned 8)
fibonacciLFSR8 = fibonacciLFSRType @('[3,4,5,7]) @8

fibonacciLFSRType
  :: forall (taps :: [Nat]) (n :: Nat) dom
   . SingI taps
  => KnownNat n
  => HiddenClockResetEnable dom
  => Signal dom (Unsigned n)
fibonacciLFSRType =
  let lfsr = autoReg def (LFSRState <$> lfsr')
      lfsr' = do lfsrState <- lfsr
                 -- shift the bit register by one, and then replace
                 -- the bit on the end by xor of the taps (via go)
                 return $
                   shiftL (runLFSRState lfsrState) 1
                   & ix 0
                   .~ go lfsrState (sing :: Sing taps)
  in unpack <$> lfsr'

  where
    go :: forall (n :: Nat) (indices :: [Nat])
        . SingI indices
       => KnownNat n
       => LFSRState n -> SList indices -> Bit
    go b@(LFSRState bs) = \case
      -- If there is only one tap left, return that bit
      (SCons a SNil) -> withKnownNat a
                        $ bs ^?! ix (fromIntegral $ TL.natVal a)
      -- XOR a tapped bit with the remaining taps
      (SCons a as) -> withSingI as $ withKnownNat a
                        $ xor (bs ^?! ix (fromIntegral $ TL.natVal a)) (go b as)
      -- This should never happen (we can't have no taps)
      SNil -> error "A no-tap LFSR is ill-defined"
