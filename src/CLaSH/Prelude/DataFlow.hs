{-# LANGUAGE DataKinds #-}
module CLaSH.Prelude.DataFlow where

import Control.Applicative (pure)
import GHC.TypeLits        (KnownNat, KnownSymbol)

import CLaSH.Signal          ((.&&.))
import CLaSH.Signal.Bundle   (Bundle (..))
import CLaSH.Signal.Explicit (Clock (..),CSignal,SystemClock,sclock)

-- | Dataflow circuit with bidirectional synchronisation channels.
--
-- * @clk@ The clock to which the circuit is synchronised.
-- * @iEn@ Type of the incoming synchronisation channel.
-- * @oEn$ Type of the outgoing synchronisation channel.
-- * @i@ Incoming data type.
-- * @o@ Outgoing data type.
newtype DataFlow clk iEn oEn i o
  = DF
  { df :: CSignal clk (i,iEn)   -- ^ Incoming data, flagged with valid bits @iEn@.
       -> CSignal clk oEn       -- ^ Incoming back-pressure edge.
       -> ( CSignal clk (o,oEn) -- ^ Outgoing data, flagged with valid bits @oEn@.
          , CSignal clk iEn     -- ^ Outgoing back-pressure edge.
          )
  }

-- | Dataflow circuit synchronised to the 'SystemClock'.
type DataFlow' iEn oEn i o = DataFlow SystemClock iEn oEn i o

-- | Sequential composition of two 'DataFlow' circuits.
(>>>) :: DataFlow clk aEn bEn a b
      -> DataFlow clk bEn cEn b c
      -> DataFlow clk aEn cEn a c
(DF f) >>> (DF g) = DF (\a cEn -> let (b,aEn) = f a bEn
                                      (c,bEn) = g b cEn
                                  in  (c,aEn))

-- | Apply the circuit to the first halve of the communication channels, leave
-- the second halve unchanged.
first :: (KnownSymbol nm, KnownNat rate)
      => DataFlow (Clk nm rate) aEn bEn a b
      -> DataFlow (Clk nm rate) (aEn,cEn) (bEn,cEn) (a,c) (b,c)
first (DF f) = DF (\ac en -> let clk       = sclock
                                 (acD,acV) = unbundle clk ac
                                 (a,c)     = unbundle clk acD
                                 (aV,cV)   = unbundle clk acV
                                 (bRd,cRd) = unbundle clk en
                                 (b,aRd)   = f (bundle clk (a,aV)) bRd
                                 (bD,bV)   = unbundle clk b
                                 bcD       = bundle clk (bD,c)
                                 bcV       = bundle clk (bV,cV)
                                 dt        = bundle clk (bcD,bcV)
                                 rd        = bundle clk (aRd,cRd)
                             in  (dt,rd)

                  )

-- | Swap the two communication channels.
swap :: (KnownSymbol nm, KnownNat rate)
     => DataFlow (Clk nm rate) (aEn,bEn) (bEn,aEn) (a,b) (b,a)
swap = DF (\ab en -> let clk       = sclock
                         (abD,abV) = unbundle clk ab
                         (a,b)     = unbundle clk abD
                         (aV,bV)   = unbundle clk abV
                         (bEn,aEn) = unbundle clk en
                         baD       = bundle clk (b,a)
                         baV       = bundle clk (bV,aV)
                         ba        = bundle clk (baD,baV)
                         en'       = bundle clk (aEn,bEn)
                     in  (ba,en')
          )

-- | Apply the circuit to the second halve of the communication channels, leave
-- the first halve unchanged.
second :: (KnownSymbol nm, KnownNat rate)
       => DataFlow (Clk nm rate) aEn bEn a b
       -> DataFlow (Clk nm rate) (cEn,aEn) (cEn,bEn) (c,a) (c,b)
second f = swap >>> first f >>> swap

-- | Compose two 'DataFlow' circuits in parallel.
(***) :: (KnownSymbol nm, KnownNat rate)
      => DataFlow (Clk nm rate) aEn bEn a b
      -> DataFlow (Clk nm rate) cEn dEn c d
      -> DataFlow (Clk nm rate) (aEn,cEn) (bEn,dEn) (a,c) (b,d)
f *** g = first f >>> second g

-- | Feed back the second halve of the communication channel.
loop :: (KnownSymbol nm, KnownNat rate)
     => DataFlow (Clk nm rate) Bool Bool (a,d) (b,d)
     -> DataFlow (Clk nm rate) Bool Bool a     b
loop (DF f) = DF (\a bEn -> let clk       = sclock
                                (aD,aV)   = unbundle clk a
                                and1      = aV   .&&. and3
                                and2      = adEn .&&. and3
                                and3      = bdV  .&&. bEn
                                and4      = bdV  .&&. and6
                                and5      = bEn  .&&. and6
                                and6      = adEn .&&. aV
                                adD       = bundle clk (aD,dD)
                                ad        = bundle clk (adD,and1)
                                (bd,adEn) = f ad and5
                                (bdD,bdV) = unbundle clk bd
                                (bD,dD)   = unbundle clk bdD
                            in  (bundle clk (bD,and4),and2)
                 )

-- | Change the synchronisation granularity, but leave the data unchanged.
mapDF :: (KnownSymbol nm, KnownNat rate)
      => (ra -> rb)
      -> (rb -> ra)
      -> DataFlow (Clk nm rate) ra rb a a
mapDF f g = DF (\din rb -> let clk    = sclock
                               (d,ra) = unbundle clk din
                               rb'    = fmap f ra
                               ra'    = fmap g rb
                           in  (bundle clk (d,rb'),ra')
               )
