{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
module CLaSH.Prelude.DataFlow where

import Control.Applicative   (Applicative (..), (<$>))
import Control.Arrow         (Arrow (..), ArrowChoice (..), ArrowLoop (..))
import Control.Category      (Category (..))
import GHC.TypeLits          (KnownNat, KnownSymbol)
import Prelude               hiding (id, (.))

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
seqDF :: DataFlow clk aEn bEn a b
      -> DataFlow clk bEn cEn b c
      -> DataFlow clk aEn cEn a c
(DF f) `seqDF` (DF g) = DF (\a cEn -> let (b,aEn) = f a bEn
                                          (c,bEn) = g b cEn
                                      in  (c,aEn))

-- | Apply the circuit to the first halve of the communication channels, leave
-- the second halve unchanged.
firstDF :: (KnownSymbol nm, KnownNat rate)
      => DataFlow (Clk nm rate) aEn bEn a b
      -> DataFlow (Clk nm rate) (aEn,cEn) (bEn,cEn) (a,c) (b,c)
firstDF (DF f) = DF (\ac en -> let clk       = sclock
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
swapDF :: (KnownSymbol nm, KnownNat rate)
       => DataFlow (Clk nm rate) (aEn,bEn) (bEn,aEn) (a,b) (b,a)
swapDF = DF (\ab en -> let clk       = sclock
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
secondDF :: (KnownSymbol nm, KnownNat rate)
         => DataFlow (Clk nm rate) aEn bEn a b
         -> DataFlow (Clk nm rate) (cEn,aEn) (cEn,bEn) (c,a) (c,b)
secondDF f = swapDF `seqDF` firstDF f `seqDF` swapDF

-- | Compose two 'DataFlow' circuits in parallel.
parDF :: (KnownSymbol nm, KnownNat rate)
      => DataFlow (Clk nm rate) aEn bEn a b
      -> DataFlow (Clk nm rate) cEn dEn c d
      -> DataFlow (Clk nm rate) (aEn,cEn) (bEn,dEn) (a,c) (b,d)
f `parDF` g = firstDF f `seqDF` secondDF g

-- | Feed back the second halve of the communication channel.
loopDF :: (KnownSymbol nm, KnownNat rate)
       => DataFlow (Clk nm rate) Bool Bool (a,d) (b,d)
       -> DataFlow (Clk nm rate) Bool Bool a     b
loopDF (DF f) = DF (\a bEn -> let clk       = sclock
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

instance Category (DataFlow clk Bool Bool) where
  id              = DF (\d r -> (d,r))
  (DF g) . (DF f) = DF (\a cR -> let (b,aR) = f a bR
                                     (c,bR) = g b cR
                                 in  (c,aR)
                       )

instance (KnownSymbol nm, KnownNat rate) =>
  Arrow (DataFlow (Clk nm rate) Bool Bool) where
  arr f        = DF (\a r -> (fmap (first f) a,r))
  first (DF f) = DF (\bd r -> let clk     = sclock
                                  (bdD,v) = unbundle clk bd
                                  (b,d)   = unbundle clk bdD
                                  (c,bR)  = f (bundle clk (b,v)) r
                                  (cD,cV) = unbundle clk c
                                  cdD     = bundle clk (cD,d)
                                  v'      = cV .&&. v
                                  cd      = bundle clk (cdD,v')
                                  r'      = bR .&&. r
                              in  (cd,r')
                    )

instance (KnownSymbol nm, KnownNat rate) =>
  ArrowLoop (DataFlow (Clk nm rate) Bool Bool) where
  loop = loopDF

instance (KnownSymbol nm, KnownNat rate) =>
  ArrowChoice (DataFlow (Clk nm rate) Bool Bool) where
  left (DF f) =
    DF (\ebd r -> let clk              = sclock
                      (c,fR)           = f b rIn
                      (ecd,rOut,b,rIn) = unbundle clk (route <$> ebd <*> r
                                                             <*> c   <*> fR)
                  in  (ecd,rOut)
        )
    where
      route (Left b,v)  r (c,cV) bR = ((Left c,cV),bR,(b,v),r)
      route (Right d,v) r _      _  = ((Right d,v),r ,(undefined,False),False)
