{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
module CLaSH.Prelude.DataFlow.LockStep where

import Control.Applicative   (Applicative (..), (<$>))
import Control.Arrow         (Arrow (..), ArrowChoice (..), ArrowLoop (..))
import Control.Category      (Category (..))
import GHC.TypeLits          (KnownNat, KnownSymbol)
import Prelude hiding        (id,(.))

import CLaSH.Signal          ((.&&.))
import CLaSH.Signal.Bundle   (Bundle (..))
import CLaSH.Signal.Explicit (Clock (..), CSignal,SystemClock,sclock)

-- | Dataflow circuit with bidirectional synchronisation channels.
--
-- Parallel compositions operate in lock-step.
newtype DataFlowLS clk i o
  = DFL
  { dfl :: CSignal clk (i,Bool)
        -> CSignal clk Bool
        -> ( CSignal clk (o,Bool)
           , CSignal clk Bool)
  }

type DataFlowLS' a b = DataFlowLS SystemClock

instance Category (DataFlowLS clk) where
  id                = DFL (\d r -> (d,r))
  (DFL g) . (DFL f) = DFL (\a cR -> let (b,aR) = f a bR
                                        (c,bR) = g b cR
                                    in  (c,aR)
                          )

instance (KnownSymbol nm, KnownNat rate) =>
  Arrow (DataFlowLS (Clk nm rate)) where
  arr f         = DFL (\a r -> (fmap (first f) a,r))
  first (DFL f) = DFL (\bd r -> let clk     = sclock
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
  ArrowLoop (DataFlowLS (Clk nm rate)) where
  loop (DFL f) = DFL (\a bEn -> let clk       = sclock
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

instance (KnownSymbol nm, KnownNat rate) =>
  ArrowChoice (DataFlowLS (Clk nm rate)) where
  left (DFL f) =
    DFL (\ebd r -> let clk              = sclock
                       (c,fR)           = f b rIn
                       (ecd,rOut,b,rIn) = unbundle clk (route <$> ebd <*> r
                                                              <*> c   <*> fR)
                   in  (ecd,rOut)
        )
    where
      route (Left b,v)  r (c,cV) bR = ((Left c,cV),bR,(b,v),r)
      route (Right d,v) r _      _  = ((Right d,v),r ,(undefined,False),False)
