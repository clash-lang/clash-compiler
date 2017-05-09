module LambdaDrop where

import CLaSH.Prelude

topEntity :: Signal (BitVector 2)
topEntity = (++#) <$> outport1 <*> outport2
  where
    (outport1, outResp1) = gpio (decodeReq 1 req)
    (outport2, outResp2) = gpio (decodeReq 2 req)
    ramResp = ram (decodeReq 0 req)

    req = core $ (<|>) <$> ramResp <*> ((<|>) <$> outResp1 <*> outResp2)

core :: Signal (Maybe Bit) -> Signal Bit
core = fmap (maybe low id)
{-# NOINLINE core #-}

ram :: Signal Bit -> Signal (Maybe Bit)
ram = fmap pure
{-# NOINLINE ram #-}

decodeReq :: Integer -> Signal Bit -> Signal Bit
decodeReq 0 = fmap (const low)
decodeReq 1 = id
decodeReq _ = fmap complement
{-# NOINLINE decodeReq #-}

gpio :: Signal Bit -> (Signal Bit,Signal (Maybe Bit))
gpio i = (i,pure <$> i)
{-# NOINLINE gpio #-}
