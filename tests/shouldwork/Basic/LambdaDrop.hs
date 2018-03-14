module LambdaDrop where

import Clash.Prelude

topEntity :: Signal System (BitVector 2)
topEntity = (++#) <$> (pack <$> outport1) <*> (pack <$> outport2)
  where
    (outport1, outResp1) = gpio (decodeReq 1 req)
    (outport2, outResp2) = gpio (decodeReq 2 req)
    ramResp = ram (decodeReq 0 req)

    req = core $ (<|>) <$> ramResp <*> ((<|>) <$> outResp1 <*> outResp2)

core :: Signal domain (Maybe Bit) -> Signal domain Bit
core = fmap (maybe low id)
{-# NOINLINE core #-}

ram :: Signal domain Bit -> Signal domain (Maybe Bit)
ram = fmap pure
{-# NOINLINE ram #-}

decodeReq :: Integer -> Signal domain Bit -> Signal domain Bit
decodeReq 0 = fmap (const low)
decodeReq 1 = id
decodeReq _ = fmap complement
{-# NOINLINE decodeReq #-}

gpio :: Signal domain Bit -> (Signal domain Bit,Signal domain (Maybe Bit))
gpio i = (i,pure <$> i)
{-# NOINLINE gpio #-}
