{-# LANGUAGE CPP #-}

module LambdaDrop where

import Clash.Prelude

topEntity :: Signal System (BitVector 2)
topEntity = (++#) <$> (pack <$> outport1) <*> (pack <$> outport2)
  where
    (outport1, outResp1) = gpio (decodeReq 1 req)
    (outport2, outResp2) = gpio (decodeReq 2 req)
    ramResp = ram (decodeReq 0 req)

    req = core $ (<|>) <$> ramResp <*> ((<|>) <$> outResp1 <*> outResp2)

core :: Signal dom (Maybe Bit) -> Signal dom Bit
core = fmap (maybe low id)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE core #-}

ram :: Signal dom Bit -> Signal dom (Maybe Bit)
ram = fmap pure
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE ram #-}

decodeReq :: Integer -> Signal dom Bit -> Signal dom Bit
decodeReq 0 = fmap (const low)
decodeReq 1 = id
decodeReq _ = fmap complement
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE decodeReq #-}

gpio :: Signal dom Bit -> (Signal dom Bit,Signal dom (Maybe Bit))
gpio i = (i,pure <$> i)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE gpio #-}
