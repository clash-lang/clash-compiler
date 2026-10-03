-- | Applicative style code over vectors creates vectors of functions, which
-- Clash cannot represent in HDL. Clash unrolls these by reducing the 'map's and
-- 'zipWith's one element at a time. It used to reduce the inner ones again for
-- every element of the outer ones, which made 'topEntity' take ~140k rewrites
-- (about a minute) to normalize instead of ~300.
module ApplicativeChain where

import Clash.Prelude
import Clash.Explicit.Testbench

data Meta = Meta
  { nWords :: Unsigned 8
  , flag :: Bool
  }
  deriving (Generic, NFDataX)

topEntity ::
  Unsigned 8 ->
  Vec 5 (Unsigned 8) ->
  Vec 5 Meta ->
  Vec 5 (Signed 4) ->
  (Vec 5 (Unsigned 8), Unsigned 8, Vec 4 Bool)
topEntity lim offsets metas ss = (chain, folded, inited)
 where
  -- 'map' followed by five 'zipWith ($)'s
  chain = go <$> offsets <*> metas <*> ss <*> repeat lim <*> reverse offsets <*> offsets
  go o Meta{nWords, flag} s l o1 o2
    | flag = o + nWords + l
    | s < 0 = o1 - o2
    | otherwise = o * nWords + l

  -- 'foldr' over a 'zipWith' with a vector of functions as right argument
  folded = foldr ($) lim (zipWith (\o g x -> g (x + o)) offsets (map (\m x -> x * nWords m) metas))

  -- 'init' of a 'map' producing functions
  inited = map ($ lim) (init (map (\o x -> x > o * 2) offsets))
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput =
    pure
      ( 7
      , 3 :> 5 :> 7 :> 11 :> 13 :> Nil
      , Meta 3 True :> Meta 5 False :> Meta 2 False :> Meta 4 False :> Meta 1 True :> Nil
      , 1 :> -1 :> 2 :> -3 :> 0 :> Nil
      )
  expectedOutput =
    outputVerifier' clk rst
      ((13 :> 6 :> 21 :> 250 :> 21 :> Nil, 174, True :> False :> False :> False :> Nil) :> Nil)
  done = expectedOutput (uncurry4 topEntity <$> testInput)
  uncurry4 f (a, b, c, d) = f a b c d
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
