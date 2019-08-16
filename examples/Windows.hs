{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, ExplicitForAll, ScopedTypeVariables #-}
module Windows where

import Clash.Prelude
import qualified Data.List as L

type Elm = Unsigned 16

type Temp = Signed 32

vrotate xs = tail xs :< head xs

windowsI :: (KnownNat (n + 1), ((m + n) + 1) ~ ((m + 1) + n))
         => SNat (m + 1)
         -> Vec (m + n + 1) a
         -> Vec (n + 1) (Vec (m + 1) a)
windowsI m xs = withSNat (\n -> windowsV m n xs)

windowsII :: (KnownNat (n + 1), KnownNat (m + 1), ((m + n) + 1) ~ ((m + 1) + n))
          => Vec (m + n + 1) a
          -> Vec (n + 1) (Vec (m + 1) a)
windowsII xs = withSNat (\m -> withSNat (\n -> windowsV m n xs))

rotated :: ((m + n) + 1) ~ ((m + 1) + n)
        => SNat (m + 1) -> SNat (n + 1)
        -> Vec (m + n + 1) a
        -> Vec (n + 1) (Vec ((m + 1) + n) a)
rotated _ n xs = iterate n vrotate xs

windowsV :: ((m + n) + 1) ~ ((m + 1) + n)
         => SNat (m + 1) -> SNat (n + 1)
         -> Vec (m + n + 1) a
         -> Vec (n + 1) (Vec (m + 1) a)
windowsV m n xs = map (take m) (rotated m n xs)

transpose :: forall r c a . (KnownNat c, KnownNat r) => Vec r (Vec c a) -> Vec c (Vec r a)
transpose m = map (\i -> map (!!i) m) indices
 where
   indices = iterateI (+ 1) 0 :: Vec c (SatIndex 'SatError r)


(+>>>) :: KnownNat n => Vec m a -> Vec (n + m) a -> Vec (m + n) a
l +>>> r = l ++ (takeI r)

swarch1d ::(Vec 3 Temp -> Temp) ->  Vec 4 Temp -> Vec 2 Temp -> (Vec 4 Temp, Vec 2 Temp)
swarch1d f xs inp = (xs', outp)
  where
    xs'  = inp +>>> xs
    outp = map f $ windowsII xs

-- hfk1d :: Vec 3 Temp -> Temp
-- hfk1d (x1 :> x2 :> x3 :> Nil) = x2 + multwc (x1 - 2 * x2 + x3)
--   where
--     multwc a = shiftR (a * 410) 10
hfk1d :: Vec 3 Temp -> Temp
hfk1d xs = x2 + multwc (x1 - 2 * x2 + x3)
  where
    multwc a = shiftR (a * 410) 10
    x1 = xs !! 0
    x2 = xs !! 1
    x3 = xs !! 2

topEntity
  :: Clock  System
  -> Reset  System
  -> Enable System
  -> Signal System (Vec 2 Temp)
  -> Signal System (Vec 2 Temp)
topEntity = exposeClockResetEnable ((swarch1d hfk1d) `mealy` (repeat 0))

res :: [Vec 2 Temp]
res = simulate (topEntity clockGen systemResetGen (enableGen)) $ L.repeat (repeat 45)
