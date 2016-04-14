{-# LANGUAGE KindSignatures #-}
module DFold where

import CLaSH.Prelude
--import Data.Singletons.Prelude
import CLaSH.Promoted.Defun
import Data.Proxy

data Append (m :: Nat) (a :: *) (f :: TyFun Nat *) :: *
type instance Apply (Append m a) l = Vec (l + m) a

append' xs ys = dfold (Proxy :: Proxy (Append m a)) (const (:>)) ys xs

topEntity :: (Vec 3 Int,Vec 7 Int) -> Vec 10 Int
topEntity = uncurry append'

testInput :: Signal (Vec 3 Int, Vec 7 Int)
testInput = pure (7:>8:>9:>Nil,0:>1:>2:>3:>4:>5:>6:>Nil)

expectedOutput :: Signal (Vec 10 Int) -> Signal Bool
expectedOutput = outputVerifier ((7:>8:>9:>0:>1:>2:>3:>4:>5:>6:>Nil):>Nil)
