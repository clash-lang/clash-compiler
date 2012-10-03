{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module CLaSH.Sized.Unsigned where

import CLaSH.Class.Num
import CLaSH.Promoted.Ord

import qualified Prelude
import Prelude (Integer,undefined,Show,(.))
import GHC.TypeLits

newtype Unsigned (n :: Nat) = U Integer deriving Show

instance Add (Unsigned n) where
  type AResult (Unsigned n) = Unsigned (n + 1)
  (U a) + (U b) = U (a Prelude.+ b)
  (U a) - (U b) = U (a Prelude.- b)

instance Mult (Unsigned n) where
  type MResult (Unsigned n) = Unsigned (n + n)
  (U a) * (U b) = U (a Prelude.* b)

resize :: Unsigned n -> Unsigned m
resize (U n) = (U n)

dot = (.).(.)

u1 :: Unsigned 4
u1 = (U 2)

u2 :: Unsigned 4
u2 = (U 4)

u3 :: Unsigned 5
u3 = u1 + u2
