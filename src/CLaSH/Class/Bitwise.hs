module CLaSH.Class.Bitwise
  ( Bitwise (..)
  , rotate
  , shift
  )
where

class Bitwise a where
  (.&.)      :: a -> a -> a
  (.|.)      :: a -> a -> a
  xor        :: a -> a -> a
  complement :: a -> a
  shiftL     :: Integral i => a -> i -> a
  shiftR     :: Integral i => a -> i -> a
  rotateL    :: Integral i => a -> i -> a
  rotateR    :: Integral i => a -> i -> a
  isSigned   :: a -> Bool

rotate :: Integral i => Num i => Ord i => Bitwise a => a -> i -> a
rotate a i = if i < 0 then rotateL a (negate i) else rotateR a i

shift :: Integral i => Num i => Ord i => Bitwise a => a -> i -> a
shift a i = if i < 0 then shiftL a (negate i) else shiftR a i
