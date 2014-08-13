module CLaSH.Class.Bitwise
  ( Bitwise (..)
  , rotate
  , shift
  )
where

class Bitwise a where
  -- | Bitwise \"and\" operation
  (.&.)      :: a -> a -> a
  -- | Bitwise \"or\" operation
  (.|.)      :: a -> a -> a
  -- | Bitwise \"xor\" operation
  xor        :: a -> a -> a
  -- | Perform a unary negation on all the bits in the argument
  complement :: a -> a
  -- | Shift the argument left by the specified number of bits. The number of
  -- specified bits must be non-negative.
  shiftL     :: Integral i => a -> i -> a
  -- | Shift the argument right by the specified number of bits. The number of
  -- specified bits must be non-negative.
  shiftR     :: Integral i => a -> i -> a
  -- | Rotate the argument left by the specified number of bits. The number of
  -- specified bits must be non-negative.
  rotateL    :: Integral i => a -> i -> a
  -- | Rotate the argument right by the specified number of bits. The number of
  -- specified bits must be non-negative.
  rotateR    :: Integral i => a -> i -> a
  -- | Returns true if the arguments represents a signed type
  isSigned   :: a -> Bool

rotate :: Integral i => Num i => Ord i => Bitwise a => a -> i -> a
rotate a i = if i < 0 then rotateL a (negate i) else rotateR a i

shift :: Integral i => Num i => Ord i => Bitwise a => a -> i -> a
shift a i = if i < 0 then shiftL a (negate i) else shiftR a i
