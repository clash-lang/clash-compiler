{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module CLaSH.Sized.Index
  ( Index
  )
where

import Data.Default               (Default (..))
import Data.Typeable              (Typeable)
import Language.Haskell.TH        (TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift(..))
import GHC.TypeLits               (KnownNat, Nat, natVal)

import CLaSH.Class.Resize         (Resize (..))

-- | Arbitrary-bounded unsigned integer represented by @ceil(log(n))@ bits
--
-- Given an upper bound @n@, an 'Index' @n@ number has a range of: [0 .. @n@-1]
newtype Index (n :: Nat) = I Integer
  deriving Typeable

instance Eq (Index n) where
  (==) = eq#
  (/=) = neq#

{-# NOINLINE eq# #-}
eq# :: (Index n) -> (Index n) -> Bool
(I n) `eq#` (I m) = n == m

{-# NOINLINE neq# #-}
neq# :: (Index n) -> (Index n) -> Bool
(I n) `neq#` (I m) = n /= m

instance Ord (Index n) where
  (<)  = lt#
  (>=) = ge#
  (>)  = gt#
  (<=) = le#

lt#,ge#,gt#,le# :: Index n -> Index n -> Bool
{-# NOINLINE lt# #-}
lt# (I n) (I m) = n < m
{-# NOINLINE ge# #-}
ge# (I n) (I m) = n >= m
{-# NOINLINE gt# #-}
gt# (I n) (I m) = n > m
{-# NOINLINE le# #-}
le# (I n) (I m) = n <= m

instance KnownNat n => Enum (Index n) where
  succ           = (+# fromInteger# 1)
  pred           = (-# fromInteger# 1)
  toEnum         = fromInteger# . toInteger
  fromEnum       = fromEnum . toInteger#
  enumFrom       = enumFrom#
  enumFromThen   = enumFromThen#
  enumFromTo     = enumFromTo#
  enumFromThenTo = enumFromThenTo#

{-# NOINLINE enumFrom# #-}
{-# NOINLINE enumFromThen# #-}
{-# NOINLINE enumFromTo# #-}
{-# NOINLINE enumFromThenTo# #-}
enumFrom#       :: KnownNat n => Index n -> [Index n]
enumFromThen#   :: KnownNat n => Index n -> Index n -> [Index n]
enumFromTo#     :: KnownNat n => Index n -> Index n -> [Index n]
enumFromThenTo# :: KnownNat n => Index n -> Index n -> Index n -> [Index n]
enumFrom# x             = map toEnum [fromEnum x ..]
enumFromThen# x y       = map toEnum [fromEnum x, fromEnum y ..]
enumFromTo# x y         = map toEnum [fromEnum x .. fromEnum y]
enumFromThenTo# x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

instance KnownNat n => Bounded (Index n) where
  minBound = fromInteger# 0
  maxBound = maxBound#

{-# NOINLINE maxBound# #-}
maxBound# :: KnownNat n => Index n
maxBound# = let res = I (natVal res - 1) in res

-- | Operators report an error on overflow
instance KnownNat n => Num (Index n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = (maxBound# -#)
  abs         = id
  signum      = signum#
  fromInteger = fromInteger#

(+#),(-#),(*#) :: KnownNat n => Index n -> Index n -> Index n
{-# NOINLINE (+#) #-}
(+#) (I a) (I b) = fromInteger_INLINE $ a + b

{-# NOINLINE (-#) #-}
(-#) (I a) (I b) = fromInteger_INLINE $ a - b

{-# NOINLINE (*#) #-}
(*#) (I a) (I b) = fromInteger_INLINE $ a * b

{-# NOINLINE signum# #-}
signum# :: Index n -> Index n
signum# (I 0) = (I 0)
signum# (I _) = (I 1)

fromInteger#,fromInteger_INLINE :: KnownNat n => Integer -> Index n
{-# NOINLINE fromInteger# #-}
fromInteger# = fromInteger_INLINE
{-# INLINE fromInteger_INLINE #-}
fromInteger_INLINE i =
  let bound = natVal res
      i'    = i `mod` bound
      err   = error (show i ++ " is out of bounds: [0.." ++ show (bound - 1) ++ "]")
      res   = if i' /= i then err else I i
  in  res

instance KnownNat n => Real (Index n) where
  toRational = toRational . toInteger#

instance KnownNat n => Integral (Index n) where
  quot      = quot#
  rem       = rem#
  div       = quot#
  mod       = mod#
  quotRem   = quotRem#
  divMod    = divMod#
  toInteger = toInteger#

quot#,rem#,mod# :: KnownNat n => Index n -> Index n -> Index n
{-# NOINLINE quot# #-}
quot# = (fst.) . quotRem_INLINE
{-# NOINLINE rem# #-}
rem# = (snd.) . quotRem_INLINE
{-# NOINLINE mod# #-}
(I a) `mod#` (I b) = fromInteger_INLINE (a `mod` b)

quotRem#,divMod# :: KnownNat n => Index n -> Index n -> (Index n, Index n)
quotRem# n d = (n `quot#` d,n `rem#` d)
divMod# n d  = (n `quot#` d,n `mod#` d)

{-# INLINE quotRem_INLINE #-}
quotRem_INLINE :: KnownNat n => Index n -> Index n -> (Index n, Index n)
(I a) `quotRem_INLINE` (I b) = let (a',b') = a `quotRem` b
                               in  (fromInteger_INLINE a', fromInteger_INLINE b')

{-# NOINLINE toInteger# #-}
toInteger# :: Index n -> Integer
toInteger# (I n) = n

instance KnownNat n => Lift (Index n) where
  lift u@(I i) = sigE [| fromInteger# i |] (decIndex (natVal u))

decIndex :: Integer -> TypeQ
decIndex n = appT (conT ''Index) (litT $ numTyLit n)

instance Show (Index n) where
  show (I n) = show n

instance KnownNat n => Default (Index n) where
  def = fromInteger# 0

-- | A resize operation that errors when the value is out of the new
-- representation bounds.
instance Resize Index where
  resize = resize#

{-# NOINLINE resize# #-}
resize# :: KnownNat m => Index n -> Index m
resize# (I n) = fromInteger_INLINE n
