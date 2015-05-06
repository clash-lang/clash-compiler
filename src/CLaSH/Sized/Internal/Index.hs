{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Sized.Internal.Index
  ( -- * Datatypes
    Index (..)
    -- * Type classes
    -- ** Eq
  , eq#
  , neq#
    -- ** Ord
  , lt#
  , ge#
  , gt#
  , le#
    -- ** Enum (not synthesisable)
  , enumFrom#
  , enumFromThen#
  , enumFromTo#
  , enumFromThenTo#
    -- ** Bounded
  , maxBound#
    -- ** Num
  , (+#)
  , (-#)
  , (*#)
  , fromInteger#
    -- ** Integral
  , quot#
  , rem#
  , mod#
  , toInteger#
  )
where

import Data.Default               (Default (..))
import Language.Haskell.TH        (TypeQ, appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift(..))
import GHC.TypeLits               (KnownNat, Nat, natVal)

-- | Arbitrary-bounded unsigned integer represented by @ceil(log_2(n))@ bits.
--
-- Given an upper bound @n@, an 'Index' @n@ number has a range of: [0 .. @n@-1]
newtype Index (n :: Nat) =
    -- | The constructor, 'I', and the field, 'unsafeToInteger', are not
    -- synthesisable.
    I { unsafeToInteger :: Integer }

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

-- | The functions: 'enumFrom', 'enumFromThen', 'enumFromTo', and
-- 'enumFromThenTo', are not synthesisable.
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

-- | Operators report an error on overflow and underflow
instance KnownNat n => Num (Index n) where
  (+)         = (+#)
  (-)         = (-#)
  (*)         = (*#)
  negate      = (maxBound# -#)
  abs         = id
  signum i    = if i == 0 then 0 else 1
  fromInteger = fromInteger#

(+#),(-#),(*#) :: KnownNat n => Index n -> Index n -> Index n
{-# NOINLINE (+#) #-}
(+#) (I a) (I b) = fromInteger_INLINE $ a + b

{-# NOINLINE (-#) #-}
(-#) (I a) (I b) = fromInteger_INLINE $ a - b

{-# NOINLINE (*#) #-}
(*#) (I a) (I b) = fromInteger_INLINE $ a * b

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
  quot        = quot#
  rem         = rem#
  div         = quot#
  mod         = mod#
  quotRem n d = (n `quot#` d,n `rem#` d)
  divMod  n d = (n `quot#` d,n `mod#` d)
  toInteger   = toInteger#

quot#,rem#,mod# :: Index n -> Index n -> Index n
{-# NOINLINE quot# #-}
(I a) `quot#` (I b) = I (a `div` b)
{-# NOINLINE rem# #-}
(I a) `rem#` (I b) = I (a `rem` b)
{-# NOINLINE mod# #-}
(I a) `mod#` (I b) = I (a `mod` b)

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
