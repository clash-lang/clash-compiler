{-|
Copyright  :  (C) 2016, University of Twente,
                  2017, Myrtle Software Ltd, QBayLogic, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

'X': An exception for uninitialized values

>>> show (errorX "undefined" :: Integer, 4 :: Int)
"(*** Exception: X: undefined
CallStack (from HasCallStack):
...
>>> showX (errorX "undefined" :: Integer, 4 :: Int)
"(X,4)"
-}

{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.XException
  ( -- * 'X': An exception for uninitialized values
    XException, errorX, isX, maybeX
    -- * Printing 'X' exceptions as \"X\"
  , ShowX (..), showsX, printX, showsPrecXWith
    -- * Strict evaluation
  , seqX
    -- * Structured undefined
  , Undefined (..)
  )
where

import Control.Exception (Exception, catch, evaluate, throw)
import Control.DeepSeq   (NFData, rnf)
import Data.Complex      (Complex)
import Data.Foldable     (toList)
import Data.Int          (Int8,Int16,Int32,Int64)
import Data.Ord          (Down (Down))
import Data.Ratio        (Ratio)
import Data.Sequence     (Seq)
import Data.Word         (Word8,Word16,Word32,Word64)
import GHC.Exts          (Char (C#), Double (D#), Float (F#), Int (I#), Word (W#))
import GHC.Generics
import GHC.Show          (appPrec)
import GHC.Stack         (HasCallStack, callStack, prettyCallStack)
import System.IO.Unsafe  (unsafeDupablePerformIO)

-- | An exception representing an \"uninitialised\" value.
newtype XException = XException String

instance Show XException where
  show (XException s) = s

instance Exception XException

-- | Like 'error', but throwing an 'XException' instead of an 'ErrorCall'
--
-- The 'ShowX' methods print these error-values as \"X\"; instead of error'ing
-- out with an exception.
errorX :: HasCallStack => String -> a
errorX msg = throw (XException ("X: " ++ msg ++ "\n" ++ prettyCallStack callStack))

-- | Like 'seq', however, whereas 'seq' will always do:
--
-- > seq  _|_              b = _|_
--
-- 'seqX' will do:
--
-- > seqX (XException msg) b = b
-- > seqX _|_              b = _|_
seqX :: a -> b -> b
seqX a b = unsafeDupablePerformIO
  (catch (evaluate a >> return b) (\(XException _) -> return b))
{-# NOINLINE seqX #-}
infixr 0 `seqX`

-- | Fully evaluate a value, returning 'Nothing' if is throws 'XException'.
--
-- > maybeX 42               = Just 42
-- > maybeX (XException msg) = Nothing
-- > maybeX _|_              = _|_
maybeX :: NFData a => a -> Maybe a
maybeX = either (const Nothing) Just . isX

-- | Fully evaluate a value, returning @'Left' msg@ if is throws 'XException'.
--
-- > isX 42               = Right 42
-- > isX (XException msg) = Left msg
-- > isX _|_              = _|_
isX :: NFData a => a -> Either String a
isX a = unsafeDupablePerformIO
  (catch (evaluate (rnf a) >> return (Right a)) (\(XException msg) -> return (Left msg)))
{-# NOINLINE isX #-}

showXWith :: (a -> ShowS) -> a -> ShowS
showXWith f x =
  \s -> unsafeDupablePerformIO (catch (f <$> evaluate x <*> pure s)
                                      (\(XException _) -> return ('X': s)))

-- | Use when you want to create a 'ShowX' instance where:
--
-- - There is no 'Generic' instance for your data type
-- - The 'Generic' derived ShowX method would traverse into the (hidden)
--   implementation details of your data type, and you just want to show the
--   entire value as \"X\".
--
-- Can be used like:
--
-- > data T = ...
-- >
-- > instance Show T where ...
-- >
-- > instance ShowX T where
-- >   showsPrecX = showsPrecXWith showsPrec
showsPrecXWith :: (Int -> a -> ShowS) -> Int -> a -> ShowS
showsPrecXWith f n = showXWith (f n)

-- | Like 'shows', but values that normally throw an 'X' exception are
-- converted to \"X\", instead of error'ing out with an exception.
showsX :: ShowX a => a -> ShowS
showsX = showsPrecX 0

-- | Like 'print', but values that normally throw an 'X' exception are
-- converted to \"X\", instead of error'ing out with an exception
printX :: ShowX a => a -> IO ()
printX x = putStrLn $ showX x

-- | Like the 'Show' class, but values that normally throw an 'X' exception are
-- converted to \"X\", instead of error'ing out with an exception.
--
-- >>> show (errorX "undefined" :: Integer, 4 :: Int)
-- "(*** Exception: X: undefined
-- CallStack (from HasCallStack):
-- ...
-- >>> showX (errorX "undefined" :: Integer, 4 :: Int)
-- "(X,4)"
--
-- Can be derived using 'GHC.Generics':
--
-- > {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- >
-- > import Clash.Prelude
-- > import GHC.Generics
-- >
-- > data T = MkTA Int | MkTB Bool
-- >   deriving (Show,Generic,ShowX)
class ShowX a where
  -- | Like 'showsPrec', but values that normally throw an 'X' exception are
  -- converted to \"X\", instead of error'ing out with an exception.
  showsPrecX :: Int -> a -> ShowS

  -- | Like 'show', but values that normally throw an 'X' exception are
  -- converted to \"X\", instead of error'ing out with an exception.
  showX :: a -> String
  showX x = showsX x ""

  -- | Like 'showList', but values that normally throw an 'X' exception are
  -- converted to \"X\", instead of error'ing out with an exception.
  showListX :: [a] -> ShowS
  showListX ls s = showListX__ showsX ls s

  default showsPrecX :: (Generic a, GShowX (Rep a)) => Int -> a -> ShowS
  showsPrecX = genericShowsPrecX

showListX__ :: (a -> ShowS) -> [a] -> ShowS
showListX__ showx = showXWith go
  where
    go []     s = "[]" ++ s
    go (x:xs) s = '[' : showx x (showl xs)
      where
        showl []     = ']':s
        showl (y:ys) = ',' : showx y (showl ys)

data ShowType = Rec        -- Record
              | Tup        -- Tuple
              | Pref       -- Prefix
              | Inf String -- Infix

genericShowsPrecX :: (Generic a, GShowX (Rep a)) => Int -> a -> ShowS
genericShowsPrecX n = gshowsPrecX Pref n . from

instance ShowX ()
instance (ShowX a, ShowX b) => ShowX (a,b)
instance (ShowX a, ShowX b, ShowX c) => ShowX (a,b,c)
instance (ShowX a, ShowX b, ShowX c, ShowX d) => ShowX (a,b,c,d)
instance (ShowX a, ShowX b, ShowX c, ShowX d, ShowX e) => ShowX (a,b,c,d,e)
instance (ShowX a, ShowX b, ShowX c, ShowX d, ShowX e, ShowX f) => ShowX (a,b,c,d,e,f)
instance (ShowX a, ShowX b, ShowX c, ShowX d, ShowX e, ShowX f, ShowX g) => ShowX (a,b,c,d,e,f,g)

-- Show is defined up to 15-tuples, but GHC.Generics only has Generic instances
-- up to 7-tuples, hence we need these orphan instances.
deriving instance Generic ((,,,,,,,) a b c d e f g h)
deriving instance Generic ((,,,,,,,,) a b c d e f g h i)
deriving instance Generic ((,,,,,,,,,) a b c d e f g h i j)
deriving instance Generic ((,,,,,,,,,,) a b c d e f g h i j k)
deriving instance Generic ((,,,,,,,,,,,) a b c d e f g h i j k l)
deriving instance Generic ((,,,,,,,,,,,,) a b c d e f g h i j k l m)
deriving instance Generic ((,,,,,,,,,,,,,) a b c d e f g h i j k l m n)
deriving instance Generic ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o)

instance (ShowX a, ShowX b, ShowX c, ShowX d, ShowX e, ShowX f, ShowX g, ShowX h) => ShowX (a,b,c,d,e,f,g,h)
instance (ShowX a, ShowX b, ShowX c, ShowX d, ShowX e, ShowX f, ShowX g, ShowX h, ShowX i) => ShowX (a,b,c,d,e,f,g,h,i)
instance (ShowX a, ShowX b, ShowX c, ShowX d, ShowX e, ShowX f, ShowX g, ShowX h, ShowX i, ShowX j)
  => ShowX (a,b,c,d,e,f,g,h,i,j)
instance (ShowX a, ShowX b, ShowX c, ShowX d, ShowX e, ShowX f, ShowX g, ShowX h, ShowX i, ShowX j, ShowX k)
  => ShowX (a,b,c,d,e,f,g,h,i,j,k)
instance (ShowX a, ShowX b, ShowX c, ShowX d, ShowX e, ShowX f, ShowX g, ShowX h, ShowX i, ShowX j, ShowX k, ShowX l)
  => ShowX (a,b,c,d,e,f,g,h,i,j,k,l)
instance (ShowX a, ShowX b, ShowX c, ShowX d, ShowX e, ShowX f, ShowX g, ShowX h, ShowX i, ShowX j, ShowX k, ShowX l
         ,ShowX m)
  => ShowX (a,b,c,d,e,f,g,h,i,j,k,l,m)
instance (ShowX a, ShowX b, ShowX c, ShowX d, ShowX e, ShowX f, ShowX g, ShowX h, ShowX i, ShowX j, ShowX k, ShowX l
         ,ShowX m, ShowX n)
  => ShowX (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
instance (ShowX a, ShowX b, ShowX c, ShowX d, ShowX e, ShowX f, ShowX g, ShowX h, ShowX i, ShowX j, ShowX k, ShowX l
         ,ShowX m, ShowX n, ShowX o)
  => ShowX (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

instance {-# OVERLAPPABLE #-} ShowX a => ShowX [a] where
  showsPrecX _ = showListX

instance ShowX Char where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Bool

instance ShowX Double where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX a => ShowX (Down a) where
  showsPrecX = showsPrecXWith showsPrecX

instance (ShowX a, ShowX b) => ShowX (Either a b)

instance ShowX Float where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Int where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Int8 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Int16 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Int32 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Int64 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Integer where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX a => ShowX (Seq a) where
  showsPrecX _ = showListX . toList

instance ShowX Word where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Word8 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Word16 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Word32 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX Word64 where
  showsPrecX = showsPrecXWith showsPrec

instance ShowX a => ShowX (Maybe a)

instance ShowX a => ShowX (Ratio a) where
  showsPrecX = showsPrecXWith showsPrecX

instance ShowX a => ShowX (Complex a)

instance {-# OVERLAPPING #-} ShowX String where
  showsPrecX = showsPrecXWith showsPrec

class GShowX f where
  gshowsPrecX :: ShowType -> Int -> f a -> ShowS
  isNullary   :: f a -> Bool
  isNullary = error "generic showX (isNullary): unnecessary case"

instance GShowX U1 where
  gshowsPrecX _ _ U1 = id
  isNullary _ = True

instance (ShowX c) => GShowX (K1 i c) where
  gshowsPrecX _ n (K1 a) = showsPrecX n a
  isNullary _ = False

instance (GShowX a, Constructor c) => GShowX (M1 C c a) where
  gshowsPrecX _ n c@(M1 x) =
    case fixity of
      Prefix ->
        showParen (n > appPrec && not (isNullary x))
          ( (if conIsTuple c then id else showString (conName c))
          . (if isNullary x || conIsTuple c then id else showString " ")
          . showBraces t (gshowsPrecX t appPrec x))
      Infix _ m -> showParen (n > m) (showBraces t (gshowsPrecX t m x))
      where fixity = conFixity c
            t = if conIsRecord c then Rec else
                  case conIsTuple c of
                    True -> Tup
                    False -> case fixity of
                                Prefix    -> Pref
                                Infix _ _ -> Inf (show (conName c))
            showBraces :: ShowType -> ShowS -> ShowS
            showBraces Rec     p = showChar '{' . p . showChar '}'
            showBraces Tup     p = showChar '(' . p . showChar ')'
            showBraces Pref    p = p
            showBraces (Inf _) p = p

            conIsTuple :: C1 c f p -> Bool
            conIsTuple y = tupleName (conName y) where
              tupleName ('(':',':_) = True
              tupleName _           = False

instance (Selector s, GShowX a) => GShowX (M1 S s a) where
  gshowsPrecX t n s@(M1 x) | selName s == "" =   gshowsPrecX t n x
                           | otherwise       =   showString (selName s)
                                               . showString " = "
                                               . gshowsPrecX t 0 x
  isNullary (M1 x) = isNullary x

instance (GShowX a) => GShowX (M1 D d a) where
  gshowsPrecX t = showsPrecXWith go
    where go n (M1 x) = gshowsPrecX t n x

instance (GShowX a, GShowX b) => GShowX (a :+: b) where
  gshowsPrecX t n (L1 x) = gshowsPrecX t n x
  gshowsPrecX t n (R1 x) = gshowsPrecX t n x

instance (GShowX a, GShowX b) => GShowX (a :*: b) where
  gshowsPrecX t@Rec     n (a :*: b) =
    gshowsPrecX t n     a . showString ", " . gshowsPrecX t n     b
  gshowsPrecX t@(Inf s) n (a :*: b) =
    gshowsPrecX t n     a . showString s    . gshowsPrecX t n     b
  gshowsPrecX t@Tup     n (a :*: b) =
    gshowsPrecX t n     a . showChar ','    . gshowsPrecX t n     b
  gshowsPrecX t@Pref    n (a :*: b) =
    gshowsPrecX t (n+1) a . showChar ' '    . gshowsPrecX t (n+1) b

  -- If we have a product then it is not a nullary constructor
  isNullary _ = False

-- Unboxed types
instance GShowX UChar where
  gshowsPrecX _ _ (UChar c)   = showsPrec 0 (C# c) . showChar '#'
instance GShowX UDouble where
  gshowsPrecX _ _ (UDouble d) = showsPrec 0 (D# d) . showString "##"
instance GShowX UFloat where
  gshowsPrecX _ _ (UFloat f)  = showsPrec 0 (F# f) . showChar '#'
instance GShowX UInt where
  gshowsPrecX _ _ (UInt i)    = showsPrec 0 (I# i) . showChar '#'
instance GShowX UWord where
  gshowsPrecX _ _ (UWord w)   = showsPrec 0 (W# w) . showString "##"

-- | Create a value where all the elements have an 'errorX', but the spine
-- is defined.
class Undefined a where
  -- | Create a value where all the elements have an 'errorX', but the spine
  -- is defined.
  deepErrorX :: HasCallStack => String -> a
  deepErrorX = errorX

instance Undefined ()
instance (Undefined a, Undefined b) => Undefined (a,b) where
  deepErrorX x = (deepErrorX x,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c) => Undefined (a,b,c) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d) =>
  Undefined (a,b,c,d) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e) =>
  Undefined (a,b,c,d,e) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f)
  => Undefined (a,b,c,d,e,f) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g)
  => Undefined (a,b,c,d,e,f,g) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h)
  => Undefined (a,b,c,d,e,f,g,h) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i)
  => Undefined (a,b,c,d,e,f,g,h,i) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j)
  => Undefined (a,b,c,d,e,f,g,h,i,j) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j
         ,Undefined k)
  => Undefined (a,b,c,d,e,f,g,h,i,j,k) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j
         ,Undefined k, Undefined l)
  => Undefined (a,b,c,d,e,f,g,h,i,j,k,l) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j
         ,Undefined k, Undefined l, Undefined m)
  => Undefined (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j
         ,Undefined k, Undefined l, Undefined m, Undefined n)
  => Undefined (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j
         ,Undefined k, Undefined l, Undefined m, Undefined n, Undefined o)
  => Undefined (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  deepErrorX x = (deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x,deepErrorX x
                 ,deepErrorX x,deepErrorX x,deepErrorX x)

instance Undefined b => Undefined (a -> b) where
  deepErrorX = pure . deepErrorX

instance Undefined a => Undefined (Down a) where
  deepErrorX = Down . deepErrorX

instance Undefined [a]
instance Undefined Char
instance Undefined Bool
instance Undefined Double
instance Undefined (Either a b)
instance Undefined Float
instance Undefined Int
instance Undefined Int8
instance Undefined Int16
instance Undefined Int32
instance Undefined Int64
instance Undefined Integer
instance Undefined (Seq a)
instance Undefined Word
instance Undefined Word8
instance Undefined Word16
instance Undefined Word32
instance Undefined Word64
instance Undefined (Maybe a)
instance Undefined (Ratio a)
instance Undefined (Complex a)
