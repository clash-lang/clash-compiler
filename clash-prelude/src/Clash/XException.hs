{-|
Copyright  :  (C) 2016,      University of Twente,
                  2017,      QBayLogic, Google Inc.
                  2017-2019, Myrtle Software Ltd
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

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.XException
  ( -- * 'X': An exception for uninitialized values
    XException(..), errorX, isX, hasX, maybeIsX, maybeHasX
    -- * Printing 'X' exceptions as \"X\"
  , ShowX (..), showsX, printX, showsPrecXWith
    -- * Strict evaluation
  , seqX, forceX, deepseqX, rwhnfX, defaultSeqX
    -- * Structured undefined / deep evaluation with undefined values
  , Undefined (rnfX, deepErrorX)
  )
where

import Control.Exception (Exception, catch, evaluate, throw)
import Control.DeepSeq   (NFData, rnf)
import Data.Complex      (Complex)
import Data.Either       (isLeft)
import Data.Foldable     (toList)
import Data.Int          (Int8,Int16,Int32,Int64)
import Data.Ord          (Down (Down))
import Data.Ratio        (Ratio, numerator, denominator)
import qualified Data.Semigroup as SG
import Data.Sequence     (Seq(Empty, (:<|)))
import Data.Word         (Word8,Word16,Word32,Word64)
import GHC.Exts          (Char (C#), Double (D#), Float (F#), Int (I#), Word (W#))
import GHC.Generics
import GHC.Natural       (Natural)
import GHC.Show          (appPrec)
import GHC.Stack         (HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import System.IO.Unsafe  (unsafeDupablePerformIO)

-- | An exception representing an \"uninitialized\" value.
newtype XException = XException String

instance Show XException where
  show (XException s) = s

instance Exception XException

-- | Either 'seqX' or 'deepSeqX' depending on the value of the cabal flag
-- '-fsuper-strict'. If enabled, 'defaultSeqX' will be 'deepseqX', otherwise
-- 'seqX'. Flag defaults to /false/ and thus 'seqX'.
defaultSeqX :: Undefined a => a -> b -> b
#ifdef CLASH_SUPER_STRICT
defaultSeqX = deepseqX
#else
defaultSeqX = seqX
#endif
{-# INLINE defaultSeqX #-}

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

-- | Evaluate a value with given function, returning 'Nothing' if it throws
-- 'XException'.
--
-- > maybeX hasX 42                  = Just 42
-- > maybeX hasX (XException msg)    = Nothing
-- > maybeX hasX (3, XException msg) = Nothing
-- > maybeX hasX (3, _|_)            = _|_
-- > maybeX hasX _|_                 = _|_
-- >
-- > maybeX isX 42                  = Just 42
-- > maybeX isX (XException msg)    = Nothing
-- > maybeX isX (3, XException msg) = Just (3, XException msg)
-- > maybeX isX (3, _|_)            = Just (3, _|_)
-- > maybeX isX _|_                 = _|_
--
maybeX :: NFData a => (a -> Either String a) -> a -> Maybe a
maybeX f a = either (const Nothing) Just (f a)

-- | Fully evaluate a value, returning 'Nothing' if it throws 'XException'.
--
-- > maybeX 42                  = Just 42
-- > maybeX (XException msg)    = Nothing
-- > maybeX (3, XException msg) = Nothing
-- > maybeX (3, _|_)            = _|_
-- > maybeX _|_                 = _|_
--
maybeHasX :: NFData a => a -> Maybe a
maybeHasX = maybeX hasX

-- | Evaluate a value to WHNF, returning 'Nothing' if it throws 'XException'.
--
-- > maybeIsX 42                  = Just 42
-- > maybeIsX (XException msg)    = Nothing
-- > maybeIsX (3, XException msg) = Just (3, XException msg)
-- > maybeIsX (3, _|_)            = Just (3, _|_)
-- > maybeIsX _|_                 = _|_
maybeIsX :: NFData a => a -> Maybe a
maybeIsX = maybeX isX

-- | Fully evaluate a value, returning @'Left' msg@ if it throws 'XException'.
--
-- > hasX 42                  = Right 42
-- > hasX (XException msg)    = Left msg
-- > hasX (3, XException msg) = Left msg
-- > hasX (3, _|_)            = _|_
-- > hasX _|_                 = _|_
--
-- If a data structure contains multiple 'XException's, the "first" message is
-- picked according to the implementation of 'rnf'.
hasX :: NFData a => a -> Either String a
hasX a =
  unsafeDupablePerformIO
    (catch
      (evaluate (rnf a) >> return (Right a))
      (\(XException msg) -> return (Left msg)))
{-# NOINLINE hasX #-}

-- | Evaluate a value to WHNF, returning @'Left' msg@ if is a 'XException'.
--
-- > isX 42                  = Right 42
-- > isX (XException msg)    = Left msg
-- > isX (3, XException msg) = Right (3, XException msg)
-- > isX (3, _|_)            = (3, _|_)
-- > isX _|_                 = _|_
isX :: a -> Either String a
isX a =
  unsafeDupablePerformIO
    (catch
      (evaluate a >> return (Right a))
      (\(XException msg) -> return (Left msg)))
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

instance ShowX Natural where
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

-- | a variant of 'deepseqX' that is useful in some circumstances:
--
-- > forceX x = x `deepseqX` x
forceX :: Undefined a => a -> a
forceX x = x `deepseqX` x
{-# INLINE forceX #-}

-- | 'deepseqX': fully evaluates the first argument, before returning the
-- second. Does not propagate 'XException's.
deepseqX :: Undefined a => a -> b -> b
deepseqX a b = rnfX a `seq` b
{-# NOINLINE deepseqX #-}

-- | Reduce to weak head normal form
--
-- Equivalent to @\\x -> 'seqX' x ()@.
--
-- Useful for defining 'Undefined.rnfX' for types for which NF=WHNF holds.
rwhnfX :: a -> ()
rwhnfX = (`seqX` ())
{-# INLINE rwhnfX #-}

-- | Hidden internal type-class. Adds a generic implementation for the "NFDataX"
-- part of 'Undefined'
class GNFDataX arity f where
  grnfX :: RnfArgs arity a -> f a -> ()

instance GNFDataX arity V1 where
  grnfX _ x = case x of {}

data Zero
data One

data RnfArgs arity a where
  RnfArgs0 :: RnfArgs Zero a
  RnfArgs1  :: (a -> ()) -> RnfArgs One a

instance GNFDataX arity U1 where
  grnfX _ u = if isLeft (isX u) then () else case u of U1 -> ()

instance Undefined a => GNFDataX arity (K1 i a) where
  grnfX _ = rnfX . unK1
  {-# INLINEABLE grnfX #-}

instance GNFDataX arity a => GNFDataX arity (M1 i c a) where
  grnfX args a =
    -- Check for X needed to handle edge-case "data Void"
    if isLeft (isX a) then
      ()
    else
      grnfX args (unM1 a)
  {-# INLINEABLE grnfX #-}

instance (GNFDataX arity a, GNFDataX arity b) => GNFDataX arity (a :*: b) where
  grnfX args xy@(~(x :*: y)) =
    if isLeft (isX xy) then
      ()
    else
      grnfX args x `seq` grnfX args y
  {-# INLINEABLE grnfX #-}

instance (GNFDataX arity a, GNFDataX arity b) => GNFDataX arity (a :+: b) where
  grnfX args lrx =
    if isLeft (isX lrx) then
      ()
    else
      case lrx of
        L1 x -> grnfX args x
        R1 x -> grnfX args x
  {-# INLINEABLE grnfX #-}

instance GNFDataX One Par1 where
  grnfX (RnfArgs1 r) = r . unPar1

instance NFDataX1 f => GNFDataX One (Rec1 f) where
  grnfX (RnfArgs1 r) = liftRnfX r . unRec1

instance (NFDataX1 f, GNFDataX One g) => GNFDataX One (f :.: g) where
  grnfX args = liftRnfX (grnfX args) . unComp1

-- | A class of functors that can be fully evaluated, according to semantics
-- of NFDataX.
class NFDataX1 f where
  -- | 'liftRnfX' should reduce its argument to normal form (that is, fully
  -- evaluate all sub-components), given an argument to reduce @a@ arguments,
  -- and then return '()'.
  --
  -- See 'rnfX' for the generic deriving.
  liftRnfX :: (a -> ()) -> f a -> ()

  default liftRnfX :: (Generic1 f, GNFDataX One (Rep1 f)) => (a -> ()) -> f a -> ()
  liftRnfX r = grnfX (RnfArgs1 r) . from1

-- | Create a value where all the elements have an 'errorX' but the spine
-- is defined, and fully evaluate a value with 'errorX's in it.
class Undefined a where
  -- | Create a value where all the elements have an 'errorX', but the spine
  -- is defined.
  deepErrorX :: HasCallStack => String -> a

  default deepErrorX :: (HasCallStack, Generic a, GUndefined (Rep a)) => String -> a
  deepErrorX = withFrozenCallStack $ to . gDeepErrorX

  -- | Evaluate a value to NF. As opposed to 'NFData's 'rnf', it does not bubble
  -- up 'XException's.
  rnfX :: a -> ()

  default rnfX :: (Generic a, GNFDataX Zero (Rep a)) => a -> ()
  rnfX = grnfX RnfArgs0 . from

instance Undefined ()
instance (Undefined a, Undefined b) => Undefined (a,b)
instance (Undefined a, Undefined b, Undefined c) => Undefined (a,b,c)
instance (Undefined a, Undefined b, Undefined c, Undefined d) => Undefined (a,b,c,d)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e) => Undefined (a,b,c,d,e)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e ,Undefined f)
  => Undefined (a,b,c,d,e,f)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g)
  => Undefined (a,b,c,d,e,f,g)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h)
  => Undefined (a,b,c,d,e,f,g,h)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i)
  => Undefined (a,b,c,d,e,f,g,h,i)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j)
  => Undefined (a,b,c,d,e,f,g,h,i,j)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j
         ,Undefined k)
  => Undefined (a,b,c,d,e,f,g,h,i,j,k)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j
         ,Undefined k, Undefined l)
  => Undefined (a,b,c,d,e,f,g,h,i,j,k,l)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j
         ,Undefined k, Undefined l, Undefined m)
  => Undefined (a,b,c,d,e,f,g,h,i,j,k,l,m)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j
         ,Undefined k, Undefined l, Undefined m, Undefined n)
  => Undefined (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
instance (Undefined a, Undefined b, Undefined c, Undefined d, Undefined e
         ,Undefined f, Undefined g, Undefined h, Undefined i, Undefined j
         ,Undefined k, Undefined l, Undefined m, Undefined n, Undefined o)
  => Undefined (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

instance Undefined b => Undefined (a -> b) where
  deepErrorX = pure . deepErrorX
  rnfX = rwhnfX

instance Undefined a => Undefined (Down a) where
  deepErrorX = Down . deepErrorX
  rnfX d@(~(Down x))= if isLeft (isX d) then rnfX x else ()

instance Undefined Bool
instance Undefined a => Undefined [a]
instance (Undefined a, Undefined b) => Undefined (Either a b)
instance Undefined a => Undefined (Maybe a)

instance Undefined Char where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Double where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Float where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Int where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Int8 where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Int16 where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Int32 where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Int64 where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Integer where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Natural where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Word where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Word8 where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Word16 where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Word32 where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined Word64 where
  deepErrorX = errorX
  rnfX = rwhnfX

instance Undefined a => Undefined (Seq a) where
  deepErrorX = errorX
  rnfX s =
    if isLeft (isX s) then () else go s
   where
    go Empty = ()
    go (x :<| xs) = rnfX x `seq` go xs

instance Undefined a => Undefined (Ratio a) where
  deepErrorX = errorX
  rnfX r = rnfX (numerator r) `seq` rnfX (denominator r)

instance Undefined a => Undefined (Complex a) where
  deepErrorX = errorX

instance (Undefined a, Undefined b) => Undefined (SG.Arg a b)
instance Undefined (SG.All)
instance Undefined (SG.Any)
instance Undefined a => Undefined (SG.Dual a)
instance Undefined a => Undefined (SG.Endo a)
instance Undefined a => Undefined (SG.First a)
instance Undefined a => Undefined (SG.Last a)
instance Undefined a => Undefined (SG.Max a)
instance Undefined a => Undefined (SG.Min a)
instance Undefined a => Undefined (SG.Option a)
instance Undefined a => Undefined (SG.Product a)
instance Undefined a => Undefined (SG.Sum a)

class GUndefined f where
  gDeepErrorX :: HasCallStack => String -> f a

instance GUndefined V1 where
  gDeepErrorX = errorX

instance GUndefined U1 where
  gDeepErrorX = const U1

instance (GUndefined a) => GUndefined (M1 m d a) where
  gDeepErrorX e = M1 (gDeepErrorX e)

instance (GUndefined f, GUndefined g) => GUndefined (f :*: g) where
  gDeepErrorX e = gDeepErrorX e :*: gDeepErrorX e

instance Undefined c => GUndefined (K1 i c) where
  gDeepErrorX e = K1 (deepErrorX e)

instance GUndefined (f :+: g) where
  gDeepErrorX = errorX
