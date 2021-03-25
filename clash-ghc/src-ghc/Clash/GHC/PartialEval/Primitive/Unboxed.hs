{-|
Copyright   : (C) 2020, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Newtypes for unboxed versions of types used in the partial evaluator. These
are needed, as it is not possible to write instances of FromAst / ToAst using
the actual unboxed types.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Clash.GHC.PartialEval.Primitive.Unboxed where

import Data.Primitive.ByteArray (ByteArray)

newtype UByteArray
  = UByteArray { boxByteArray :: ByteArray }
  deriving (Show)

newtype UChar
  = UChar { boxChar :: Char }
  deriving (Show)

newtype UInt
  = UInt { boxInt :: Int }
  deriving (Show)

newtype UWord
  = UWord {boxWord :: Word }
  deriving (Show)

newtype UFloat
  = UFloat { boxFloat :: Float }
  deriving (Show)

newtype UDouble
  = UDouble { boxDouble :: Double }
  deriving (Show)

newtype UTuple2 a b
  = UTuple2 { boxTuple2 :: (a, b) }
  deriving (Show)

newtype UTuple4 a b c d
  = UTuple4 { boxTuple4 :: (a, b, c, d) }
  deriving (Show)

data Ref a = Ref
  { refAddr :: Maybe Int
  , refValue :: a
  } deriving (Show)
