{-|
  Copyright   :  (C) 2019, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module AutoReg where
import Data.Int
import Clash.Prelude
import Clash.Sized.Internal.BitVector
import Control.Monad (when)
import qualified Data.List as L
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

#ifndef OUTPUTTEST
data Tup2 a b = MkTup2 { getA :: a, getB :: b } deriving (Generic,NFDataX)
instance (BitPack a,BitPack b) => BitPack (Tup2 a b)
deriveAutoReg ''Tup2


data Tup3 a b c = MkTup3 { fieldA :: a, fieldB :: b, fieldC :: c } deriving (Generic,NFDataX)
instance (BitPack a, BitPack b, BitPack c) => BitPack (Tup3 a b c)
deriveAutoReg ''Tup3


newtype OtherPair a b = OtherPair (Tup2 a b) deriving (Generic,NFDataX)
instance (BitPack a, BitPack b) => BitPack (OtherPair a b)
deriveAutoReg ''OtherPair

data Tup2_ a b c = MkTup2_ a b deriving (Generic,NFDataX)
instance (BitPack a, BitPack b) => BitPack (Tup2_ a b c)
deriveAutoReg ''Tup2_
-- NOTE: For some reason this deriveAutoReg ''Tup2_ creates invalid code when
-- run by runghc-8.4.4 for the output test. (newer versions are ok)
-- Since output test only cares about main[VHDL,...] we can #ifndef the rest away.

data Concrete = BoolAndInt Bool Int8 deriving (Generic,NFDataX,BitPack)
deriveAutoReg ''Concrete

data InfixDataCon a b = a :-.- b deriving (Generic,NFDataX)
instance (BitPack a, BitPack b) => BitPack (InfixDataCon a b)
deriveAutoReg ''InfixDataCon


test
  :: forall a dom n rest
   . ( HiddenClockResetEnable dom
     , AutoReg a, BitPack a
     , KnownNat n, KnownNat rest
     , rest ~ (n-(BitSize a))
     )
  => Signal dom (BitVector n) -> Signal dom a
test = autoReg (unpack 0) . fmap (unpack . fst . split# @rest)

topEntity
  :: Clock System -> Reset System
  -> Signal System (BitVector 32)
  -> _
topEntity clk rst xs = withClockResetEnable clk rst enableGen $
  -- This used to be one big tuple. We split it up so we can get away with
  -- -DMAX_TUPLE_SIZE=12, which considerably improves compilation speed of
  -- clash-prelude
  ( ( test @(Unsigned 16) xs
    , test @Bool xs
    , test @(Tup2 Bool Bool) xs
    , test @(Tup3 Bool Bool Bool) xs
    , test @(Maybe Bool) xs
    , test @(Maybe (Maybe Bool)) xs
    , test @(OtherPair Int8 Int16) xs
    )
  , ( test @Concrete xs
    , test @(InfixDataCon Bool Bool) xs
    , test @((),Bool) xs
    , test @(Vec 2 Bool) xs
    , test @(Vec 2 (Maybe Bool)) xs
    , test @(Maybe (Vec 2 (Maybe Bool))) xs
    )
  )
#endif

expectedRegCount = sum
  [ 1   -- Unsigned
  , 1   -- Bool
  , 2   -- Tup2
  , 3   -- Tup3
  , 2   -- Maybe Bool
  , 3   -- Maybe (Maybe Bool)
  , 2   -- OtherPair Int8 Int16
  , 2   -- Concrete
  , 2   -- InfixDataCon Bool Bool
  , 1   -- ((),Bool)
  , 2   -- Vec 2 Bool
  , 2*2 -- Vec 2 (Maybe Bool)
  , 1+2*2 -- Maybe (Vec 2 (Maybe Bool))
  ]

countLinesContaining :: String -> String -> Int
countLinesContaining needle haystack = L.length $ L.filter (needle `L.isInfixOf`) $ lines haystack

mainHDL :: String -> IO ()
mainHDL topFile = do
  [topDir] <- getArgs
  content <- readFile (takeDirectory topDir </> topFile)
  let regCount = countLinesContaining "register begin" content
  when (expectedRegCount /= regCount)
    (error $ unlines
      [ ""
      , "Error: Found " <> show regCount <> " registers in " <> topFile
      , "But expected " <> show expectedRegCount
      ])

mainSystemVerilog, mainVerilog, mainVHDL :: IO ()
mainSystemVerilog = mainHDL "topEntity.sv"
mainVerilog       = mainHDL "topEntity.v"
mainVHDL          = mainHDL "topEntity.vhdl"
