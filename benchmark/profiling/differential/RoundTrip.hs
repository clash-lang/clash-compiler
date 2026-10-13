{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Check the 'Binary' instances of scoped Clash Core
  ("Clash.Core.Binary"): that the round trip through them gives every
  binding of a serialized design back, up to alpha-equivalence.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RoundTrip
  ( roundTrip
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Binary (decode, encode)
import qualified Data.List as List
import qualified Data.Text as Text
import System.IO (hFlush, stdout)

import qualified Clash.Core.Name as C
-- For the orphan 'Binary' instance of 'Clash.Netlist.Types.TopEntityT'
import Clash.Driver.Manifest ()
import Clash.Driver.Types (Binding (..), GlobalBindings)

import Clash.Core.Alpha (aeqTy)
import Clash.Core.NameMap (globalEnvElems)
import Clash.Core.Syntax

import Compare
import SerialiseInstances (NetlistInputs, NormalizationInputs, readInputs)

-- | Run the round trip on every binding of a serialized design: inputs of
-- normalization (@*.bin@), of netlist generation (@*.net.bin@), or
-- normalized bindings (@*.norm.bin@). Prints the first @n@ differences, and
-- gives whether there are none.
roundTrip :: Int -> Int -> Int -> FilePath -> IO Bool
roundTrip n l w f = do
  bm <- readBindings f
  results <- forM (globalEnvElems bm) $ \b -> do
    let tm = bindingTerm b
        ty = globalIdType (bindingId b)
    r <- attempt "the round trip" $ evaluate $
      if aeqTy ty (decode (encode ty))
        then compareTerms tm (decode (encode tm))
        else Different (Crash "the round trip" "the type of the binder differs")
    pure (bindingId b, either Different id r)
  let different = [(i, m) | (i, Different m) <- results]
  mapM_ (\(i, m) -> putStr (renderMismatch ("before", "after") l w (nameOf i) m))
    (take n different)
  putStrLn $ f <> ": " <> show (length results) <> " bindings compared, "
    <> show (length results - length different) <> " equal, "
    <> show (length different) <> " different"
  hFlush stdout
  pure (null different)
 where
  nameOf = Text.unpack . C.nameOcc . globalIdName

-- | The bindings of a serialized design
readBindings :: FilePath -> IO GlobalBindings
readBindings f
  | ".net.bin" `List.isSuffixOf` f = do
      (bm, _, _, _, _, _, _) <- readInputs f :: IO NetlistInputs
      pure bm
  | ".norm.bin" `List.isSuffixOf` f = readInputs f
  | otherwise = do
      (bm, _, _, _, _, _, _, _) <- readInputs f :: IO NormalizationInputs
      pure bm
