module Multiple where

import Prelude
import Clash.Prelude (defSyn)
import System.Environment (getArgs)
import System.FilePath (splitFileName)
import System.Directory (listDirectory)
import Data.List (sort)

import Debug.Trace

topEntity1 :: Int
topEntity1 = 11111111
{-# ANN topEntity1 (defSyn "topentity1") #-}
{-# NOINLINE topEntity1 #-}

topEntity2 :: Int
topEntity2 = topEntity1
{-# ANN topEntity2 (defSyn "topentity2") #-}
{-# NOINLINE topEntity2 #-}

topEntity3 :: Int
topEntity3 = topEntity2
{-# NOINLINE topEntity3 #-}

-- | Make sure topEntity2 is not compiled when -main-is topEntity1
main1SystemVerilog :: IO ()
main1SystemVerilog = do
  [(dir, _fname)] <- map splitFileName <$> getArgs
  files <- listDirectory dir
  if any (`elem` files) [show 'topEntity2, show 'topEntity3] then
    error ("Unexpected files / directories: " ++ show files)
  else
    pure ()

-- | Check whether we can compile a binder that doesn't have a synthesize
-- annotation _and_ isn't called 'topEntity' using -main-is.
main3VHDL :: IO ()
main3VHDL = do
  [dir] <- getArgs
  files <- listDirectory dir
  let expected = [show 'topEntity1, show 'topEntity2, show 'topEntity3]
  if all (`elem` files) expected then
    pure ()
  else
    error ("Unexpected files / directories: " ++ show (sort files))
