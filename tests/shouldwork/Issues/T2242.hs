module T2242 where
import System.Environment
import System.FilePath
import Clash.Prelude

topEntity ::
  HiddenClock System =>
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
topEntity = dflipflop
{-# NOINLINE topEntity #-}

checkContent :: String -> IO ()
checkContent content
  | '$' `elem` content = error "Error: output contains $ symbol"
  | otherwise = pure ()

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content  <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")
  checkContent content

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content  <- readFile (topDir </> show 'topEntity </> "topEntity.v")
  checkContent content

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topDir] <- getArgs
  content  <- readFile (topDir </> show 'topEntity </> "topEntity.sv")
  checkContent content
