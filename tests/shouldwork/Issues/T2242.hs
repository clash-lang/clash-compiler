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

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile $ topDir </> show 'topEntity </> "topEntity.v"
  if '$' `elem` content then error "Error: output contains $ symbol" else pure ()
