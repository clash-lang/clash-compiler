-- | Without @-fclash-split-registers@, registers of product types are rendered
-- as a single register. See "SplitRegister".
module SplitRegisterOff where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Explicit.Prelude

topEntity ::
  Clock System -> Reset System -> Enable System ->
  Signal System (Unsigned 4) -> Signal System (Unsigned 4, Unsigned 4)
topEntity clk rst en x = s
 where
  s = register clk rst en (0, 0) (bundle (x, fmap fst s))

-- | Check that there's exactly one register
assertOneRegister :: String -> IO ()
assertOneRegister content =
  case P.length (P.filter ("register begin" `isInfixOf`) (P.lines content)) of
    1 -> return ()
    n -> P.error ("Expected one register, but found " P.<> show n P.<> " in:\n\n" P.<> content)

readHdl :: FilePath -> IO String
readHdl file = do
  [topDir] <- getArgs
  readFile (topDir </> show 'topEntity </> file)

mainVerilog :: IO ()
mainVerilog = readHdl "topEntity.v" >>= assertOneRegister

mainSystemVerilog :: IO ()
mainSystemVerilog = readHdl "topEntity.sv" >>= assertOneRegister

mainVHDL :: IO ()
mainVHDL = readHdl "topEntity.vhdl" >>= assertOneRegister
