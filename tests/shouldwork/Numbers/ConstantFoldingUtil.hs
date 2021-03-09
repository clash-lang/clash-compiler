{-# LANGUAGE ViewPatterns #-}

module ConstantFoldingUtil (lit,mainVHDL,mainVerilog,mainSystemVerilog) where
import Clash.Prelude

import Data.Char (toLower,toUpper)
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup ((<>))
import System.Directory
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import System.Exit
import System.IO

import Text.Parser.Combinators
import Text.Trifecta
import Text.Trifecta.Delta

import Debug.Trace

-- | Prevent GHC from constant folding operations. Clash should be able to
-- do it though.
lit
  :: Num a
  => Signed 64
  -> a
lit = fromIntegral

---------------
-- output tests
---------------
-- Any number found in the HDL between [22000,22999] is considered unfolded
-- and is reported as an error.
mainVHDL = checkForUnfolded vhdlNr
mainVerilog = checkForUnfolded verilogNr
mainSystemVerilog = checkForUnfolded verilogNr

-- | Implements @glob (topDir </> "*.topEntity" </> "topEntity.{vhdl,sv,v}")@.
-- We can't use glob because 'topDir' contains spaces.
findTopEntity :: FilePath -> IO FilePath
findTopEntity topDir = do
  [topLib] <- filter (".topEntity" `L.isSuffixOf`) <$> listDirectory topDir
  topFiles <- listDirectory (topDir </> topLib)
  let
    exts = [".vhdl", ".sv", ".v"]
    isTopEnt f = "topEntity" `L.isPrefixOf` f && any (`L.isSuffixOf` f) exts
    [topFile] = filter isTopEnt topFiles
  pure (topDir </> topLib </> topFile)

checkForUnfolded nrParser = do
  [topDir] <- getArgs
  content <- readFile =<< findTopEntity topDir
  case parseString (allNrs nrParser) mempty content of
    Failure err -> die ("Parsing failed with: " <> show err)
    Success nrs -> case filter isUnfolded nrs of
                     [] -> hPutStrLn stderr ("Checked: " <> show (L.length nrs) <> " literals")
                     unfolded -> die ("Error: found unfolded constants:" <> show unfolded)

readDec,readHex,readBin :: String -> Integer
readDec = read
readHex = read . ("0x" <>)
readBin = go 0
  where
    go x [] = x
    go x ('0':bs) = go (x*2) bs
    go x ('1':bs) = go (x*2+1) bs

data Nr = Dec String
        | Hex String
        | Bin String
        deriving Show

nrValue :: Nr -> Integer
nrValue nr = case nr of
  Bin ds -> readBin ds
  Dec ds -> readDec ds
  Hex ds -> readHex ds

decNr = Dec <$> some digit

binDigit = satisfyRange '0' '1'

verilogBinNr = char '\'' *> skipOptional (chari 's') *> chari 'b' *> (Bin <$> some binDigit)
verilogHexNr = char '\'' *> skipOptional (chari 's') *> chari 'h' *> (Hex <$> some hexDigit)
verilogNr = choice [verilogBinNr, verilogHexNr, decNr]

vhdlHexNr = chari 'x' *> char '"' *> (Hex <$> some hexDigit) <* char '"'
vhdlBinNr = char '"' *> (Hex <$> some binDigit) <* char '"'
vhdlNr = choice [vhdlHexNr, vhdlBinNr, decNr]

-- | Parse char case-insensitive
chari c = oneOf [toLower c, toUpper c]

isUnfolded :: Nr -> Bool
isUnfolded (nrValue -> n) = 22000 <= n && n < 23000

allNrs :: CharParsing m => m Nr -> m [Nr]
allNrs nrParser = catMaybes <$> many maybeNr <* eof
  where
    maybeNr = Just <$> try nrParser
              <|> const Nothing <$> anyChar
