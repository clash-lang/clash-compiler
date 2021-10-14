{-|
  Copyright   :  (C) 2015-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Extra where

import           Control.Exception    (throw)
import qualified Data.IntMap          as IntMap
import           Data.IntMap          (IntMap)
import qualified Data.Ix              as Ix
import qualified Data.Text            as T
import           Data.Text            (Text,pack,unpack)
import qualified Data.Text.Lazy       as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Text.Encoding.Error (UnicodeException(..))
import           Data.List            (intercalate)
import           Data.List.Extra      (groupOn)
import           Data.Tuple.Extra     (second, first)
import           Data.Aeson           (FromJSON, Result (..), fromJSON, json)
import           Data.Attoparsec.Lazy (Result (..), parse)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSChar
import           System.FilePath      ()

import qualified Clash.Util.Interpolate as I
import           Clash.Util           (ClashException(..))
#if MIN_VERSION_ghc(9,0,0)
import           GHC.Types.SrcLoc     (mkGeneralSrcSpan)
import           GHC.Data.FastString  (mkFastString)
#else
import           SrcLoc               (mkGeneralSrcSpan)
import           FastString           (mkFastString)
#endif
import           GHC.Stack            (HasCallStack)

-- | See 'toSpecNewlines'. A line map maps "virtual" lines to a range of
-- "real" lines. E.g., a map of {0: (0, 3), 1: (4, 5)} would mean that line
-- 0 in the virtual JSON (i.e., the one with newlines replaced) file map to
-- lines 0 up to and including 3 in the original user-supplied one.
type LineMap = IntMap (Int, Int)

-- | Aeson versions <1.5.0 accept unescaped newlines in JSON strings. This is in
-- violation of RFC 7159. Aeson 1.5.0 fixes this bug. Unfortunately, "Clash
-- JSON" files rely on the old behavior. This function replaces newlines (in
-- strings) with their escaped variants.
toSpecNewlines
  :: ByteString
  -> Either UnicodeException (LineMap, ByteString)
toSpecNewlines bs = do
 s0 <- LT.unpack <$> LT.decodeUtf8' bs
 Right ( toLineMap (go2 0 False s0)
       , LT.encodeUtf8 (LT.pack (go False s0)))
 where
  -- replace newlines with escaped ones
  go :: Bool -> String -> String
  go _ [] = []
  go True ('\n':rest) = '\\' : 'n' : go True rest
  go True ('\r':rest) = '\\' : 'r' : go True rest
  go True ('\t':rest) = '\\' : 't' : go True rest
  go inString ('\\':r:rest) = '\\' : r : go inString rest
  go inString ('"':rest) = '"' : go (not inString) rest
  go inString (r:rest) = r : go inString rest

  -- Calculate real:virtual mapping
  go2
    -- virtual line counter.
    :: Int
    -- Processing a JSON string?
    -> Bool
    -- String left to process
    -> String
    -- Virtual line numbers. [0, 1, 1, 2, 2, ..] would mean:
    --
    --  real | virtual
    --  --------------
    --     0 |       0
    --     1 |       1
    --     2 |       1
    --     3 |       2
    --     4 |       2
    --   ... |     ...
    --
    -> [Int]
  go2 n _        [] = [n]
  go2 n True     ('\n':rest) = n : go2 n True rest
  go2 n False    ('\n':rest) = n : go2 (succ n) False rest
  go2 n inString ('\\':_:rest) = go2 n inString rest
  go2 n inString ('"':rest) = go2 n (not inString) rest
  go2 n inString (_:rest) = go2 n inString rest

  toLineMap :: [Int] -> LineMap
  toLineMap virtuals =
      IntMap.fromList
    $ map (second (\reals -> (minimum reals, maximum reals)))
    $ map (first head . unzip)
    $ groupOn fst
    $ zip virtuals [(0::Int)..]

genLineErr' :: [Text] -> (Int, Int) -> [Int] -> Text
genLineErr' allLines range errorLines =
  T.unlines [ T.concat [ if elem i errorLines then pack ">> " else  pack "   "
            , pack $ show (i + 1)
            , pack ". "
            , allLines !! i
            ] | i <- Ix.range range]

-- | Pretty print part of json file related to error
genLineErr :: LineMap -> ByteString -> ByteString -> ByteString -> Text
genLineErr lineMap fullOrig full part =
    genLineErr' allLinesOrig interval [errorLineMin..errorLineMax]
  where
    -- Determine error line in "virtual" json file
    nLastLines = 1 + (length $ LT.lines $ LT.decodeUtf8 part)
    errorLineN = min (length allLines - 1) (length allLines - nLastLines + 1)
    allLines   = T.lines $ LT.toStrict $ LT.decodeUtf8 full

    -- Convert to error lines in actual json file, and calculate interval
    -- to display to user.
    allLinesOrig = T.lines $ LT.toStrict $ LT.decodeUtf8 fullOrig
    (errorLineMin, errorLineMax) = lineMap IntMap.! errorLineN
    interval = ( max 0 (errorLineMin - 5)
               , min (max 0 $ length allLinesOrig - 1) (errorLineMax + 5) )

-- | Parse a ByteString according to the given JSON template. Throws exception
-- if it fails.
decodeOrErr
  :: (HasCallStack, FromJSON a)
  => FilePath
  -- ^ Path read from (for error message)
  -> ByteString
  -- ^ Bytestring to parse
  -> a
decodeOrErr path contents0 =
  case toSpecNewlines contents0 of
    Left (DecodeError err _) -> clashError [I.i|
      Failed to decode JSON file as UTF8:

        #{path}

      Decoder reported:

        #{err}
    |]
    Left _ -> error "unreachable"
    Right (!lineMap, !contents1) ->
      case parse json contents1 of
        Done leftover v ->
          case fromJSON v of
            Success _ | BS.any notWhitespace leftover ->
              clashError ("After parsing " ++  show path
                     ++ ", found unparsed trailing garbage:\n"
                     ++ BSChar.unpack leftover)
            Success a ->
              a
            Error msg ->
              clashError
                ( "Could not deduce valid scheme for json in "
               ++ show path ++ ". Error was: \n\n" ++ msg )

        -- JSON parse error:
        Fail bytes cntxs msg ->
          clashError
            ( "Could not read or parse json in " ++ show path ++ ". "
           ++ (if null cntxs then "" else "Context was:\n  " ++ intercalate "\n  " cntxs)
           ++ "\n\nError reported by Attoparsec was:\n  "
           ++ msg
           ++ "\n\nApproximate location of error:\n\n"
           -- HACK: Replace with proper parser/fail logic in future. Or don't. It's not important.
           ++ (unpack $ genLineErr lineMap contents0 contents1 bytes) )
  where
    loc = mkGeneralSrcSpan $ mkFastString path
    clashError msg = throw $ ClashException loc msg Nothing
    notWhitespace c = BS.notElem c whitespace
      where whitespace = BSChar.pack " \t\n\r"
