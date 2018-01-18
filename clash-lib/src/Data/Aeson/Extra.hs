{-|
  Copyright   :  (C) 2015-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module Data.Aeson.Extra where

import qualified Data.Ix              as Ix
import qualified Data.Text            as T
import           Data.Text            (Text,pack,unpack)
import           Data.List            (intercalate)
import           Data.Aeson           (FromJSON, Result (..), fromJSON, json)
import           Data.Attoparsec.Lazy (Result (..), parse)
import           Data.ByteString.Lazy (ByteString)
import           System.FilePath      ()

-- Quick and dirty way of replacing fake escapes in naively converted bytestring
replaceCommonEscapes :: Text -> Text
replaceCommonEscapes = ( T.replace (pack "\\n") (pack "\n") ) .
                       ( T.replace (pack "\\\\") (pack "\\") ) .
                       ( T.replace (pack "\\\"") (pack "\"") )

genLineErr' :: [Text] -> (Int, Int) -> Int -> Text
genLineErr' allLines range errorLineN = T.unlines [ T.concat [ if i == errorLineN then pack ">> " else  pack "   "
                                                             , pack $ show i
                                                             , pack ". "
                                                             , allLines !! i
                                                             ] | i <- Ix.range range]

-- | Pretty print part of json file related to error
genLineErr :: ByteString -> ByteString -> Text
genLineErr full part = genLineErr' allLines interval errorLineN
  where
    -- Determine interval, and pass to helper function
    nLastLines = 1 + (length $ T.lines $ replaceCommonEscapes $ pack $ show part)
    errorLineN = length allLines - nLastLines + 1
    allLines   = T.lines $ replaceCommonEscapes $ pack $ show full
    interval   = (max 0 (errorLineN - 5), min (max 0 $ length allLines - 1) (errorLineN + 5))

-- | Parse a ByteString according to the given JSON template. Prints failures
-- on @stdout@, and returns 'Nothing' if parsing fails.
decodeOrErr :: (FromJSON a)
                => FilePath
                -> ByteString -- ^ Bytestring to parse
                -> Maybe a
decodeOrErr path contents =
  case parse json contents of
    Done _ v -> case fromJSON v of
                    Success a -> Just a
                    Error msg -> error ("Could not deduce valid scheme for '" ++ show path ++ "'. Error was: \n\n" ++ msg)

    -- JSON parse error:
    Fail bytes cntxs msg -> error ( "Could not read or parse " ++ show path ++ ". "
                                 ++ (if null cntxs then "" else "Context was:\n  " ++ intercalate "\n  " cntxs)
                                 ++ "\n\nError reported by Attoparsec was:\n  "
                                 ++ msg
                                 ++ "\n\nApproximate location of error:\n\n"
                                 -- HACK: Replace with proper parser/fail logic in future. Or don't. It's not important.
                                 ++ (unpack $ genLineErr contents bytes)
                                 )
