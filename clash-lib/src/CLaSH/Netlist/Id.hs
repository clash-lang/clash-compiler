{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Transform/format a Netlist Identifier so that it is acceptable as a HDL identifier
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE ViewPatterns      #-}

module CLaSH.Netlist.Id
  -- ( mkBasicId
  -- , mkBasicId'
  -- , stripDollarPrefixes
  -- )
  ( mkBasicId'
  , stripDollarPrefixes
  )
where

import Data.Char      (isAsciiLower,isAsciiUpper,isDigit,ord)
import Data.List      (group, stripPrefix, uncons)
import Numeric        (showHex)

-- | Transform/format a text so that it is acceptable as a HDL identifier
-- mkBasicId :: Text
--           -> Text
-- mkBasicId = mkBasicId' False

mkBasicId' :: Bool
           -> String
           -> String
mkBasicId' tupEncode = stripMultiscore . stripLeading . zEncode tupEncode
  where
    stripLeading    = dropWhile (`elem` ('_':['0'..'9']))
    stripMultiscore = concat
                    . map (\cs -> case head cs of
                                            '_' -> "_"
                                            _   -> cs
                                  )
                    . group

stripDollarPrefixes :: String -> String
stripDollarPrefixes = stripWorkerPrefix . stripSpecPrefix . stripConPrefix
                    . stripWorkerPrefix . stripDictFunPrefix
  where
    stripDictFunPrefix t = case stripPrefix "$f" t of
                             Just k  -> takeWhileEnd' (/= '_') k
                             Nothing -> t

    takeWhileEnd' p = reverse . takeWhile p . reverse

    stripWorkerPrefix t = case stripPrefix "$w" t of
                              Just k  -> k
                              Nothing -> t

    stripConPrefix t = case stripPrefix "$c" t of
                         Just k  -> k
                         Nothing -> t

    stripSpecPrefix t = case stripPrefix "$s" t of
                          Just k -> k
                          Nothing -> t

type UserString    = String -- As the user typed it
type EncodedString = String -- Encoded form

zEncode :: Bool -> UserString -> EncodedString
zEncode False cs = go (uncons cs)
  where
    go Nothing         = ""
    go (Just (c,cs'))  = (encodeDigitCh c) ++ (go' $ uncons cs')
    go' Nothing        = ""
    go' (Just (c,cs')) = (encodeCh c) ++ (go' $ uncons cs')

zEncode True cs = case maybeTuple cs of
                    Just (n,cs') -> n ++ (go' (uncons cs'))
                    Nothing      -> go (uncons cs)
  where
    go Nothing         = ""
    go (Just (c,cs'))  = (encodeDigitCh c) ++ (go' $ uncons cs')
    go' Nothing        = ""
    go' (Just (c,cs')) = case maybeTuple (c:cs') of
                           Just (n,cs2) -> n ++ (go' $ uncons cs2)
                           Nothing      -> (encodeCh c) ++ (go' $ uncons cs')

encodeDigitCh :: Char -> EncodedString
encodeDigitCh c | isDigit c = encodeAsUnicodeChar c
encodeDigitCh c             = encodeCh c

encodeCh :: Char -> EncodedString
encodeCh c | unencodedChar c = [c]     -- Common case first

-- Constructors
encodeCh '['  = "ZM"
encodeCh ']'  = "ZN"
encodeCh ':'  = "ZC"

-- Variables
encodeCh '&'  = "za"
encodeCh '|'  = "zb"
encodeCh '^'  = "zc"
encodeCh '$'  = "zd"
encodeCh '='  = "ze"
encodeCh '>'  = "zf"
encodeCh '#'  = "zg"
encodeCh '.'  = "zh"
encodeCh '<'  = "zu"
encodeCh '-'  = "zj"
encodeCh '!'  = "zk"
encodeCh '+'  = "zl"
encodeCh '\'' = "zm"
encodeCh '\\' = "zn"
encodeCh '/'  = "zo"
encodeCh '*'  = "zp"
encodeCh '%'  = "zq"
encodeCh c    = encodeAsUnicodeChar c

encodeAsUnicodeChar :: Char -> EncodedString
encodeAsUnicodeChar c = 'z':(if isDigit (head hex_str)
                                then hex_str
                                else '0':hex_str)
  where hex_str = showHex (ord c) "U"

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar c  = or [ isAsciiLower c
                      , isAsciiUpper c
                      , isDigit c
                      , c == '_']

maybeTuple :: UserString -> Maybe (EncodedString,UserString)
maybeTuple "(# #)" = Just ("Z1H","")
maybeTuple "()"    = Just ("Z0T","")
maybeTuple (uncons -> Just ('(',uncons -> Just ('#',cs))) =
  case countCommas 0 cs of
    (n,uncons -> Just ('#',uncons -> Just (')',cs'))) -> Just (('Z':shows (n+1) "H"),cs')
    _ -> Nothing
maybeTuple (uncons -> Just ('(',cs)) =
  case countCommas 0 cs of
    (n,uncons -> Just (')',cs')) -> Just (('Z':shows (n+1) "T"),cs')
    _ -> Nothing
maybeTuple _  = Nothing

countCommas :: Int -> UserString -> (Int,UserString)
countCommas n (uncons -> Just (',',cs)) = countCommas (n+1) cs
countCommas n cs                        = (n,cs)
