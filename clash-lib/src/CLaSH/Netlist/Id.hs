{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Transform/format a Netlist Identifier so that it is acceptable as a VHDL identifier
module CLaSH.Netlist.Id
  ( mkBasicId
  , mkBasicId'
  , stripDollarPrefixes
  )
where

import Data.Char      (isAsciiLower,isAsciiUpper,isDigit,ord)
import Data.Text.Lazy as Text
import Numeric        (showHex)

-- | Transform/format a text so that it is acceptable as a VHDL identifier
mkBasicId :: Text
          -> Text
mkBasicId = mkBasicId' False

mkBasicId' :: Bool
           -> Text
           -> Text
mkBasicId' tupEncode = stripMultiscore . stripLeading . zEncode tupEncode
  where
    stripLeading    = Text.dropWhile (`elem` ('_':['0'..'9']))
    stripMultiscore = Text.concat
                    . Prelude.map (\cs -> case Text.head cs of
                                            '_' -> "_"
                                            _   -> cs
                                  )
                    . Text.group

stripDollarPrefixes :: Text -> Text
stripDollarPrefixes = stripSpecPrefix . stripConPrefix
                    . stripWorkerPrefix . stripDictFunPrefix
  where
    stripDictFunPrefix t = case Text.stripPrefix "$f" t of
                             Just k  -> takeWhileEnd (/= '_') k
                             Nothing -> t

    takeWhileEnd p = Text.reverse . Text.takeWhile p . Text.reverse

    stripWorkerPrefix t = case Text.stripPrefix "$w" t of
                              Just k  -> k
                              Nothing -> t

    stripConPrefix t = case Text.stripPrefix "$c" t of
                         Just k  -> k
                         Nothing -> t

    stripSpecPrefix t = snd (Text.breakOnEnd "$s" t)


type UserString    = Text -- As the user typed it
type EncodedString = Text -- Encoded form

zEncode :: Bool -> UserString -> EncodedString
zEncode False cs = go (uncons cs)
  where
    go Nothing         = empty
    go (Just (c,cs'))  = append (encodeDigitCh c) (go' $ uncons cs')
    go' Nothing        = empty
    go' (Just (c,cs')) = append (encodeCh c) (go' $ uncons cs')

zEncode True cs = case maybeTuple cs of
                    Just (n,cs') -> append n (go' (uncons cs'))
                    Nothing      -> go (uncons cs)
  where
    go Nothing         = empty
    go (Just (c,cs'))  = append (encodeDigitCh c) (go' $ uncons cs')
    go' Nothing        = empty
    go' (Just (c,cs')) = case maybeTuple (cons c cs') of
                           Just (n,cs2) -> append n (go' $ uncons cs2)
                           Nothing      -> append (encodeCh c) (go' $ uncons cs')

encodeDigitCh :: Char -> EncodedString
encodeDigitCh c | isDigit c = encodeAsUnicodeChar c
encodeDigitCh c             = encodeCh c

encodeCh :: Char -> EncodedString
encodeCh c | unencodedChar c = singleton c     -- Common case first

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
encodeAsUnicodeChar c = cons 'z' (if isDigit (Text.head hex_str)
                                    then hex_str
                                    else cons '0' hex_str)
  where hex_str = pack $ showHex (ord c) "U"

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar c  = or [ isAsciiLower c
                      , isAsciiUpper c
                      , isDigit c
                      , c == '_']

maybeTuple :: UserString -> Maybe (EncodedString,UserString)
maybeTuple "(# #)" = Just ("Z1H",empty)
maybeTuple "()"    = Just ("Z0T",empty)
maybeTuple (uncons -> Just ('(',uncons -> Just ('#',cs))) =
  case countCommas 0 cs of
    (n,uncons -> Just ('#',uncons -> Just (')',cs'))) -> Just (pack ('Z':shows (n+1) "H"),cs')
    _ -> Nothing
maybeTuple (uncons -> Just ('(',cs)) =
  case countCommas 0 cs of
    (n,uncons -> Just (')',cs')) -> Just (pack ('Z':shows (n+1) "T"),cs')
    _ -> Nothing
maybeTuple _  = Nothing

countCommas :: Int -> UserString -> (Int,UserString)
countCommas n (uncons -> Just (',',cs)) = countCommas (n+1) cs
countCommas n cs                        = (n,cs)
