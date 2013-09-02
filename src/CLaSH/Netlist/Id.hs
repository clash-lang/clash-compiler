{-# LANGUAGE OverloadedStrings #-}
module CLaSH.Netlist.Id
  (mkBasicId)
where

import Data.Char      (isAsciiLower,isAsciiUpper,isDigit,ord)
import Data.Text.Lazy as Text
import Numeric        (showHex)

-- | Transform/format a text so that it is acceptable as a VHDL identifier
mkBasicId :: Text
          -> Text
mkBasicId = stripMultiscore . stripLeading . zEncode
  where
    stripLeading    = Text.dropWhile (`elem` ['0'..'9'])
    stripMultiscore = Text.concat
                    . Prelude.map (\cs -> case Text.head cs of
                                            '_' -> "_"
                                            _   -> cs
                                  )
                    . Text.group

type UserString    = Text -- As the user typed it
type EncodedString = Text -- Encoded form

zEncode :: UserString -> EncodedString
zEncode cs = go (uncons cs)
  where
    go Nothing         = empty
    go (Just (c,cs'))  = append (encodeDigitCh c) (go' $ uncons cs')
    go' Nothing        = empty
    go' (Just (c,cs')) = append (encodeCh c) (go' $ uncons cs')

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
