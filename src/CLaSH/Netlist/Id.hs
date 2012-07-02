{-# LANGUAGE OverloadedStrings #-}
module CLaSH.Netlist.Id
  (mkBasicId)
where

import Data.Char      (isDigit,ord)
import Data.Text.Lazy as Text
import Numeric        (showHex)

mkBasicId ::
  Text
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
    go (Just (c,cs'))  = append (encode_digit_ch c) (go' $ uncons cs')
    go' Nothing        = empty
    go' (Just (c,cs')) = append (encode_ch c) (go' $ uncons cs')

encode_digit_ch :: Char -> EncodedString
encode_digit_ch c | c >= '0' && c <= '9' = encode_as_unicode_char c
encode_digit_ch c | otherwise            = encode_ch c

encode_ch :: Char -> EncodedString
encode_ch c | unencodedChar c = singleton c     -- Common case first

-- Constructors
encode_ch '['  = "ZM"
encode_ch ']'  = "ZN"
encode_ch ':'  = "ZC"

-- Variables
encode_ch '&'  = "za"
encode_ch '|'  = "zb"
encode_ch '^'  = "zc"
encode_ch '$'  = "zd"
encode_ch '='  = "ze"
encode_ch '>'  = "zg"
encode_ch '#'  = "zh"
encode_ch '.'  = "zi"
encode_ch '<'  = "zl"
encode_ch '-'  = "zm"
encode_ch '!'  = "zn"
encode_ch '+'  = "zp"
encode_ch '\'' = "zq"
encode_ch '\\' = "zr"
encode_ch '/'  = "zs"
encode_ch '*'  = "zt"
encode_ch '%'  = "zv"
encode_ch c    = encode_as_unicode_char c

encode_as_unicode_char :: Char -> EncodedString
encode_as_unicode_char c = cons 'z' (if isDigit (Text.head hex_str)
                                       then hex_str
                                       else cons '0' hex_str)
  where hex_str = pack $ showHex (ord c) "U"

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar c   =  c >= 'a' && c <= 'z'
                  || c >= 'A' && c <= 'Z'
                  || c >= '0' && c <= '9'
                  || c == '_'
