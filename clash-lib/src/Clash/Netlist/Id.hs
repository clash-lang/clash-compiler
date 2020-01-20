{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Transform/format a Netlist Identifier so that it is acceptable as a HDL identifier
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Netlist.Id
  ( IdType (..)
  , mkBasicId'
  , stripDollarPrefixes
  )
where

import Clash.Annotations.Primitive (HDL (..))
import Data.Char (isAsciiLower,isAsciiUpper,isDigit)
import Data.Text as Text

data IdType = Basic | Extended

mkBasicId'
  :: HDL
  -> Bool
  -> Text
  -> Text
mkBasicId' hdl tupEncode = stripMultiscore hdl . stripLeading hdl . zEncode hdl tupEncode
  where
    stripLeading VHDL = Text.dropWhile (`elem` ('_':['0'..'9']))
    stripLeading _    = Text.dropWhile (`elem` ('$':['0'..'9']))
    stripMultiscore VHDL
      = Text.concat
      . Prelude.map (\cs -> case Text.head cs of
                              '_' -> "_"
                              _   -> cs
                    )
      . Text.group
    stripMultiscore _ = id

stripDollarPrefixes :: Text -> Text
stripDollarPrefixes = stripWorkerPrefix . stripSpecPrefix . stripConPrefix
                    . stripWorkerPrefix . stripDictFunPrefix
  where
    stripDictFunPrefix t = case Text.stripPrefix "$f" t of
                             Just k  -> takeWhileEnd (/= '_') k
                             Nothing -> t

    stripWorkerPrefix t = case Text.stripPrefix "$w" t of
                              Just k  -> k
                              Nothing -> t

    stripConPrefix t = case Text.stripPrefix "$c" t of
                         Just k  -> k
                         Nothing -> t

    stripSpecPrefix t = case Text.stripPrefix "$s" t of
                          Just k -> k
                          Nothing -> t -- snd (Text.breakOnEnd "$s" t)


type UserString    = Text -- As the user typed it
type EncodedString = Text -- Encoded form

zEncode :: HDL -> Bool -> UserString -> EncodedString
zEncode hdl False cs = go (uncons cs)
  where
    go Nothing         = empty
    go (Just (c,cs'))  = append (encodeDigitCh hdl c) (go' $ uncons cs')
    go' Nothing        = empty
    go' (Just (c,cs')) = append (encodeCh hdl c) (go' $ uncons cs')

zEncode hdl True cs = case maybeTuple cs of
                    Just (n,cs') -> append n (go' (uncons cs'))
                    Nothing      -> go (uncons cs)
  where
    go Nothing         = empty
    go (Just (c,cs'))  = append (encodeDigitCh hdl c) (go' $ uncons cs')
    go' Nothing        = empty
    go' (Just (c,cs')) = case maybeTuple (cons c cs') of
                           Just (n,cs2) -> append n (go' $ uncons cs2)
                           Nothing      -> append (encodeCh hdl c) (go' $ uncons cs')

encodeDigitCh :: HDL -> Char -> EncodedString
encodeDigitCh _   c | isDigit c = Text.empty -- encodeAsUnicodeChar c
encodeDigitCh hdl c             = encodeCh hdl c

encodeCh :: HDL -> Char -> EncodedString
encodeCh hdl c | unencodedChar hdl c = singleton c     -- Common case first
               | otherwise           = Text.empty

unencodedChar :: HDL -> Char -> Bool   -- True for chars that don't need encoding
unencodedChar hdl c  =
  or [ isAsciiLower c
     , isAsciiUpper c
     , isDigit c
     , if hdl == VHDL then c == '_' else c `elem` ['_','$']
     ]

maybeTuple :: UserString -> Maybe (EncodedString,UserString)
maybeTuple "(# #)" = Just ("Unit",empty)
maybeTuple "()"    = Just ("Unit",empty)
maybeTuple (uncons -> Just ('(',uncons -> Just ('#',cs))) =
  case countCommas 0 cs of
    (n,uncons -> Just ('#',uncons -> Just (')',cs'))) -> Just (pack ("Tup" ++ show (n+1)),cs')
    _ -> Nothing
maybeTuple (uncons -> Just ('(',cs)) =
  case countCommas 0 cs of
    (n,uncons -> Just (')',cs')) -> Just (pack ("Tup" ++ show (n+1)),cs')
    _ -> Nothing
maybeTuple _  = Nothing

countCommas :: Int -> UserString -> (Int,UserString)
countCommas n (uncons -> Just (',',cs)) = countCommas (n+1) cs
countCommas n cs                        = (n,cs)
