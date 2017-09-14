{-|
  Copyright  :  (C) 2012-2016, University of Twente
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Transform/format a Netlist Identifier so that it is acceptable as a HDL identifier
-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Netlist.Id
  ( IdType (..)
  , mkBasicId'
  , stripDollarPrefixes
  )
where

#ifndef MIN_VERSION_text
#error MIN_VERSION_text undefined
#endif

import Data.Char      (isAsciiLower,isAsciiUpper,isDigit)
import Data.Text.Lazy as Text

data IdType = Basic | Extended

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
stripDollarPrefixes = stripWorkerPrefix . stripSpecPrefix . stripConPrefix
                    . stripWorkerPrefix . stripDictFunPrefix
  where
    stripDictFunPrefix t = case Text.stripPrefix "$f" t of
                             Just k  -> takeWhileEnd (/= '_') k
                             Nothing -> t

#if !MIN_VERSION_text(1,2,2)
    takeWhileEnd p = Text.reverse . Text.takeWhile p . Text.reverse
#endif

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
encodeDigitCh c | isDigit c = Text.empty -- encodeAsUnicodeChar c
encodeDigitCh c             = encodeCh c

encodeCh :: Char -> EncodedString
encodeCh c | unencodedChar c = singleton c     -- Common case first
           | otherwise       = Text.empty

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar c  = or [ isAsciiLower c
                      , isAsciiUpper c
                      , isDigit c
                      , c == '_']

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
