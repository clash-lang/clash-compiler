{-# LANGUAGE OverloadedStrings #-}

module Clash.Netlist.Id.Common where

import           Control.Arrow (first)
import           Control.Applicative ((<|>))
import           Control.Applicative.Extra (orEmpty)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import           TextShow (showt)

parseWhiteSpace :: Text -> Maybe Text
parseWhiteSpace = parseSingle isWhiteSpace

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` [' ', '\n', '\t']

parsePrintable :: Text -> Maybe Text
parsePrintable = parseSingle (\c -> Char.isPrint c && Char.isAscii c)

parseSingle :: (Char -> Bool) -> Text -> Maybe Text
parseSingle predicate s = do
  (l, ls) <- Text.uncons s
  orEmpty (predicate l) ls

parseMaybeSingle :: (Char -> Bool) -> Text -> Maybe Text
parseMaybeSingle predicate s = Just (fromMaybe s (parseSingle predicate s))

parseLetter :: Text -> Maybe Text
parseLetter = parseSingle (\c -> Char.isAscii c && Char.isLetter c)

parseDigit :: Text -> Maybe Text
parseDigit = parseSingle Char.isDigit

parseLetterOrDigit :: Text -> Maybe Text
parseLetterOrDigit s = parseLetter s <|> parseDigit s

parseUnderscore :: Text -> Maybe Text
parseUnderscore = parseSingle (=='_')

parseDollar :: Text -> Maybe Text
parseDollar = parseSingle (=='$')

parseTab :: Text -> Maybe Text
parseTab = parseSingle (=='\t')

parseBackslash :: Text -> Maybe Text
parseBackslash = parseSingle (=='\\')

failNonEmpty :: Text -> Maybe Text
failNonEmpty s | Text.null s = Just Text.empty
               | otherwise = Nothing

repeatParseN :: (Text -> Maybe Text) -> Text -> Maybe (Int, Text)
repeatParseN parser = go 0
 where
  go n s0 =
    case parser s0 of
      Just s1 -> go (n+1) s1
      Nothing -> Just (n, s0)

repeatParse :: (Text -> Maybe Text) -> Text -> Maybe Text
repeatParse parser s0 = snd <$> repeatParseN parser s0

-- | Encodes tuples as "TupN" and removes all characters not matching a
-- predicate.
zEncode
  :: (Char -> Bool)
  -- ^ Characters to keep
  -> Text
  -> Text
zEncode keep s =
  let go = zEncode keep in
  case maybeTuple s of
    Just (tupName, rest) ->
      tupName <> go rest
    Nothing ->
      case Text.uncons s of
        Just (c, rest) ->
          if keep c then
            Text.cons c (go rest)
          else
            go rest
        Nothing -> s

prettyName :: Text -> Text
prettyName t = maybe t (uncurry (<>)) (maybeTuple t)

maybeTuple :: Text -> Maybe (Text, Text)
maybeTuple "(# #)" = Just ("Unit", "")
maybeTuple "()" = Just ("Unit", "")
maybeTuple t = first (\n -> "Tup" <> showt n) <$> parseTuple t

parseTuple :: Text -> Maybe (Int, Text)
parseTuple t0 = do
  t1 <- parseSingle (=='(') t0
  t2 <- parseMaybeSingle (=='#') t1
  (n, t3) <- repeatParseN (parseSingle (== ',')) t2
  t4 <- parseMaybeSingle (=='#') t3
  t5 <- parseSingle (==')') t4
  pure (n+1, t5)

