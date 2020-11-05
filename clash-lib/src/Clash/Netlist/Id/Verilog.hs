{-|
  Copyright  :  (C) 2020, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com
-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Netlist.Id.Verilog where

import           Control.Applicative ((<|>))
import qualified Data.Char as Char
import           Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import           Clash.Netlist.Id.Common
import           Clash.Netlist.Types (IdentifierType(..))

-- List of reserved Verilog-2005 keywords
keywords :: HashSet Text
keywords = HashSet.fromList
  ["always","and","assign","automatic","begin","buf","bufif0"
  ,"bufif1","case","casex","casez","cell","cmos","config","deassign","default"
  ,"defparam","design","disable","edge","else","end","endcase","endconfig"
  ,"endfunction","endgenerate","endmodule","endprimitive","endspecify"
  ,"endtable","endtask","event","for","force","forever","fork","function"
  ,"generate","genvar","highz0","highz1","if","ifnone","incdir","include"
  ,"initial","inout","input","instance","integer","join","large","liblist"
  ,"library","localparam","macromodule","medium","module","nand","negedge"
  ,"nmos","nor","noshowcancelled","not","notif0","notif1","or","output"
  ,"parameter","pmos","posedge","primitive","pull0","pull1","pulldown","pullup"
  ,"pulsestyle_onevent","pulsestyle_ondetect","rcmos","real","realtime","reg"
  ,"release","repeat","rnmos","rpmos","rtran","rtranif0","rtranif1","scalared"
  ,"showcancelled","signed","small","specify","specparam","strong0","strong1"
  ,"supply0","supply1","table","task","time","tran","tranif0","tranif1","tri"
  ,"tri0","tri1","triand","trior","trireg","unsigned","use","uwire","vectored"
  ,"wait","wand","weak0","weak1","while","wire","wor","xnor","xor"]

isKeyword :: Text -> Bool
isKeyword t = HashSet.member (Text.toLower t) keywords

parseBasic :: Text -> Bool
parseBasic id0 = parseBasic' id0 && not (isKeyword id0)

parseBasic' :: Text -> Bool
parseBasic' id0 = isJust $ do
  id1 <- parseUnderscore id0 <|> parseLetter id0
  id2 <- repeatParse parseAllowedChars id1
  failNonEmpty id2
 where
  parseAllowedChars s =
        parseLetterOrDigit s
    <|> parseUnderscore s
    <|> parseDollar s

parseExtended :: Text -> Bool
parseExtended id0 =
  isJust ((parse id0 >>= failNonEmpty) >> parseEnd id0)
 where
  -- Extended identifier must start with backslash, followed by printable chars
  parse s = parseBackslash s >>= repeatParse parsePrintable

  -- Extended identifier must end in exactly one whitespace
  parseEnd :: Text -> Maybe Text
  parseEnd s =
    case Text.unpack (Text.takeEnd 2 s) of
      [c0, c1] | not (isWhiteSpace c0) && isWhiteSpace c1 -> Just ""
      _ -> Nothing

toBasic' :: Text -> Text
toBasic' (zEncode isBasicChar -> t) =
  case Text.uncons t of
    Just (c, _) | Char.isDigit c || c == '$' -> Text.cons '_' t
    _ -> t

toBasic :: Text -> Text
toBasic (toBasic' -> t) =
  if HashSet.member (Text.toLower t) keywords then "r_" <> t else t

isBasicChar :: Char -> Bool
isBasicChar c = or
  [ Char.isAsciiLower c
  , Char.isAsciiUpper c
  , Char.isDigit c
  , c == '_'
  , c == '$'
  ]

unextend :: Text -> Text
unextend =
     Text.strip
   . (\t -> fromMaybe t (Text.stripPrefix "\\" t))
   . Text.strip

toText :: IdentifierType -> Text -> Text
toText Basic t = t
toText Extended t = "\\" <> t <> " "
