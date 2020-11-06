{-|
  Copyright  :  (C) 2020, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com
-}
{-# LANGUAGE OverloadedStrings #-}
module Clash.Netlist.Id.VHDL where

import Clash.Netlist.Id.Common

import           Control.Applicative ((<|>))
import qualified Data.Char as Char
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Maybe (isJust, fromMaybe)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import           Clash.Netlist.Types (IdentifierType(..))

-- | Identifiers which are imported from the following:
--
-- use IEEE.STD_LOGIC_1164.ALL;
-- use IEEE.NUMERIC_STD.ALL;
-- use IEEE.MATH_REAL.ALL;
-- use std.textio.all;
--
-- Clash should not use these identifiers, as it can lead to errors when
-- interfacing with an EDA tool.
--
-- See https://github.com/clash-lang/clash-compiler/issues/1439.
--
importedNames :: [Text]
importedNames =
  [ -- ieee.std_logic_1164.all
    "std_ulogic", "std_ulogic_vector", "resolved", "std_logic", "std_logic_vector"
  , "x01", "x01z", "ux01", "ux01z", "to_bit", "to_bitvector", "to_stdulogic"
  , "to_stdlogicvector", "to_stdulogicvector", "to_01", "to_x01", "to_x01z"
  , "to_ux01", "rising_edge", "falling_edge", "is_x"
    -- ieee.numeric_std.all
  , "unresolved_unsigned", "unresolved_signed", "u_unsigned", "u_signed"
  , "unsigned", "signed", "find_leftmost", "find_rightmost", "minimum"
  , "maximum", "shift_left", "shift_right", "rotate_left", "rotate_right"
  , "resize", "to_integer", "to_unsigned", "to_signed", "std_match"
    -- ieee.math_real.all
  , "math_e", "math_1_over_e", "math_pi", "math_2_pi", "math_1_over_pi"
  , "math_pi_over_2", "math_pi_over_3", "path_pi_over_4", "path_3_pi_over_2"
  , "math_log_of_2", "math_log_of_10", "math_log10_of_e", "math_sqrt_2"
  , "math_1_over_sqrt_2", "math_sqrt_pi", "math_deg_to_rad", "math_rad_to_deg"
  , "sign", "ceil", "floor", "round", "trunc", "realmax", "realmin", "uniform"
  , "sqrt", "cbrt", "exp", "log", "log2", "log10", "sin", "cos", "tan", "arcsin"
  , "arccos", "arctan", "sinh", "cosh", "tanh", "arcsinh", "arccosh", "arctanh"
    -- std.textio.all
  , "line", "text", "side", "width", "justify", "input", "output", "readline"
  , "read", "sread", "string_read", "bread", "binary_read", "oread", "octal_read"
  , "hread", "hex_read", "writeline", "tee", "write", "swrite", "string_write"
  , "bwrite", "binary_write", "owrite", "octal_write", "hwrite", "hex_write"
  ]

-- | Time units: are added to 'reservedWords' as simulators trip over signals
-- named after them.
timeUnits :: [Text]
timeUnits = ["fs", "ps", "ns", "us", "ms", "sec", "min", "hr"]

-- List of reserved VHDL-2008 keywords
-- + used internal names: toslv, fromslv, tagtoenum, datatotag
-- + used IEEE library names: integer, boolean, std_logic, std_logic_vector,
--   signed, unsigned, to_integer, to_signed, to_unsigned, string
keywords :: HashSet Text
keywords = HashSet.fromList $
  ["abs","access","after","alias","all","and","architecture"
  ,"array","assert","assume","assume_guarantee","attribute","begin","block"
  ,"body","buffer","bus","case","component","configuration","constant","context"
  ,"cover","default","disconnect","downto","else","elsif","end","entity","exit"
  ,"fairness","file","for","force","function","generate","generic","group"
  ,"guarded","if","impure","in","inertial","inout","is","label","library"
  ,"linkage","literal","loop","map","mod","nand","new","next","nor","not","null"
  ,"of","on","open","or","others","out","package","parameter","port","postponed"
  ,"procedure","process","property","protected","pure","range","record"
  ,"register","reject","release","rem","report","restrict","restrict_guarantee"
  ,"return","rol","ror","select","sequence","severity","signal","shared","sla"
  ,"sll","sra","srl","strong","subtype","then","to","transport","type"
  ,"unaffected","units","until","use","variable","vmode","vprop","vunit","wait"
  ,"when","while","with","xnor","xor","toslv","fromslv","tagtoenum","datatotag"
  ,"integer", "boolean", "std_logic", "std_logic_vector", "signed", "unsigned"
  ,"to_integer", "to_signed", "to_unsigned", "string","log"] <> timeUnits <> importedNames

isKeyword :: Text -> Bool
isKeyword t = HashSet.member (Text.toLower t) keywords

parseBasic :: Text -> Bool
parseBasic id0 = parseBasic' id0 && not (isKeyword id0)

parseBasic' :: Text -> Bool
parseBasic' id0 = isJust $ do
  id1 <- parseLetter id0
  id2 <- repeatParse parseGroup id1
  failNonEmpty id2
 where
  parseGroup s0 = do
    s1 <- parseUnderscore s0 <|> Just s0
    s2 <- parseLetterOrDigit s1
    repeatParse parseLetterOrDigit s2

parseExtended :: Text -> Bool
parseExtended id0 = isJust $ do
  id1 <- parseBackslash id0
  id2 <- parse id1
  id3 <- parseBackslash id2
  failNonEmpty id3
 where
  parse s0 =
    case parseBackslash s0 of
      Just s1 -> parseBackslash s1 >>= repeatParse parse
      Nothing -> parsePrintable s0 >>= repeatParse parse

toBasic :: Text -> Text
toBasic =
    replaceKeywords
  . stripMultiscore
  . Text.dropWhileEnd (=='_')
  . Text.dropWhile (\c -> c == '_' || Char.isDigit c)
  . zEncode isBasicChar
  . stripDollarPrefixes
--  . Text.toLower
 where
  replaceKeywords i
    | isKeyword i = "r_" <> i
    | otherwise = i

  stripMultiscore =
      Text.concat
    . Prelude.map (\cs -> case Text.head cs of {'_' -> "_"; _ -> cs})
    . Text.group

isBasicChar :: Char -> Bool
isBasicChar c = or
  [ Char.isAsciiLower c
  , Char.isAsciiUpper c
  , Char.isDigit c
  , c == '_'
  ]

stripDollarPrefixes :: Text -> Text
stripDollarPrefixes = stripWorkerPrefix . stripSpecPrefix . stripConPrefix
                    . stripWorkerPrefix . stripDictFunPrefix
  where
    stripDictFunPrefix t =
      maybe t (Text.takeWhileEnd (/='_')) (Text.stripPrefix "$f" t)
    stripWorkerPrefix t = fromMaybe t (Text.stripPrefix "$w" t)
    stripConPrefix t = fromMaybe t (Text.stripPrefix "$c" t)
    stripSpecPrefix t = fromMaybe t (Text.stripPrefix "$s" t)

unextend :: Text -> Text
unextend =
     Text.strip
   . (\t -> fromMaybe t (Text.stripPrefix "\\" t))
   . (\t -> fromMaybe t (Text.stripSuffix "\\" t))
   . Text.strip

toText :: IdentifierType -> Text -> Text
toText Basic t = t
toText Extended t = "\\" <> t <> "\\"
