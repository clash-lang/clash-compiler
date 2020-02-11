{-|
Copyright  :  (C) 2019, QBayLogic B.V.
                  2013, Nikita Volkov
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-
This is an adaptation of

  https://github.com/nikita-volkov/neat-interpolation/tree/0fc1dd73ea

which is licensed under MIT. The original license will follow.

---------

Copyright (c) 2013, Nikita Volkov

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Util.Interpolate where

import           Language.Haskell.Meta.Parse (parseExp)
import           Language.Haskell.TH.Lib     (appE, varE)
import           Language.Haskell.TH.Quote   (QuasiQuoter(..))
import           Language.Haskell.TH.Syntax  (Q, Exp)

import qualified Numeric                as N
import           Data.Char
  (isHexDigit, chr, isOctDigit, isDigit, isSpace)
import           Data.Maybe             (fromMaybe, isJust, catMaybes)
import           Text.Read              (readMaybe)

data Line
  = EmptyLine
  | ExprLine Int String
  | Line Int [Node]
  deriving (Show)

data Node
  = Literal String
  | Expression String
  deriving (Show)

type Indent = Int

format :: [Node] -> String
format = stripWhiteSpace . showLines . nodesToLines
 where
  go _ [] = []
  go n (c:cs) | c == ' ' = go (n+1) cs
  go 0 (c:cs) = c : go 0 cs
  go n cs = replicate n ' ' ++ (go 0 cs)

  stripWhiteSpace = go 0 . dropWhile isSpace


showLines :: [Line] -> String
showLines [] = ""
showLines ns = init (concatMap showLine ns)
 where
  showLine :: Line -> String
  showLine EmptyLine = "\n"
  showLine (Line n ns') =
    let theIndent = replicate (n - commonIndent) ' ' in
    theIndent ++ (concatMap nodeToString ns') ++ "\n"
  showLine (ExprLine n s) =
    let theIndent = replicate (n - commonIndent) ' ' in
    concat [theIndent ++ l ++ "\n" | l <- lines s]

  nodeToString :: Node -> String
  nodeToString (Literal s) = s
  nodeToString (Expression s) = s

  commonIndent :: Int
  commonIndent = foldl1 min (catMaybes (map indent ns))

  indent :: Line -> Maybe Int
  indent EmptyLine = Nothing
  indent (ExprLine n _) = Just n
  indent (Line n _) = Just n

-- | Collects nodes into lines. Expressions might still contain newlines! Does
-- not start or end with 'EmptyLine'.
nodesToLines :: [Node] -> [Line]
nodesToLines =
    concatMap splitLines
  . mergeLines
  . dropEmpty
  . map splitWords
  . map toLine
  . map dropTrailingEmpty
  . collectLines []
  . joinLiterals
 where
  emptyLit (Literal s) =
    if all isSpace s then
      Just (length s)
    else
      Nothing
  emptyLit _ = Nothing

  isEmptyLine EmptyLine = True
  isEmptyLine _ = False

  dropEmpty = reverse . dropWhile isEmptyLine . reverse . dropWhile isEmptyLine
  dropTrailingEmpty = reverse . dropWhile (isJust . emptyLit) . reverse

  splitLines :: Line -> [Line]
  splitLines EmptyLine = [EmptyLine]
  splitLines e@(ExprLine {}) = [e]
  splitLines (Line n nodes) = map (Line n) (go 0 [] nodes)
   where
    maxLength = 80

    go :: Int -> [Node] -> [Node] -> [[Node]]
    go accLen acc goNodes | accLen > maxLength = reverse acc : go 0 [] goNodes
    go accLen acc (l@(Literal s):goNodes) = go (accLen + length s) (l:acc) goNodes
    go accLen acc (e@(Expression s):goNodes) = go (accLen + length s) (e:acc) goNodes
    go _accLen acc [] = [reverse acc]

  mergeLines :: [Line] -> [Line]
  mergeLines (l0@(Line n0 nodes0):l1@(Line n1 nodes1):ls) =
    if n0 == n1 then
      mergeLines (Line n0 (nodes0 ++ [Literal " "] ++ nodes1) : ls)
    else
      l0:mergeLines (l1:ls)
  mergeLines (l:ls) = l:mergeLines ls
  mergeLines [] = []

  splitWords :: Line -> Line
  splitWords EmptyLine = EmptyLine
  splitWords e@(ExprLine {})= e
  splitWords (Line n nodes) = Line n (concatMap go nodes)
   where
    go (Expression s) = [Expression s]
    go (Literal "") = []
    go (Literal s0) =
      let
        pre = takeWhile (not . (==' ')) s0
        post = dropWhile (not . (== ' ')) s0
      in case post of
        [] -> [Literal s0]
        (_:s1) -> Literal (pre ++ " ") : go (Literal s1)

  -- Convert to 'Line' type
  toLine = \case
    [] -> EmptyLine
    [emptyLit -> Just _] -> EmptyLine
    [Expression s] -> ExprLine 0 s
    [emptyLit -> Just n, Expression s] -> ExprLine n s
    ns@(Expression _:_) -> Line 0 ns
    (Literal s:ns) ->
      Line
        (length (takeWhile (==' ') s))
        (Literal (dropWhile (==' ') s):ns)

  -- collects list of nodes, where each list is a single line
  collectLines collected todo =
    case (collected, todo) of
      ([], []) -> []
      (_, []) -> [reverse collected]
      (_, s@(Expression _):ns) ->
        collectLines (s:collected) ns
      (_, Literal s0:ns) ->
        let
          pre = takeWhile (/= '\n') s0
          post = dropWhile (/= '\n') s0
        in case post of
          [] ->
            collectLines (Literal s0:collected) ns
          (_:s1) ->
            reverse (Literal pre:collected) : collectLines [] (Literal s1:ns)

  joinLiterals :: [Node] -> [Node]
  joinLiterals [] = []
  joinLiterals (Literal s0:Literal s1:ss) = joinLiterals (Literal (s0 ++ s1):ss)
  joinLiterals (n:ns) = n:joinLiterals ns

i :: QuasiQuoter
i = QuasiQuoter {
    quoteExp = (varE 'format `appE`) . toExp . parseNodes . decodeNewlines
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name =
      error ("Clash.Util.Interpolate.i: This QuasiQuoter can not be used as a "
           ++ name ++ "!")

    toExp:: [Node] -> Q Exp
    toExp nodes = case nodes of
      [] -> [|[]|]
      (x:xs) -> f x `appE` toExp xs
      where
        f (Literal s) = [|(Literal s:)|]
        f (Expression e) = [|(Expression (toString ($(reifyExpression e))):)|]

        reifyExpression :: String -> Q Exp
        reifyExpression s = case parseExp s of
          Left _ -> do
            fail "Parse error in expression!" :: Q Exp
          Right e -> return e

-------------------------------------------------------------------
-- Everything below this line is unchanged from neat-interpolate --
-------------------------------------------------------------------
decodeNewlines :: String -> String
decodeNewlines = go
  where
    go xs = case xs of
      '\r' : '\n' : ys -> '\n' : go ys
      y : ys -> y : go ys
      [] -> []

parseNodes :: String -> [Node]
parseNodes = go ""
  where
    go :: String -> String -> [Node]
    go acc input = case input of
      ""  -> [(lit . reverse) acc]
      '\\':x:xs -> go (x:'\\':acc) xs
      '#':'{':xs -> case span (/= '}') xs of
        (ys, _:zs) -> (lit . reverse) acc : Expression ys : go "" zs
        (_, "") -> [lit (reverse acc ++ input)]
      x:xs -> go (x:acc) xs

    lit :: String -> Node
    lit = Literal . unescape

toString :: Show a => a -> String
toString a = let s = show a in fromMaybe s (readMaybe s)
{-# NOINLINE toString #-}
{-# RULES "toString/String" toString = id #-}
{-# RULES "toString/Int" toString = show :: Int -> String #-}
{-# RULES "toString/Integer" toString = show :: Integer -> String #-}
{-# RULES "toString/Float" toString = show :: Float -> String #-}
{-# RULES "toString/Double" toString = show :: Double -> String #-}

-- Haskell 2010 character unescaping, see:
-- http://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6
unescape :: String -> String
unescape = go
  where
    go input = case input of
      "" -> ""
      '\\' : 'x' : x : xs | isHexDigit x -> case span isHexDigit xs of
        (ys, zs) -> (chr . readHex $ x:ys) : go zs
      '\\' : 'o' : x : xs | isOctDigit x -> case span isOctDigit xs of
        (ys, zs) -> (chr . readOct $ x:ys) : go zs
      '\\' : x : xs | isDigit x -> case span isDigit xs of
        (ys, zs) -> (chr . read $ x:ys) : go zs
      '\\' : input_ -> case input_ of
        '\\' : xs -> '\\' : go xs
        'a' : xs -> '\a' : go xs
        'b' : xs -> '\b' : go xs
        'f' : xs -> '\f' : go xs
        'n' : xs -> '\n' : go xs
        'r' : xs -> '\r' : go xs
        't' : xs -> '\t' : go xs
        'v' : xs -> '\v' : go xs
        '&' : xs -> go xs
        'N':'U':'L' : xs -> '\NUL' : go xs
        'S':'O':'H' : xs -> '\SOH' : go xs
        'S':'T':'X' : xs -> '\STX' : go xs
        'E':'T':'X' : xs -> '\ETX' : go xs
        'E':'O':'T' : xs -> '\EOT' : go xs
        'E':'N':'Q' : xs -> '\ENQ' : go xs
        'A':'C':'K' : xs -> '\ACK' : go xs
        'B':'E':'L' : xs -> '\BEL' : go xs
        'B':'S' : xs -> '\BS' : go xs
        'H':'T' : xs -> '\HT' : go xs
        'L':'F' : xs -> '\LF' : go xs
        'V':'T' : xs -> '\VT' : go xs
        'F':'F' : xs -> '\FF' : go xs
        'C':'R' : xs -> '\CR' : go xs
        'S':'O' : xs -> '\SO' : go xs
        'S':'I' : xs -> '\SI' : go xs
        'D':'L':'E' : xs -> '\DLE' : go xs
        'D':'C':'1' : xs -> '\DC1' : go xs
        'D':'C':'2' : xs -> '\DC2' : go xs
        'D':'C':'3' : xs -> '\DC3' : go xs
        'D':'C':'4' : xs -> '\DC4' : go xs
        'N':'A':'K' : xs -> '\NAK' : go xs
        'S':'Y':'N' : xs -> '\SYN' : go xs
        'E':'T':'B' : xs -> '\ETB' : go xs
        'C':'A':'N' : xs -> '\CAN' : go xs
        'E':'M' : xs -> '\EM' : go xs
        'S':'U':'B' : xs -> '\SUB' : go xs
        'E':'S':'C' : xs -> '\ESC' : go xs
        'F':'S' : xs -> '\FS' : go xs
        'G':'S' : xs -> '\GS' : go xs
        'R':'S' : xs -> '\RS' : go xs
        'U':'S' : xs -> '\US' : go xs
        'S':'P' : xs -> '\SP' : go xs
        'D':'E':'L' : xs -> '\DEL' : go xs
        '^':'@' : xs -> '\^@' : go xs
        '^':'A' : xs -> '\^A' : go xs
        '^':'B' : xs -> '\^B' : go xs
        '^':'C' : xs -> '\^C' : go xs
        '^':'D' : xs -> '\^D' : go xs
        '^':'E' : xs -> '\^E' : go xs
        '^':'F' : xs -> '\^F' : go xs
        '^':'G' : xs -> '\^G' : go xs
        '^':'H' : xs -> '\^H' : go xs
        '^':'I' : xs -> '\^I' : go xs
        '^':'J' : xs -> '\^J' : go xs
        '^':'K' : xs -> '\^K' : go xs
        '^':'L' : xs -> '\^L' : go xs
        '^':'M' : xs -> '\^M' : go xs
        '^':'N' : xs -> '\^N' : go xs
        '^':'O' : xs -> '\^O' : go xs
        '^':'P' : xs -> '\^P' : go xs
        '^':'Q' : xs -> '\^Q' : go xs
        '^':'R' : xs -> '\^R' : go xs
        '^':'S' : xs -> '\^S' : go xs
        '^':'T' : xs -> '\^T' : go xs
        '^':'U' : xs -> '\^U' : go xs
        '^':'V' : xs -> '\^V' : go xs
        '^':'W' : xs -> '\^W' : go xs
        '^':'X' : xs -> '\^X' : go xs
        '^':'Y' : xs -> '\^Y' : go xs
        '^':'Z' : xs -> '\^Z' : go xs
        '^':'[' : xs -> '\^[' : go xs
        '^':'\\' : xs -> '\^\' : go xs
        '^':']' : xs -> '\^]' : go xs
        '^':'^' : xs -> '\^^' : go xs
        '^':'_' : xs -> '\^_' : go xs
        xs -> go xs
      x:xs -> x : go xs

    readHex :: String -> Int
    readHex xs = case N.readHex xs of
      [(n, "")] -> n
      _ -> error "Data.String.Interpolate.Util.readHex: no parse"

    readOct :: String -> Int
    readOct xs = case N.readOct xs of
      [(n, "")] -> n
      _ -> error "Data.String.Interpolate.Util.readHex: no parse"

