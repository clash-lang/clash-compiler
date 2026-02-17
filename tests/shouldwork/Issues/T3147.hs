module T3147 where

import Clash.Explicit.Prelude
import Data.Char (isAlphaNum, isSpace)
import Data.List (intercalate, isPrefixOf, nub, sort)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified Prelude as P

delayedCounter ::
  forall dom d n.
  (KnownDomain dom, KnownNat d, KnownNat n, NFDataX (Unsigned n)) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  SNat d ->
  Signal dom (Unsigned n)
delayedCounter clk rst ena d = last $ iterate (succSNat d) delayStep cnt
 where
  regStep = register clk rst ena 0
  delayStep = delay clk ena 0
  cnt = regStep (cnt + 1)

topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Unsigned 8)
topEntity clk rst ena = delayedCounter clk rst ena d3

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.v")
  let contentLines = fmap trimLeft (P.lines content)
  let regs = sortedUniq $ mapMaybe regDecl contentLines
  let assigns = sortedUniq $ mapMaybe seqAssignLhs contentLines
  assertSeqAssignsAreRegs regs assigns content

trimLeft :: String -> String
trimLeft = P.dropWhile isSpace

dropOptionalWidth :: String -> String
dropOptionalWidth s =
  case trimLeft s of
    '[':xs ->
      let (_, rest) = P.break (== ']') xs
       in case rest of
            [] -> s
            (_:more) -> trimLeft more
    other -> other

dropOptionalSigned :: String -> String
dropOptionalSigned s =
  let s' = trimLeft s
   in if "signed" `isPrefixOf` s'
        then trimLeft (P.drop (P.length ("signed" :: String)) s')
        else s'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '$'

takeIdent :: String -> String
takeIdent = P.takeWhile isIdentChar . trimLeft

sortedUniq :: [String] -> [String]
sortedUniq = sort . nub

regDecl :: String -> Maybe String
regDecl line =
  case line of
    s
      | "reg" `isPrefixOf` s
      , let after = P.drop 3 s
      , case after of
          [] -> True
          (c:_) -> isSpace c || c == '[' ->
          let rest0 = trimLeft after
              rest1 = dropOptionalSigned rest0
              rest2 = dropOptionalWidth rest1
              declPart = P.takeWhile (/= ';') rest2
              ident = takeIdent declPart
           in if P.null ident then Nothing else Just ident
      | otherwise -> Nothing

dropOptionalIndex :: String -> String
dropOptionalIndex s =
  case trimLeft s of
    '[':xs ->
      let (_, rest) = P.break (== ']') xs
       in case rest of
            [] -> s
            (_:more) -> trimLeft more
    other -> trimLeft other

seqAssignLhs :: String -> Maybe String
seqAssignLhs line =
  let (ident, rest) = P.span isIdentChar line
      rest' = dropOptionalIndex rest
   in if P.null ident
        then Nothing
        else if "<=" `isPrefixOf` rest'
          then Just ident
          else Nothing

assertSeqAssignsAreRegs :: [String] -> [String] -> String -> IO ()
assertSeqAssignsAreRegs regs assigns verilog =
  let bad = P.filter (`P.notElem` regs) assigns
   in if P.null bad
        then return ()
        else
          P.error $
            P.concat
              [ "Sequential assignment to non-reg(s): "
              , intercalate ", " bad
              , "\nRegs: "
              , intercalate ", " regs
              , "\nSequential assignments: "
              , intercalate ", " assigns
              , "\nVerilog:\n\n"
              , verilog
              ]
