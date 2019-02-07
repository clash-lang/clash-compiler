{-|
  Copyright  :  (C) 2019, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Blackbox generation for GHC.Word.WordX# data constructors.
-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Primitives.GHC.Word where

import qualified Data.Text.Lazy               as LT
import           Data.Text
  (Text, stripPrefix, stripSuffix, unpack)
import           Text.Read                    (readMaybe)
import           TextShow                     (showtl)

import           Clash.Core.Literal           (Literal(WordLiteral))
import           Clash.Core.Term              (Term(Literal))
import           Clash.Core.Type              (Type)
import           Clash.Netlist.Types          (BlackBox(BBTemplate))
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, Element(Text, Arg, Result), emptyBlackBoxMeta
  ,BlackBoxMeta, bbKind, TemplateKind(TDecl))

unsigned :: Element -> [Element]
unsigned el = [Text "$unsigned(", el, Text ")"]

assign :: Element -> [Element] -> [Element]
assign lhs rhs = Text "assign " : lhs : Text " = " : rhs ++ [Text ";"]

literal :: Int -> Integer -> Element
literal wordSize wordVal =
  Text (LT.concat [showtl wordSize, "'d", showtl wordVal])

-- | Parse integer in strings of the form "GHC.Word.WordX#"
readWordSize :: Text -> Maybe Int
readWordSize nm0 = do
  nm1 <- stripPrefix "GHC.Word.W" nm0
  nm2 <- stripSuffix "#" nm1
  readMaybe (unpack nm2)

-- | Template function for Word8/Word16/.. Constructs "clean" literals.
wordTF :: BlackBoxFunction
wordTF isDecl _resId primName args _resTy =
  case readWordSize primName of
    Nothing ->
      Left "Can only make blackboxes for 'GHC.Word.WordX#'"
    Just n ->
      Right (wordTF' isDecl args n)

wordTF'
  :: Bool
  -- ^ Is declaration
  -> [Either Term Type]
  -- ^ Arguments
  -> Int
  -- ^ Word size
  -> (BlackBoxMeta, BlackBox)
wordTF' False [Left (Literal (WordLiteral n))] wordSize =
  -- Literal as expression:
  ( emptyBlackBoxMeta
  , BBTemplate (unsigned (literal wordSize n)))

wordTF' True [Left (Literal (WordLiteral n))] wordSize =
  -- Literal as declaration:
  ( emptyBlackBoxMeta
  , BBTemplate (assign (Result False) [literal wordSize n]))

wordTF' _isDecl _args _wordSize =
  -- Not a literal. We need an assignment as Verilog does not support truncating
  -- arbitrary expression.
  ( emptyBlackBoxMeta {bbKind = TDecl }
  , BBTemplate (assign (Result False) (unsigned (Arg False 0))))
