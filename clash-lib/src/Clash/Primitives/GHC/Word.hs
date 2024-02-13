{-|
  Copyright  :  (C) 2019, Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Blackbox generation for GHC.Word.WordX# data constructors. (System)Verilog only!
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Primitives.GHC.Word (wordTF) where

import           Clash.Core.Literal           (Literal(..))
import           Clash.Core.Term              (Term(Literal))
import           Clash.Core.Type              (Type)
import           Clash.Primitives.GHC.Literal
  (literalTF, unsigned, unsignedLiteral, assign)
import           Clash.Netlist.Types          (BlackBox(BBTemplate))
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, Element(Arg, Result), emptyBlackBoxMeta
  ,BlackBoxMeta, bbKind, TemplateKind(TDecl))

-- | Template function for Word8,Word16,.. Constructs "clean" literals. This
-- function generates valid (System)Verilog only!
wordTF :: BlackBoxFunction
wordTF = literalTF "GHC.Word.W" wordTF'

getWordLit
  :: Literal
  -> Maybe Integer
getWordLit =
  \case
    WordLiteral i   -> Just i
    Word8Literal i  -> Just i
    Word16Literal i -> Just i
    Word32Literal i -> Just i
    Word64Literal i -> Just i
    _               -> Nothing

wordTF'
  :: Bool
  -- ^ Is declaration
  -> [Either Term Type]
  -- ^ Arguments
  -> Int
  -- ^ Word size
  -> (BlackBoxMeta, BlackBox)
wordTF' False [Left (Literal (getWordLit -> Just n))] wordSize =
  -- Literal as expression:
  ( emptyBlackBoxMeta
  , BBTemplate [unsignedLiteral wordSize n])

wordTF' True [Left (Literal (getWordLit -> Just n))] wordSize =
  -- Literal as declaration:
  ( emptyBlackBoxMeta
  , BBTemplate (assign Result [unsignedLiteral wordSize n]))

wordTF' _isDecl _args _wordSize =
  -- Not a literal. We need an assignment as Verilog does not support truncating
  -- arbitrary expression.
  ( emptyBlackBoxMeta {bbKind = TDecl }
  , BBTemplate (assign Result (unsigned (Arg 0))))
