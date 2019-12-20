{-|
  Copyright  :  (C) 2019, Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Blackbox generation for GHC.Int.IntX# data constructors. (System)Verilog only!
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Primitives.GHC.Int (intTF) where

import           Clash.Core.Literal
  (Literal(IntegerLiteral, IntLiteral, Int64Literal))
import           Clash.Core.Term              (Term(Literal))
import           Clash.Core.Type              (Type)
import           Clash.Primitives.GHC.Literal
  (literalTF, signed, signedLiteral, assign)
import           Clash.Netlist.Types          (BlackBox(BBTemplate))
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, Element(Arg, Result), emptyBlackBoxMeta
  ,BlackBoxMeta, bbKind, TemplateKind(TDecl))

getIntLit
  :: Literal
  -> Maybe Integer
getIntLit =
  \case
    IntegerLiteral i -> Just i
    IntLiteral i     -> Just i
    Int64Literal i   -> Just i
    _                -> Nothing

-- | Template function for Int8,Int16,.. Constructs "clean" literals.
intTF :: BlackBoxFunction
intTF = literalTF "GHC.Int.I" intTF'

intTF'
  :: Bool
  -- ^ Is declaration
  -> [Either Term Type]
  -- ^ Arguments
  -> Int
  -- ^ Word size
  -> (BlackBoxMeta, BlackBox)
intTF' False [Left (Literal (getIntLit -> Just n))] intSize =
  -- Literal as expression:
  ( emptyBlackBoxMeta
  , BBTemplate [signedLiteral intSize n])

intTF' True [Left (Literal (getIntLit -> Just n))] intSize =
  -- Literal as declaration:
  ( emptyBlackBoxMeta
  , BBTemplate (assign (Result False) [signedLiteral intSize n]))

intTF' _isDecl _args _intSize =
  -- Not a literal. We need an assignment as Verilog does not support truncating
  -- arbitrary expression.
  ( emptyBlackBoxMeta {bbKind = TDecl }
  , BBTemplate (assign (Result False) (signed (Arg False 0))))
