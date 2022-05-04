{-|
  Copyright   :  (C) 2021-2022, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  VHDL Blackbox implementations for "Clash.Sized.Internal.Signed.toInteger#".
-}

{-# LANGUAGE OverloadedStrings #-}
module Clash.Primitives.Sized.Signed (fromIntegerTFvhdl) where

import Control.Monad.State (State)
import Data.Monoid (Ap(getAp))
import Data.Text.Prettyprint.Doc.Extra (Doc, tupled)

import Clash.Backend (Backend, expr)
import Clash.Netlist.Ast.Type (HWType(..))
import Clash.Netlist.Types
  (BlackBoxContext (..), Expr (..), Literal (..), Modifier (..),
   TemplateFunction (..))

fromIntegerTFvhdl :: TemplateFunction
fromIntegerTFvhdl = TemplateFunction used valid fromIntegerTFTemplateVhdl
 where
  used = [0,1]
  valid bbCtx = case bbInputs bbCtx of
    [kn,_] -> case kn of
      (Literal _ (NumLit _),_,True) -> True
      _ -> False
    _ -> False

fromIntegerTFTemplateVhdl
  :: Backend s
  => BlackBoxContext
  -> State s Doc
fromIntegerTFTemplateVhdl bbCtx = getAp $ do
  let [(Literal _ (NumLit sz),_,_), (i, Signed szI, _)] = bbInputs bbCtx
  case compare sz (toInteger szI) of
    LT -> case i of
           Identifier iV m ->
            let sl = Sliced (Signed szI,fromInteger sz-1,0)
                m1 = Just (maybe sl (`Nested` sl) m)
            in expr False (Identifier iV m1)
           _ -> "signed(std_logic_vector(resize(unsigned(std_logic_vector(" <> expr False i <> "))," <> expr False (Literal Nothing (NumLit sz)) <> ")))"
    EQ -> expr False i
    GT -> "resize" <> tupled (sequenceA [expr False i
                                        ,expr False (Literal Nothing (NumLit sz))])
