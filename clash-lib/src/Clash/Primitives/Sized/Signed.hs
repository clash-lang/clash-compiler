{-|
  Copyright   :  (C) 2021 QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  VHDL Blackbox implementations for "Clash.Sized.Internal.Signed.toInteger#".
-}

{-# LANGUAGE OverloadedStrings #-}
module Clash.Primitives.Sized.Signed (fromIntegerTF) where

import Control.Monad.State (State)
import Data.Monoid (Ap(getAp))
import Data.Text.Prettyprint.Doc.Extra (Doc, tupled)

import Clash.Backend (Backend, expr)
import Clash.Netlist.Types
  (BlackBoxContext (..), Expr (..), HWType (..), Literal (..), Modifier (..),
   TemplateFunction (..))

fromIntegerTF :: TemplateFunction
fromIntegerTF = TemplateFunction used valid fromIntegerTFTemplate
 where
  used = [0,1]
  valid bbCtx = case bbInputs bbCtx of
    [(Literal _ (NumLit _),_,True),v] -> case v of
      (Identifier {}, Signed {}, False) -> True
      (Literal {}, _, True) -> True
      _ -> error (show (bbInputs bbCtx))
    _ -> error (show (bbInputs bbCtx))

fromIntegerTFTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
fromIntegerTFTemplate bbCtx = getAp $ do
  let [(Literal _ (NumLit sz),_,_), (i@(Identifier iV m), Signed szI, _)] = bbInputs bbCtx
  case compare sz (toInteger szI) of
    LT -> let sl = Sliced (Signed szI,fromInteger sz-1,0)
              m1 = Just (maybe sl (`Nested` sl) m)
           in expr False (Identifier iV m1)
    EQ -> expr False i
    GT -> "resize" <> tupled (sequenceA [expr False i
                                        ,expr False (Literal Nothing (NumLit sz))])
