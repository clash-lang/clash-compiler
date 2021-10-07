{-# LANGUAGE OverloadedStrings #-}

module Clash.Primitives.ROM.File
  ( romStringTF
  ) where

import Control.Monad.State (State)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import Data.String (fromString)

import Clash.Backend (Backend)
import Clash.Netlist.Types
  (BlackBoxContext(..), Expr(..), Literal(..), TemplateFunction(..))

romStringTF
  :: TemplateFunction
romStringTF = TemplateFunction used valid romStringTemplate
 where
  used = [5]
  valid = const True

romStringTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
romStringTemplate bbCtx = pure bbText
 where
  (BlackBoxE "GHC.CString.unpackCString#" [] [] [] _ contentBBCtx _, _, _) =
    bbInputs bbCtx !! 5
  (Literal Nothing (StringLit content), _, _) = head $ bbInputs contentBBCtx

  bbText = fromString $ content
