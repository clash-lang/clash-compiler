{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Narrowing
  ( narrowingPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import GHC.Prim
import GHC.Types

import Clash.GHC.PartialEval.Internal

-- | Primitive Operations to narrow native sized Int / Word.
--
narrowingPrims :: HashMap Text PrimImpl
narrowingPrims = HashMap.fromList
  [ ("GHC.Prim.narrow8Int#", narrowInt narrow8Int#)
  , ("GHC.Prim.narrow16Int#", narrowInt narrow16Int#)
  , ("GHC.Prim.narrow32Int#", narrowInt narrow32Int#)
  , ("GHC.Prim.narrow8Word#", narrowWord narrow8Word#)
  , ("GHC.Prim.narrow16Word#", narrowWord narrow16Word#)
  , ("GHC.Prim.narrow32Word#", narrowWord narrow32Word#)
  ]

narrowInt :: (Int# -> Int#) -> PrimImpl
narrowInt f =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x in UInt (I# (f a))

narrowWord :: (Word# -> Word#) -> PrimImpl
narrowWord f =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in UWord (W# (f a))

