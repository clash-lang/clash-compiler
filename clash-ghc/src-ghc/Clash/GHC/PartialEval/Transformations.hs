{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Transformations
  ( transformationsPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Types (Int(..))

import Clash.GHC.PartialEval.Internal

transformationsPrims :: HashMap Text PrimImpl
transformationsPrims = HashMap.fromList
  [ ("EmptyCase", liftId)
  , ("Clash.Transformations.eqInt", primEqInt)
  , ("Clash.Transformations.removedArg", liftId)
  , ("Clash.Transformations.undefined", liftId)
  , ("Clash.Transformations.ref", liftId)

    -- Not transformations, but must be saved from defaulting to core.
  , ("Clash.Signal.Internal.register#", liftId)
  , ("Clash.NFDataX.deepErrorX", liftId)
  , ("Clash.NFDataX.rnfx", liftId)
  , ("Clash.NFDataX.hasUndefined", liftId)
  , ("Clash.NFDataX.ensureSpine", liftId)
  , ("Clash.XException.errorX", liftId)
  , ("Clash.XException.seqX", liftId)
  , ("Clash.XException.deepseqX", liftId)
  , ("Clash.XException.hwSeqX", liftId)
  ]

primEqInt :: PrimImpl
primEqInt =
  liftBinary $ \x y ->
    let !(I# _) = x
        !(I# _) = y
     in x == y

