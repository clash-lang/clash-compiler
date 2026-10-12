{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Restore the functions of serialized primitives (see
  'SerialiseInstances.removeBBfunc'), so that Netlist generation can call them.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module RestorePrimitives
  ( restorePrimMap
  ) where

import qualified Data.HashMap.Strict          as HashMap
import qualified Data.List                    as List
import qualified Data.Text                    as Text

import           Clash.Annotations.Primitive  (PrimitiveGuard (..))
import           Clash.Backend                (initBackend, primDirs)
import           Clash.Backend.VHDL           (VHDLState)
import           Clash.Driver                 (knownBlackBoxFunctions, knownTemplateFunctions)
import           Clash.Driver.Types           (defClashOpts)
import           Clash.Netlist.Types          (BlackBox (..))
import           Clash.Primitives.Types
  (BlackBoxFunctionName (..), CompiledPrimMap, CompiledPrimitive, Primitive (..),
   ResolvedPrimMap, TemplateFormat (..))
import           Clash.Primitives.Util        (generatePrimMap)

import           SerialiseInstances           (CompiledPrimMap', unremoveBBfunc)

-- | Restore the functions of serialized primitives, if they are among the
-- functions that Clash knows without interpreting them (see
-- 'Clash.Driver.knownBlackBoxFunctions' and
-- 'Clash.Driver.knownTemplateFunctions'). The others error when called. A
-- compiled template function only keeps the name of its primitive, so the
-- names of template functions come from the primitive definitions of Clash,
-- for VHDL, the HDL that the primitives were serialized for.
--
-- Also gives the names of the primitives whose blackbox functions can't be
-- restored.
restorePrimMap :: CompiledPrimMap' -> IO (CompiledPrimMap, [Text.Text])
restorePrimMap prims = do
  resolved <- generatePrimMap [] [] =<< primDirs (initBackend @VHDLState defClashOpts)
  let restored = fmap (fmap (restorePrimitive resolved . unremoveBBfunc)) prims
      missing =
        [ name
        | HasBlackBox _ (BlackBoxHaskell {name, functionName = BlackBoxFunctionName mods fun})
            <- HashMap.elems prims
        , not (HashMap.member (qualName mods fun) knownBlackBoxFunctions) ]
  pure (restored, missing)

restorePrimitive :: ResolvedPrimMap -> CompiledPrimitive -> CompiledPrimitive
restorePrimitive resolved = \case
  p@BlackBoxHaskell {functionName = BlackBoxFunctionName mods fun}
    | Just f <- HashMap.lookup (qualName mods fun) knownBlackBoxFunctions ->
        -- Serialization removed the hash of the function too
        p {function = (0, f)}
  p@BlackBox {name}
    | Just (HasBlackBox _ r@BlackBox {}) <- HashMap.lookup name resolved -> p
        { includes = zipWith (\(_, a) (k, b) -> (k, restore a b)) (includes r) (includes p)
        , resultNames = zipWith restore (resultNames r) (resultNames p)
        , resultInits = zipWith restore (resultInits r) (resultInits p)
        , template = restore (template r) (template p)
        }
  p -> p
 where
  restore ((THaskell, BlackBoxFunctionName mods fun), _) (BBFunction nm h _)
    | Just tf <- HashMap.lookup (qualName mods fun) knownTemplateFunctions = BBFunction nm h tf
  restore _ bb = bb

qualName :: [String] -> String -> String
qualName mods fun = List.intercalate "." (mods ++ [fun])
