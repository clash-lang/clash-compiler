{-# LANGUAGE TemplateHaskell #-}

module THCorePluginHelper (pluginValue) where

import Language.Haskell.TH.Syntax (addCorePlugin)
import Prelude

$(addCorePlugin "THCorePlugin" >> pure [])

-- OPAQUE ensures the importing splice executes this module's object code.
-- The Core plugin must replace this value with 42 before code generation.
pluginValue :: Int
pluginValue = 0
{-# OPAQUE pluginValue #-}
