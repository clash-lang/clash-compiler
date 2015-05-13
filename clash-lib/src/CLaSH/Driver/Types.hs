-- | Type definitions used by the Driver module
module CLaSH.Driver.Types where

import Data.HashMap.Lazy (HashMap)

import CLaSH.Core.Term   (Term,TmName)
import CLaSH.Core.Type   (Type)

import CLaSH.Rewrite.Types (DebugLevel)

-- | Global function binders
type BindingMap = HashMap TmName (Type,Term)

data CLaSHOpts = CLaSHOpts { opt_inlineLimit :: Int
                           , opt_specLimit   :: Int
                           , opt_dbgLevel    :: DebugLevel
                           }
