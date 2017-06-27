{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                         2017, QBayLogic, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type definitions used by the Driver module
-}

module CLaSH.Driver.Types
  (module CLaSH.Driver.Types
  ,SrcSpan, noSrcSpan
  )
where

import Control.Exception (Exception)
import Data.HashMap.Lazy (HashMap)
import Data.Text.Lazy    (Text)

import SrcLoc            (SrcSpan, noSrcSpan)

import CLaSH.Core.Term   (Term,TmName,TmOccName)
import CLaSH.Core.Type   (Type)

import CLaSH.Rewrite.Types (DebugLevel)
import CLaSH.Netlist.BlackBox.Types (HdlSyn)

-- | Global function binders
type BindingMap = HashMap TmOccName (TmName,Type,SrcSpan,Term)

data CLaSHOpts = CLaSHOpts { opt_inlineLimit :: Int
                           , opt_specLimit   :: Int
                           , opt_inlineBelow :: Int
                           , opt_dbgLevel    :: DebugLevel
                           , opt_cleanhdl    :: Bool
                           , opt_intWidth    :: Int
                           , opt_hdlDir      :: Maybe String
                           , opt_hdlSyn      :: HdlSyn
                           , opt_errorExtra  :: Bool
                           , opt_floatSupport :: Bool
                           , opt_allowZero   :: Bool
                           , opt_importPaths :: [FilePath]
                           , opt_errorInvalidCoercions :: Bool
                           }

data CLaSHException = CLaSHException SrcSpan String (Maybe String)

instance Show CLaSHException where
  show (CLaSHException _ s eM) = s ++ "\n" ++ maybe "" id eM

instance Exception CLaSHException

-- | Information about the generated HDL between (sub)runs of the compiler
data Manifest
  = Manifest
  { manifestHash :: (Int,Maybe Int)
    -- ^ Hash of the TopEntity and all its dependencies
    --   + (maybe) Hash of the TestBench and all its dependencies
  , portInTypes  :: [Text]
    -- ^ The rendered versions of the types of the input ports of the TopEntity
    --
    -- Used when dealing with multiple @TopEntity@s who have different names
    -- for types which are structurally equal
  , portOutTypes :: [Text]
    -- ^ The rendered versions of the types of the output ports of the TopEntity
    --
    -- Used when dealing with multiple @TopEntity@s who have different names
    -- for types which are structurally equal
  }
  deriving (Show,Read)
