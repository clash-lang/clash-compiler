{-|
  Copyright  :  (C) 2013-2016, University of Twente, 2017, QBayLogic
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

import SrcLoc            (SrcSpan, noSrcSpan)

import CLaSH.Core.Term   (Term,TmName)
import CLaSH.Core.Type   (Type)

import CLaSH.Rewrite.Types (DebugLevel)
import CLaSH.Netlist.BlackBox.Types (HdlSyn)

-- | Global function binders
type BindingMap = HashMap TmName (Type,SrcSpan,Term)

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
                           }

data CLaSHException = CLaSHException SrcSpan String (Maybe String)

instance Show CLaSHException where
  show (CLaSHException _ s eM) = s ++ "\n" ++ maybe "" id eM

instance Exception CLaSHException
