
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Clash.Signal.Trace.VCD where

import           Data.Int              (Int64)
import           GHC.Natural           (Natural)
import qualified Data.Text             as Text
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Format      (formatTime, defaultTimeLocale)
import           Data.Bifunctor        (first,second)
import           Data.Bits             (testBit)
import           GHC.Exts              (sortWith)

type Value    = (Natural, Natural) -- (Mask, Value)
type Width    = Int

data VCDFile = VCDFile [DeclarationCommand] [SimulationCommand]
  deriving (Show)


data DeclarationCommand
  = Version String
  | Date UTCTime
  | TimeScale Int TimeUnit
  | VarDec Var
  | Scope Scope
  | UpScope
  deriving (Show)

data TimeUnit = TimeS | TimeMS | TimeUS | TimeNS | TimePS | TimeFS
instance Show TimeUnit where
  showsPrec _ TimeS = ('s' :)
  showsPrec _ TimeMS = showString "ms"
  showsPrec _ TimeUS = showString "us"
  showsPrec _ TimeNS = showString "ns"
  showsPrec _ TimePS = showString "ps"
  showsPrec _ TimeFS = showString "fs"

data Var
  = Var
  { varSize   :: Width
  , varIDCode :: IDCode
  , varReference    :: String
  }
  deriving (Show)

type IDCode = String

type Scope = String
type VCDTime = Int64

data SimulationCommand
  = SimulationTime VCDTime
  | DumpVars [ValueChange]
  | ValueChanges [ValueChange]
  deriving (Show)

data ValueChange
  = ValueChange
  { changeSize :: Width
  , changeIDCode :: IDCode
  , changeValue :: Value
  }
  deriving (Show)

iso8601Format :: UTCTime -> String
iso8601Format = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

class RenderVCD a where
  renderVCD :: a -> Text.Text

instance RenderVCD a => RenderVCD [a] where
  renderVCD = Text.unlines . map renderVCD

instance RenderVCD VCDFile where
  renderVCD (VCDFile dec sim) = Text.unlines
    [ renderVCD dec
    , "$enddefinitions $end"
    , renderVCD sim ]

instance RenderVCD DeclarationCommand where
  renderVCD (Version version) = Text.unwords
    [ "$version"
    , Text.pack version
    , "$end"]
  renderVCD (Date t) = Text.unwords
    [ "$date"
    , Text.pack $ iso8601Format t
    , "$end" ]
  renderVCD (TimeScale amount unit) = Text.unwords
    [ "$timescale"
    , Text.pack $ shows amount $ show unit
    , "$end" ]
  renderVCD (VarDec var) = renderVCD var
  renderVCD (Scope scope) = Text.unwords
    [ "$scope module"
    , Text.pack scope
    , "$end" ]
  renderVCD UpScope = Text.pack "$upscope $end"

instance RenderVCD Var where
  renderVCD Var{..} = Text.unwords $ map Text.pack
    [ "$var wire"
    , show varSize
    , varIDCode
    , varReference
    , "$end" ]

instance RenderVCD SimulationCommand where
  renderVCD (SimulationTime t) = Text.pack ('#':show t)
  renderVCD (DumpVars changes) = Text.unlines
    [ "$dumpvars"
    , renderVCD changes
    , "$end" ]
  renderVCD (ValueChanges changes) = renderVCD changes

instance RenderVCD ValueChange where
  renderVCD (ValueChange 1 idCode (0, 0)) =
    Text.pack ('0':idCode)
  renderVCD (ValueChange 1 idCode (0, 1)) =
    Text.pack ('1':idCode)
  renderVCD (ValueChange 1 idCode (1, _)) =
    Text.pack ('x':idCode)
  renderVCD (ValueChange 1 idCode (mask, val)) =
    error $
      "Can't format 1 bit wide value for "
        ++ show idCode
        ++ ": value "
        ++ show val
        ++ " and mask "
        ++ show mask
  renderVCD ValueChange{..} =
    Text.pack $
         'b'
      :  map digit (reverse [0 .. changeSize - 1])
      ++ ' ':changeIDCode
   where
    (mask, val) = changeValue
    digit d = case (testBit mask d, testBit val d) of
      (False,False) -> '0'
      (False,True)  -> '1'
      (True,_)      -> 'x'


------------------------------

-- Some functions for dealing with scope splitting of trace names
-- These might be more suitable for Signal.Trace
-- Use as:
-- mkScopes $ zipWith3 mkScopedVar traceNames widths labels


mkScopedVar :: String -> Width -> IDCode -> ([Scope],Var)
mkScopedVar name width label = (scopes,Var width label name')
  where
    parts = words $ map \case{'.' -> ' ';c->c} name
    scopes = init parts
    name' = last parts

mkScopes :: [([Scope],Var)] -> [DeclarationCommand]
mkScopes traces = mkScope ("logic",traces')
  where
    traces' = sortWith fst $ traces

    mkScope :: (Scope,[([Scope],Var)]) -> [DeclarationCommand]
    mkScope (scopeName,traces'') =
         Scope scopeName
      :  map VarDec signals
      ++ concatMap mkScope scopes
      ++ [ UpScope ]
      where
        (signals,scopes) = groupSignals traces''

    groupSignals :: [([Scope],Var)] -> ([Var],[(Scope,[([Scope],Var)])])
    groupSignals [] = ([],[])
    groupSignals (([],v):rest) = first (v:) $ groupSignals rest
    groupSignals traces''@((s:_,_):_) = second ((s,scope):) $ groupSignals rest
      where (scope,rest) = splitBy s traces''

    splitBy :: String -> [([Scope],Var)] -> ([([Scope],Var)],[([Scope],Var)])
    splitBy s ((s':ss,v):rest) | s==s' = first ((ss,v):) $ splitBy s rest
    splitBy _ rest = ([],rest)
