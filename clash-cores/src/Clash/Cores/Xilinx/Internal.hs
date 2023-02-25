{-|
  Copyright   :  (C) 2022 Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Common utilities for defining Xilinx IP primitives.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Internal where

import Prelude
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
  ((<+>), align, backslash, indent, line, pretty, rbrace, vsep)
#else
import Data.Text.Prettyprint.Doc
  ((<+>), align, backslash, indent, line, pretty, rbrace, vsep)
#endif
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import Prettyprinter.Interpolate (di, __di)

import Clash.Netlist.Types (IdentifierText)

type PropName = Text
type PropValue = Text
data Property = Property !PropName !PropValue
  deriving Show

-- | String representation of values in Tcl
--
-- __WARNING__: This does not (yet?) do any escaping of special characters!
-- Generating a proper Tcl word where needed is left to the implementation.
class TclShow a where
  -- | Represent the value as a Tcl string
  --
  -- Default: 'show'
  tclShow :: a -> Text
  default tclShow :: Show a => a -> Text
  tclShow = Text.pack . show
  -- | The method @tclShowList@ is provided to allow the programmer to give a
  -- specialised way of showing lists of values. For example, this is used by
  -- the @TclShow@ instance of the @Char@ type, where values of type @String@
  -- should be a simple string rather than a list of @Char@ values.
  --
  -- The default implementation renders into a Tcl list of the shape
  -- @[list X Y Z]@.
  tclShowList :: [a] -> Text
  tclShowList [] = "{}"
  tclShowList [e] = tclShow e
  tclShowList es = ("[list" <>) . shows0 es $ "]"
   where
    shows0 [] s = s
    shows0 (e:es0) s = Text.cons ' ' . (tclShow e <>) . shows0 es0 $ s

showLower :: Show a => a -> Text
showLower = Text.toLower . Text.pack . show

instance TclShow a => TclShow [a] where
  tclShow = tclShowList

instance TclShow Bool where
  tclShow = showLower

instance TclShow Char where
  tclShow = Text.singleton
  tclShowList = Text.pack

instance TclShow Text where
  tclShow = id

instance TclShow Int
instance TclShow Integer
instance TclShow Natural

newtype BraceTcl a = BraceTcl a
  deriving (Show, Eq)

instance TclShow a => TclShow (BraceTcl a) where
  tclShow (BraceTcl x) = '{' `Text.cons` tclShow x `Text.snoc` '}'

property :: TclShow a => Text -> a -> Property
property name_ value = Property name_ (tclShow value)

data TclPurpose = IpConfigPurpose !IpConfig | ReadXdcPurpose !ReadXdc

data IpConfig = IpConfig
  { name :: !Text
  , vendor :: !Text
  , library :: !Text
  , version :: !Text
  , moduleName :: !IdentifierText
  , properties :: ![Property]
  }

data ReadXdc = ReadXdc
  { xdcFile :: !Text
  , order :: !Order
  , usedIn :: !UsedIn
  , scopeRef :: !(Maybe Text)
  }

data Order = Early | Normal | Late
  deriving (Eq, Show)

instance TclShow Order where
  tclShow = showLower

data UsedIn = Synthesis | Implementation | Both
  deriving (Eq, Show)

instance TclShow UsedIn where
  tclShow Both = "{synthesis implementation}"
  tclShow u = showLower u

defIpConfig ::
  -- | Name of IP core. For example: \"fifo_generator\".
  Text ->
  -- | Version of IP core. For example: \"13.2\".
  Text ->
  -- | Name of module the IP core should be generated as. For example: \"dcfifo\". This
  -- name should be unique. See "Clash.Netlist.Id" for more information on how to
  -- generate unique identifiers.
  IdentifierText ->
  -- | Configuration with sensible defaults.
  IpConfig
defIpConfig name_ version_ moduleName_ = IpConfig
  { name = name_
  , version = version_
  , moduleName = moduleName_
  , vendor = "xilinx.com"
  , library = "ip"
  , properties = []
  }

defReadXdc :: Text -> ReadXdc
defReadXdc file = ReadXdc
  { xdcFile = file
  , order = Early
  , usedIn = Both
  , scopeRef = Nothing
  }

-- We currently have no users of the "multipleScripts" and the ReadXdc
-- mechanisms, as it turned out that DcFifo bundled the correct constraints
-- after all (through the 'xpm_cdc_gray' module it uses). But the Tcl code has
-- been verified and will be useful later.
renderTcl :: HasCallStack => [TclPurpose] -> Doc
renderTcl = \case
  [] ->
    error "Tcl script needs at least one purpose"
  [p] ->
    rootSnippet <> line <> indent 2 (renderOne p) <> line <> rbrace
  ps ->
    let ms = zipWith renderMulti [1 :: Int ..] ps
    in rootSnippet <> line <>
       indent 2 "variable scriptPurpose multipleScripts" <> line <> rbrace <>
       line <> vsep ms
 where
  rootSnippet = [__di|
    namespace eval $tclIface {
      variable api 1
    |]

  renderOne (IpConfigPurpose IpConfig{..}) =
    ipSnippet <> line <> indent 4 renderedProperties <> line <>
    indent 2 "return" <> line <> rbrace
   where
    -- \& is needed to prevent CPP from joining lines :-(
    ipSnippet = [__di|
      variable scriptPurpose createIp
      variable ipName {#{moduleName}}
      proc createIp {ipName0 args} {
        create_ip \\\&
          -name #{name} \\\&
          -vendor #{vendor} \\\&
          -library #{library} \\\&
          -version #{version} \\\&
          -module_name $ipName0 \\\&
          {*}$args

        set_property \\\&
      |]
    renderedProperties =
      "-dict [list " <> align (vsep ("\\" : map prop properties)) <>
      line <> "      ] [get_ips $ipName0]"
    prop (Property name_ value) =
      "CONFIG." <> pretty name_ <+> pretty value <+> backslash
  renderOne (ReadXdcPurpose ReadXdc{..}) = vsep $ catMaybes vars
   where
    var :: TclShow a => Doc -> a -> Doc
    var name_ value = "variable" <+> name_ <+> pretty (tclShow value)
    vars :: [Maybe Doc]
    vars = [ Just $ var @Text "scriptPurpose" "readXdc"
           , Just $ var       "order"         order
           , fmap ( var       "scopeRef"      . BraceTcl) scopeRef
           , Just $ var       "usedIn"        usedIn
           , Just $ var       "xdcFile"       $ BraceTcl xdcFile
           ]

  renderMulti n p =
    line <> [di|namespace eval ${tclIface}::multipleScripts::script#{n} {|] <>
    line <> indent 2 (renderOne p) <> line <> rbrace
