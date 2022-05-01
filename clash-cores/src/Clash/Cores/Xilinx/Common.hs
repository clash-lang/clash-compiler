{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Xilinx.Common where

import Prelude

import Clash.Netlist.Id (Identifier)
import Data.String (fromString)
import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)
import Data.Text.Prettyprint.Doc.Extra (Doc)

import qualified Clash.Netlist.Id as Id
import qualified Data.Text as Text
import Data.List (intercalate)

type PropName = String
type PropValue = String
type Property = (PropName, PropValue)

data IpConfig = IpConfig
  { name :: String
  , vendor :: String
  , library :: String
  , version :: String
  , moduleName :: Identifier
  , properties :: [Property]
  }

defIpConfig ::
  -- | Name of IP core. For example: \"fifo_generator\".
  String ->
  -- | Version of IP core. For example: \"13.2\".
  String ->
  -- | Name of module the IP core should be generated as. For example: \"dcfifo\". This
  -- name should be unique. See "Clash.Netlist.Id" for more information on how to
  -- generate unique identifiers.
  Identifier ->
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

renderTcl :: IpConfig -> Doc
renderTcl IpConfig{..} =
  fromString $ unindent [i|
    create_ip \\
      -name #{name} \\
      -vendor #{vendor} \\
      -library #{library} \\
      -version #{version} \\
      -module_name {#{moduleNameString}}

    set_property -dict [list \\
#{renderedProperties}
                       ] \\
                      [get_ips {#{moduleNameString}}]

    generate_target {synthesis simulation} [get_ips {#{moduleNameString}}]
  |]

 where
  moduleNameString = Text.unpack (Id.toText moduleName)
  renderedProperties = intercalate "\n" (map prop properties)
  prop (name_, value) = [i|#{indent}CONFIG.#{name_} {#{value}} \\|]
  indent = replicate 25 ' '

toTclBool :: Bool -> String
toTclBool True = "true"
toTclBool False = "false"
