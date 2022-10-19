{-|
Copyright  :  (C) 2021-2022, QBayLogic B.V.,
                  2022     , Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Floating.BlackBoxes
  ( addTclTF
  , subTclTF
  , mulTclTF
  , divTclTF
  , fromUTclTF
  ) where

import Prelude

import Control.Monad.State (State)
import Data.Maybe (isJust, fromJust)
import Data.String (fromString, IsString)
import Data.String.Interpolate (i, __i)

import Clash.Backend (Backend)
import Clash.Netlist.Types
  (BlackBoxContext(..), Expr(..), HWType(..), Literal(..), Modifier(..),
   TemplateFunction(..))
import Data.Text.Prettyprint.Doc.Extra (Doc)

import Clash.Cores.Xilinx.Floating.Internal

data HasCustom = HasCustom
  { addSubVal ::  !(Maybe String)
  , hasArchOpt :: !Bool
  , hasDspUsage :: !Bool
  , hasBMemUsage :: !Bool
  }

defHasCustom :: HasCustom
defHasCustom = HasCustom
  { addSubVal = Nothing
  , hasArchOpt = True
  , hasDspUsage = True
  , hasBMemUsage = False
  }

hasNoCustom :: HasCustom
hasNoCustom = HasCustom
  { addSubVal = Nothing
  , hasArchOpt = False
  , hasDspUsage = False
  , hasBMemUsage = False
  }

addTclTF :: TemplateFunction
addTclTF =
  binaryTclTF (defHasCustom { addSubVal = Just "Add" }) "Add_Subtract"

subTclTF :: TemplateFunction
subTclTF =
  binaryTclTF (defHasCustom { addSubVal = Just "Subtract" }) "Add_Subtract"

mulTclTF :: TemplateFunction
mulTclTF = binaryTclTF hasCustom "Multiply"
 where
  hasCustom = HasCustom { addSubVal = Nothing
                        , hasArchOpt = False
                        , hasDspUsage = True
                        , hasBMemUsage = False
                        }

divTclTF :: TemplateFunction
divTclTF = binaryTclTF hasNoCustom "Divide"

binaryTclTF
  :: HasCustom
  -> String
  -> TemplateFunction
binaryTclTF hasCustom operType =
  TemplateFunction used valid (tclTemplate hasCustom operType)
 where
  used = [0..4]
  valid = const True

tclTemplate
  :: Backend s
  => HasCustom
  -> String
  -> BlackBoxContext
  -> State s Doc
tclTemplate (HasCustom {..}) operType bbCtx = pure bbText
 where
  compName = bbQsysIncName bbCtx !! 0

  (Literal _ (NumLit latency), _, _) = bbInputs bbCtx !! 1
  (DataCon _ _ cfgExprs, _, _) = bbInputs bbCtx !! 3
  cfgArchOptExpr = cfgExprs !! 0
  cfgDspUsageExpr = cfgExprs !! 1
  cfgBMemUsageExpr = cfgExprs !! 2

  DataCon _ (DC (Sum _ cfgArchOptConstrs, cfgArchOptTag)) _ = cfgArchOptExpr
  cfgArchOpt = cfgArchOptConstrs !! cfgArchOptTag
  tclArchOpt :: String
  tclArchOpt
    | cfgArchOpt == show0 'SpeedArch = "Speed_Optimized"
    | cfgArchOpt == show0 'LatencyArch = "Low_Latency"
    | otherwise = error "Unknown ArchOpt constructor"

  DataCon _ (DC (Sum _ cfgDspUsageConstrs, cfgDspUsageTag)) _ = cfgDspUsageExpr
  cfgDspUsage = cfgDspUsageConstrs !! cfgDspUsageTag
  tclDspUsage :: String
  tclDspUsage
    | cfgDspUsage == show0 'NoDspUsage =  "No_Usage"
    | cfgDspUsage == show0 'MediumDspUsage = "Medium_Usage"
    | cfgDspUsage == show0 'FullDspUsage = "Full_Usage"
    | cfgDspUsage == show0 'MaxDspUsage = "Max_Usage"
    | otherwise = error "Unknown FloatingDspUsage constructor"

  DataCon _ (DC (Sum _ cfgBMemUsageConstrs, cfgBMemUsageTag)) _ = cfgBMemUsageExpr
  cfgBMemUsage = cfgBMemUsageConstrs !! cfgBMemUsageTag
  tclBMemUsage :: String
  tclBMemUsage
    | cfgBMemUsage == show0 'NoBMemUsage = "No_Usage"
    | cfgBMemUsage == show0 'FullBMemUsage = "Full_Usage"
    | otherwise = error "Unknown BMemUsage constructor"

  tclClkEn :: String
  tclClkEn =
    case bbInputs bbCtx !! 5 of
      (DataCon _ _ [Literal Nothing (BoolLit True)], _, _) -> "false"
      _                                                    -> "true"

  props =
    foldr prop ""
      [ (True, "CONFIG.Operation_Type", operType)
      , (isJust addSubVal, "CONFIG.Add_Sub_Value", fromJust addSubVal)
      , (hasArchOpt, "CONFIG.C_Optimization", tclArchOpt)
      , (hasDspUsage, "CONFIG.C_Mult_Usage", tclDspUsage)
      , (hasBMemUsage, "CONFIG.C_BRAM_Usage", tclBMemUsage)
      , (True, "CONFIG.Flow_Control", "NonBlocking")
      , (True, "CONFIG.Has_ACLKEN", tclClkEn)
      , (True, "CONFIG.Has_RESULT_TREADY", "false")
      , (True, "CONFIG.Maximum_Latency", "false")
      , (True, "CONFIG.C_Latency", show latency)
      ]
  prop (False, _, _) s = s
  prop (True, name, value) s =
    replicate 29 ' ' ++ name ++ ' ': value ++ " \\\n" ++ s

  bbText = [i|namespace eval $tclIface {
  variable api 1
  variable scriptPurpose createIp
  variable ipName {#{compName}}

  proc createIp {ipName0 args} {
    create_ip -name floating_point -vendor xilinx.com -library ip \\
        -version 7.1 -module_name $ipName0 {*}$args

    set_property -dict [list \\
#{props}                       ] [get_ips $ipName0]
    return
  }
}|]

fromUTclTF :: TemplateFunction
fromUTclTF = TemplateFunction used valid fromUTclTemplate
 where
  used = [1,4,5]
  valid = const True

fromUTclTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
fromUTclTemplate bbCtx = pure bbText
 where
  [compName] = bbQsysIncName bbCtx
  (Literal _ (NumLit latency), _, _) = bbInputs bbCtx !! 1
  tclClkEn :: String
  tclClkEn =
    case bbInputs bbCtx !! 4 of
      (DataCon _ _ [Literal Nothing (BoolLit True)], _, _) -> "false"
      _                                                    -> "true"
  (_, Unsigned inpLen, _) = bbInputs bbCtx !! 5

  bbText = [__i|
    namespace eval $tclIface {
      variable api 1
      variable scriptPurpose createIp
      variable ipName {#{compName}}

      proc createIp {ipName0 args} {
        create_ip -name floating_point -vendor xilinx.com -library ip \\
            -version 7.1 -module_name $ipName0 {*}$args

        set_property -dict [list \\
                                 CONFIG.Operation_Type Fixed_to_float \\
                                 CONFIG.A_Precision_Type Uint#{inpLen} \\
                                 CONFIG.Flow_Control NonBlocking \\
                                 CONFIG.Has_ACLKEN #{tclClkEn} \\
                                 CONFIG.C_A_Exponent_Width #{inpLen} \\
                                 CONFIG.C_A_Fraction_Width 0 \\
                                 CONFIG.Has_RESULT_TREADY false \\
                                 CONFIG.C_Latency #{latency} \\
                                 CONFIG.C_Rate 1 \\
                                 CONFIG.Maximum_Latency false \\
                           ] [get_ips $ipName0]
        return
      }
    }|]

show0 :: (Show a, IsString s) => a -> s
show0 = fromString . show
