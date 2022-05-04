{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
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
  ) where

import Prelude

import Control.Monad.State (State)
import Data.Maybe (isJust, fromJust)
import Data.String (fromString, IsString)
import Data.String.Interpolate (i)

import Clash.Backend (Backend)
import Clash.Netlist.Ast.Type (HWType(..))
import Clash.Netlist.Types
  (BlackBoxContext(..), Expr(..), Literal(..), Modifier(..),
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
    replicate 25 ' ' ++ name ++ ' ': value ++ " \\\n" ++ s

  bbText =
    fromString [i|create_ip -name floating_point -vendor xilinx.com -library ip \\
          -version 7.1 -module_name {#{compName}}
set_property -dict [list \\
#{props}                   ] \\
                   [get_ips {#{compName}}]
generate_target {synthesis simulation} [get_ips {#{compName}}]|]

show0 :: (Show a, IsString s) => a -> s
show0 = fromString . show
