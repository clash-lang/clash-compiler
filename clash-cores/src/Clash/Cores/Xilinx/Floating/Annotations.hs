{-|
Copyright  :  (C) 2021-2022, QBayLogic B.V.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Xilinx.Floating.Annotations
  ( veriBinaryPrim
  , vhdlBinaryPrim
  ) where

import Prelude

import Data.String.Interpolate (__i)
import Language.Haskell.TH.Syntax (Name)

import Clash.Annotations.Primitive (Primitive(..), HDL(..))

-- | The InlinePrimitive annotation for a binary function in VHDL.

-- Note: The BlackBox template includes ~DEVNULL[~LIT[3]] which will ensure the
-- template function (tclTFName argument) gets a fully evaluated Config.
vhdlBinaryPrim
  :: Name
  -> Name
  -> String
  -> Primitive
vhdlBinaryPrim primName tclTFName funcName = InlineYamlPrimitive [VHDL] [__i|
  BlackBox:
    name: #{primName}
    type: |-
      #{primName}
        :: ( KnownDomain dom           --     ARG[0]
           , KnownNat d                --     ARG[1]
           , HasCallStack              --     ARG[2]
           )
        => Config                      --     ARG[3]
        -> Clock dom                   --     ARG[4]
        -> Enable dom                  --     ARG[5]
        -> DSignal dom n Float         -- x , ARG[6]
        -> DSignal dom n Float         -- y , ARG[7]
        -> DSignal dom (n + d) Float
    kind: Declaration
    template: |-
      -- #{funcName} begin
      ~DEVNULL[~LIT[3]]~GENSYM[#{funcName}][0] : block
        COMPONENT ~INCLUDENAME[0]
          PORT (
            aclk : IN STD_LOGIC;
      ~IF~ISACTIVEENABLE[5]~THEN      aclken : IN STD_LOGIC;
      ~ELSE~FI      s_axis_a_tvalid : IN STD_LOGIC;
            s_axis_a_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            s_axis_b_tvalid : IN STD_LOGIC;
            s_axis_b_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
            m_axis_result_tvalid : OUT STD_LOGIC;
            m_axis_result_tdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
          );
        END COMPONENT;
      ~IF~ISACTIVEENABLE[5]~THEN  signal ~GENSYM[clken_std][2]: std_logic;
      begin
        ~SYM[2] <= '1' when (~ARG[5]) else '0';
      ~ELSEbegin
      ~FI  ~GENSYM[#{funcName}][1] : ~INCLUDENAME[0]
          PORT MAP (
            aclk => ~ARG[4],
      ~IF~ISACTIVEENABLE[5]~THEN      aclken => ~SYM[2],
      ~ELSE~FI      s_axis_a_tvalid => '1',
            s_axis_a_tdata => ~ARG[6],
            s_axis_b_tvalid => '1',
            s_axis_b_tdata => ~ARG[7],
            m_axis_result_tvalid => open,
            m_axis_result_tdata => ~RESULT
          );

      end block;
      -- #{funcName} end
    includes:
      - extension: tcl
        name: floating_point
        format: Haskell
        templateFunction: #{tclTFName}
  |]

-- | The InlinePrimitive annotation for a binary function in Verilog.

-- Note: The BlackBox template includes ~DEVNULL[~LIT[3]] which will ensure the
-- template function (tclTFName argument) gets a fully evaluated Config.
veriBinaryPrim
  :: Name
  -> Name
  -> String
  -> Primitive
veriBinaryPrim primName tclTFName funcName =
  InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      type: |-
        #{primName}
          :: ( KnownDomain dom           --     ARG[0]
             , KnownNat d                --     ARG[1]
             , HasCallStack              --     ARG[2]
             )
          => Config                      --     ARG[3]
          -> Clock dom                   --     ARG[4]
          -> Enable dom                  --     ARG[5]
          -> DSignal dom n Float         -- x , ARG[6]
          -> DSignal dom n Float         -- y , ARG[7]
          -> DSignal dom (n + d) Float
      kind: Declaration
      template: |-
        ~DEVNULL[~LIT[3]]~INCLUDENAME[0] ~GENSYM[#{funcName}][0] (
          .aclk(~ARG[4]),
        ~IF~ISACTIVEENABLE[5]~THEN  .aclken(~ARG[5]),
        ~ELSE~FI  .s_axis_a_tvalid(1'b1),
          .s_axis_a_tdata(~ARG[6]),
          .s_axis_b_tvalid(1'b1),
          .s_axis_b_tdata(~ARG[7]),
          .m_axis_result_tvalid(),
          .m_axis_result_tdata(~RESULT)
        );
      includes:
        - extension: tcl
          name: floating_point
          format: Haskell
          templateFunction: #{tclTFName}
    |]
