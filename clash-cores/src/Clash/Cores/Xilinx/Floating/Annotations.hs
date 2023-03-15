{-|
Copyright  :  (C) 2021-2023, QBayLogic B.V.,
                  2022     , Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Cores.Xilinx.Floating.Annotations
  ( veriBinaryPrim
  , vhdlBinaryPrim
  , vhdlFromUPrim
  , veriFromUPrim
  , vhdlFromSPrim
  , veriFromSPrim
  , vhdlComparePrim
  , veriComparePrim
  ) where

import Prelude

import Data.List.Infinite (Infinite(..), (...))
import Data.String.Interpolate (__i)
import Language.Haskell.TH.Syntax (Name)

import Clash.Annotations.Primitive (Primitive(..), HDL(..))

import Clash.Cores.Xilinx.Floating.BlackBoxes

-- | The InlinePrimitive annotation for a binary function in VHDL.

-- Note: The BlackBox template includes @~DEVNULL[~LIT[#{config}]]@ which will
-- ensure the template function (tclTFName argument) gets a fully evaluated
-- Config.
vhdlBinaryPrim
  :: Name
  -> Name
  -> String
  -> Primitive
vhdlBinaryPrim primName tclTFName funcName =
  let
    _knownDomain
      :< _knownNat
      :< _hasCallStack
      :< config
      :< clk
      :< en
      :< x
      :< y
      :< _ = ((0 :: Int)...)
    blockSym
      :< instSym
      :< stdEnSym
      :< _ = ((0 :: Int)...)
  in InlineYamlPrimitive [VHDL] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      template: |-
        -- #{funcName} begin
        ~DEVNULL[~LIT[#{config}]]~GENSYM[#{funcName}][#{blockSym}] : block
          COMPONENT ~INCLUDENAME[0]
            PORT (
              aclk : IN STD_LOGIC;
        ~IF~ISACTIVEENABLE[#{en}]~THEN      aclken : IN STD_LOGIC;
        ~ELSE~FI      s_axis_a_tvalid : IN STD_LOGIC;
              s_axis_a_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
              s_axis_b_tvalid : IN STD_LOGIC;
              s_axis_b_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
              m_axis_result_tvalid : OUT STD_LOGIC;
              m_axis_result_tdata : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
            );
          END COMPONENT;
        ~IF~ISACTIVEENABLE[#{en}]~THEN  signal ~GENSYM[clken_std][#{stdEnSym}]: std_logic;
        begin
          ~SYM[#{stdEnSym}] <= '1' when (~ARG[#{en}]) else '0';
        ~ELSEbegin
        ~FI  ~GENSYM[#{funcName}][#{instSym}] : ~INCLUDENAME[0]
            PORT MAP (
              aclk => ~ARG[#{clk}],
        ~IF~ISACTIVEENABLE[#{en}]~THEN      aclken => ~SYM[#{stdEnSym}],
        ~ELSE~FI      s_axis_a_tvalid => '1',
              s_axis_a_tdata => ~ARG[#{x}],
              s_axis_b_tvalid => '1',
              s_axis_b_tdata => ~ARG[#{y}],
              m_axis_result_tvalid => open,
              m_axis_result_tdata => ~RESULT
            );
        end block;
        -- #{funcName} end
      includes:
        - extension: clash.tcl
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
  let
    _knownDomain
      :< _knownNat
      :< _hasCallStack
      :< config
      :< clk
      :< en
      :< x
      :< y
      :< _ = ((0 :: Int)...)
    instSym = 0 :: Int
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      template: |-
        ~DEVNULL[~LIT[#{config}]]~INCLUDENAME[0] ~GENSYM[#{funcName}][#{instSym}] (
          .aclk(~ARG[#{clk}]),
        ~IF~ISACTIVEENABLE[#{en}]~THEN  .aclken(~ARG[#{en}]),
        ~ELSE~FI  .s_axis_a_tvalid(1'b1),
          .s_axis_a_tdata(~ARG[#{x}]),
          .s_axis_b_tvalid(1'b1),
          .s_axis_b_tdata(~ARG[#{y}]),
          .m_axis_result_tvalid(),
          .m_axis_result_tdata(~RESULT)
        );
      includes:
        - extension: clash.tcl
          name: floating_point
          format: Haskell
          templateFunction: #{tclTFName}
    |]

vhdlFromUPrim
  :: Name
  -> String
  -> Primitive
vhdlFromUPrim primName funcName =
  let tfName = 'fromUTclTF
      _knownDomain
        :< _knownNat
        :< _hasCallStack
        :< clk
        :< en
        :< input
        :< _ = ((0 :: Int)...)
      blockSym
        :< inpSlvSym
        :< compSym
        :< clkEnStdSym
        :< _ = ((0 :: Int)...)
  in InlineYamlPrimitive [VHDL] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      template: |-
        -- #{funcName} begin
        ~GENSYM[#{funcName}][#{blockSym}] : block
          component ~INCLUDENAME[0]
            port (
              aclk : in std_logic;
        ~IF~ISACTIVEENABLE[#{en}]~THEN      aclken : in std_logic;
        ~ELSE~FI      s_axis_a_tvalid : in std_logic;
              s_axis_a_tdata : in std_logic_vector(~SIZE[~TYP[#{input}]]-1 downto 0);
              m_axis_result_tvalid : out std_logic;
              m_axis_result_tdata : out std_logic_vector(31 downto 0)
            );
          end component;
          signal ~GENSYM[inp_slv][#{inpSlvSym}]: std_logic_vector(~SIZE[~TYP[#{input}]]-1 downto 0);
        ~IF~ISACTIVEENABLE[#{en}]~THEN  signal ~GENSYM[clken_std][#{clkEnStdSym}]: std_logic;
        begin
          ~SYM[#{clkEnStdSym}] <= '1' when (~ARG[#{en}]) else '0';
        ~ELSEbegin
        ~FI  ~SYM[#{inpSlvSym}] <= ~TOBV[~ARG[#{input}]][~TYP[#{input}]];
          ~GENSYM[#{funcName}][#{compSym}] : ~INCLUDENAME[0]
            port map (
              aclk => ~ARG[#{clk}],
        ~IF~ISACTIVEENABLE[#{en}]~THEN      aclken => ~SYM[#{clk}],
        ~ELSE~FI      s_axis_a_tvalid => '1',
              s_axis_a_tdata => ~SYM[#{inpSlvSym}],
              m_axis_result_tvalid => open,
              m_axis_result_tdata => ~RESULT
            );
        end block;
        -- #{funcName} end
      includes:
        - extension: clash.tcl
          name: floating_point
          format: Haskell
          templateFunction: #{tfName}
    |]

veriFromUPrim
  :: Name
  -> String
  -> Primitive
veriFromUPrim primName funcName =
  let
    tfName = 'fromUTclTF
    _knownDomain
      :< _knownNat
      :< _hasCallStack
      :< clk
      :< en
      :< input
      :< _ = ((0 :: Int)...)
    instSym = 0 :: Int
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      template: |-
        // #{funcName} begin
        ~INCLUDENAME[0] ~GENSYM[#{funcName}][#{instSym}] (
          .aclk(~ARG[#{clk}]),
        ~IF~ISACTIVEENABLE[#{en}]~THEN  .aclken(~ARG[#{en}]),
        ~ELSE~FI  .s_axis_a_tvalid(1'b1),
          .s_axis_a_tdata(~ARG[#{input}]),
          .m_axis_result_tvalid(),
          .m_axis_result_tdata(~RESULT)
        );
        // #{funcName} end
      includes:
        - extension: clash.tcl
          name: floating_point
          format: Haskell
          templateFunction: #{tfName}
    |]

vhdlFromSPrim
  :: Name
  -> String
  -> Primitive
vhdlFromSPrim primName funcName =
  let
    tfName = 'fromSTclTF
    _knownDomain
      :< _knownNat
      :< _hasCallStack
      :< clk
      :< en
      :< input
      :< _ = ((0 :: Int)...)
    blockSym
      :< inpSlvSym
      :< compSym
      :< clkEnStdSym
      :< _ = ((0 :: Int)...)
  in InlineYamlPrimitive [VHDL] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      template: |-
        -- #{funcName} begin
        ~GENSYM[#{funcName}][#{blockSym}] : block
          component ~INCLUDENAME[0]
            port (
              aclk : in std_logic;
        ~IF~ISACTIVEENABLE[#{en}]~THEN      aclken : in std_logic;
        ~ELSE~FI      s_axis_a_tvalid : in std_logic;
              s_axis_a_tdata : in std_logic_vector(~SIZE[~TYP[#{input}]]-1 downto 0);
              m_axis_result_tvalid : out std_logic;
              m_axis_result_tdata : out std_logic_vector(31 downto 0)
            );
          end component;
          signal ~GENSYM[inp_slv][#{inpSlvSym}]: std_logic_vector(~SIZE[~TYP[#{input}]]-1 downto 0);
        ~IF~ISACTIVEENABLE[#{en}]~THEN  signal ~GENSYM[clken_std][#{clkEnStdSym}]: std_logic;
        begin
          ~SYM[#{clkEnStdSym}] <= '1' when (~ARG[#{en}]) else '0';
        ~ELSEbegin
        ~FI  ~SYM[#{inpSlvSym}] <= ~TOBV[~ARG[#{input}]][~TYP[#{input}]];
          ~GENSYM[#{funcName}][#{compSym}] : ~INCLUDENAME[0]
            port map (
              aclk => ~ARG[#{clk}],
        ~IF~ISACTIVEENABLE[#{en}]~THEN      aclken => ~SYM[#{clk}],
        ~ELSE~FI      s_axis_a_tvalid => '1',
              s_axis_a_tdata => ~SYM[#{inpSlvSym}],
              m_axis_result_tvalid => open,
              m_axis_result_tdata => ~RESULT
            );
        end block;
        -- #{funcName} end
      includes:
        - extension: clash.tcl
          name: floating_point
          format: Haskell
          templateFunction: #{tfName}
    |]

veriFromSPrim
  :: Name
  -> String
  -> Primitive
veriFromSPrim primName funcName =
  let
    tfName = 'fromSTclTF
    _knownDomain
      :< _knownNat
      :< _hasCallStack
      :< clk
      :< en
      :< input
      :< _ = ((0 :: Int)...)
    instSym = 0 :: Int
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      template: |-
        // #{funcName} begin
        ~INCLUDENAME[0] ~GENSYM[#{funcName}][#{instSym}] (
          .aclk(~ARG[#{clk}]),
        ~IF~ISACTIVEENABLE[#{en}]~THEN  .aclken(~ARG[#{en}]),
        ~ELSE~FI  .s_axis_a_tvalid(1'b1),
          .s_axis_a_tdata(~ARG[#{input}]),
          .m_axis_result_tvalid(),
          .m_axis_result_tdata(~RESULT)
        );
        // #{funcName} end
      includes:
        - extension: clash.tcl
          name: floating_point
          format: Haskell
          templateFunction: #{tfName}
    |]

-- | The InlinePrimitive annotation for Xilinx's compare floating point
-- primitive, in VHDL.
vhdlComparePrim
  :: Name
  -> Name
  -> String
  -> Primitive
vhdlComparePrim primName tclTFName funcName =
  let
    _knownDomain
      :< _knownNat
      :< _hasCallStack
      :< clock
      :< enable
      :< x
      :< y
      :< _ = ((0 :: Int)...)
    blockSym
      :< compSym
      :< clkEnStdSym
      :< ipResultSym
      :< _ = ((0 :: Int)...)
  in InlineYamlPrimitive [VHDL] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      template: |-
        -- #{funcName} begin
        ~GENSYM[#{funcName}][#{blockSym}] : block
          COMPONENT ~INCLUDENAME[0]
            PORT (
              aclk : IN STD_LOGIC;
        ~IF~ISACTIVEENABLE[#{enable}]~THEN      aclken : IN STD_LOGIC;
        ~ELSE~FI      s_axis_a_tvalid : IN STD_LOGIC;
              s_axis_a_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
              s_axis_b_tvalid : IN STD_LOGIC;
              s_axis_b_tdata : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
              m_axis_result_tvalid : OUT STD_LOGIC;
              m_axis_result_tdata : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
            );
          END COMPONENT;
        ~IF~ISACTIVEENABLE[#{enable}]~THEN  signal ~GENSYM[clken_std][#{clkEnStdSym}]: std_logic;~ELSE~FI
          signal ~GENSYM[ip_result][#{ipResultSym}] : std_logic_vector(7 downto 0);
        begin
          ~IF~ISACTIVEENABLE[#{enable}]~THEN~SYM[#{clkEnStdSym}] <= '1' when (~ARG[#{enable}]) else '0';~ELSE~FI
          ~RESULT <= ~SYM[#{ipResultSym}](3 downto 0);
          ~GENSYM[#{funcName}][#{compSym}] : ~INCLUDENAME[0]
            PORT MAP (
              aclk => ~ARG[#{clock}],
        ~IF~ISACTIVEENABLE[#{enable}]~THEN      aclken => ~SYM[#{clkEnStdSym}],
        ~ELSE~FI      s_axis_a_tvalid => '1',
              s_axis_a_tdata => ~ARG[#{x}],
              s_axis_b_tvalid => '1',
              s_axis_b_tdata => ~ARG[#{y}],
              m_axis_result_tvalid => open,
              m_axis_result_tdata => ~SYM[#{ipResultSym}]
            );
        end block;
        -- #{funcName} end
      includes:
        - extension: clash.tcl
          name: floating_point
          format: Haskell
          templateFunction: #{tclTFName}
    |]

-- | The InlinePrimitive annotation for Xilinx's compare floating point
-- primitive, in Verilog.
veriComparePrim
  :: Name
  -> Name
  -> String
  -> Primitive
veriComparePrim primName tclTFName funcName =
  let
    _knownDomain
      :< _knownNat
      :< _hasCallStack
      :< clock
      :< enable
      :< x
      :< y
      :< _ = ((0 :: Int)...)
    compSym = 0 :: Int
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{primName}
      kind: Declaration
      template: |-
        // #{funcName} begin
        ~INCLUDENAME[0] ~GENSYM[#{funcName}][#{compSym}] (
          .aclk(~ARG[#{clock}]),
        ~IF~ISACTIVEENABLE[#{enable}]~THEN  .aclken(~ARG[#{enable}]),
        ~ELSE~FI  .s_axis_a_tvalid(1'b1),
          .s_axis_a_tdata(~ARG[#{x}]),
          .s_axis_b_tvalid(1'b1),
          .s_axis_b_tdata(~ARG[#{y}]),
          .m_axis_result_tvalid(),
          .m_axis_result_tdata(~RESULT)
        );
        // #{funcName} end
      includes:
        - extension: clash.tcl
          name: floating_point
          format: Haskell
          templateFunction: #{tclTFName}
    |]
