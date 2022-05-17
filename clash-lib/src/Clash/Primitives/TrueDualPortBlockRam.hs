{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Clash.Primitives.TrueDualPortBlockRam where

import           Data.Text.Lazy                     (pack)
import           Text.Trifecta.Result               (Result(Success))
import qualified Data.String.Interpolate            as I
import qualified Data.String.Interpolate.Util       as I

import           Clash.Netlist.BlackBox.Parser      (runParse)
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(TDecl), emptyBlackBoxMeta)
import           Clash.Netlist.Types(BlackBox(BBTemplate))

import           Clash.Core.TermLiteral
import           Clash.Core.Pretty
import Clash.Explicit.BlockRam(WriteMode(..))

trueDualPortBlockRamVerilog :: BlackBoxFunction
trueDualPortBlockRamVerilog _isD _primName args _ty = return bb
  where
    meta bbKi = emptyBlackBoxMeta{bbKind=bbKi}
    [ _nAddr
      , _domA
      , _domB
      , _a
      , _hasCallStack
      , _KnownNatnAddrs
      , _KnownDomaindomA
      , _KnownDomaindomB
      , _NFDataX
      , Left wmA
      , Left wmB
      , _clkA
      , _enA
      , _wEnA
      , _nAddrA
      , _datA
      , _clkB
      ,  _enB
      , _wEnB
      , _nAddrB
      , _datB] = case args of
        [ _nAddr             -- tyVar
          , _dom             -- tyVar
          , _domB            -- tyVar
          , _a               -- tyVar
          , _hasCallStacc    -- 0
          , _KnownNatnAddrs  -- 1
          , _KnownDomaindomA -- 2
          , _KnownDomaindomB -- 3
          , _NFDataX         -- 4
          , Left wmA         -- 5
          , Left wmB         -- 6
          , _clkA            -- 7
          , _enA             -- 8
          , _wEnA            -- 9
          , _nAddrA          -- 10
          , _datA            -- 11
          , _clkB            -- 12
          , _enB             -- 13
          , _wEnB            -- 14
          , _nAddrB          -- 15
          , _datB]           -- 16
          -> args
        _ -> error $ unlines $ map show args

    bb = case runParse (pack $ I.unindent $ pre ++ portA ++ portB ++ post) of
       Success t    -> Right (meta TDecl, BBTemplate t)
       _            -> Left "internal error: parse fail"
    pre = [I.i|
        // trueDualPortBlockRam begin
        // Shared memory
        reg [~SIZE[~TYP[11]]-1:0] ~GENSYM[mem][0] [~LIT[1]-1:0];

        reg ~SIGD[~GENSYM[data_slow][1]][11];
        reg ~SIGD[~GENSYM[data_fast][2]][16];|]

    portA = case termToData wmA of
      Right WriteFirst -> [I.i|
        // Port A WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[7]) begin
            if(~ARG[8]) begin
                ~SYM[1] <= ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI];
                if(~ARG[9]) begin
                    ~SYM[1] <= ~ARG[11];
                    ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI] <= ~ARG[11];
                end
            end
        end|]
      Right ReadFirst -> [I.i|
        // Port A ReadFirst
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[7]) begin
                if(~ARG[8]) begin
                    ~SYM[1] <= ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI];
                    if(~ARG[9]) begin
                        ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI] <= ~ARG[11];
                    end
                end
            end|]
      Right NoChange -> [I.i|
        // Port A NoChange
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[7]) begin
                if(~ARG[8]) begin
                    if(~ARG[9]) begin
                        ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI] <= ~ARG[11];
                    end else begin
                        ~SYM[1] <= ~SYM[0][~IF~SIZE[~TYP[10]]~THEN~ARG[10]~ELSE0~FI];
                    end
                end
            end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ showPpr t

    portB = case termToData wmB of
      Right WriteFirst -> [I.i|
        // Port B WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[12]) begin
            if(~ARG[13]) begin
                ~SYM[2] <= ~SYM[0][~IF~SIZE[~TYP[15]]~THEN~ARG[15]~ELSE0~FI];
                if(~ARG[14]) begin
                    ~SYM[2] <= ~ARG[16];
                    ~SYM[0][~IF~SIZE[~TYP[15]]~THEN~ARG[15]~ELSE0~FI] <= ~ARG[16];
                end
            end
        end|]
      Right ReadFirst -> [I.i|
        // Port B ReadFirst
            always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[12]) begin
                if(~ARG[13]) begin
                    ~SYM[2] <= ~SYM[0][~IF~SIZE[~TYP[15]]~THEN~ARG[15]~ELSE0~FI];
                    if(~ARG[14]) begin
                        ~SYM[0][~IF~SIZE[~TYP[15]]~THEN~ARG[15]~ELSE0~FI] <= ~ARG[16];
                    end
                end
            end|]
      Right NoChange -> [I.i|
        // Port B NoChange
            always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[12]) begin
                if(~ARG[13]) begin
                    if(~ARG[14]) begin
                        ~SYM[0][~IF~SIZE[~TYP[15]]~THEN~ARG[15]~ELSE0~FI] <= ~ARG[16];
                    end else begin
                        ~SYM[2] <= ~SYM[0][~IF~SIZE[~TYP[15]]~THEN~ARG[15]~ELSE0~FI];
                    end
                end
            end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ showPpr t

    post = [I.i|
      assign ~RESULT = {~SYM[1], ~SYM[2]};
      // end trueDualPortBlockRam"|]

trueDualPortBlockRamSystemVerilog :: BlackBoxFunction
trueDualPortBlockRamSystemVerilog _isD _primName args _ty = return bb
  where
    meta bbKi = emptyBlackBoxMeta{bbKind=bbKi}
    [_,_,_,_,_,_,_,_,_,Left wmA,Left wmB,_clkA, _enA, _wEnA, _nAddrA, _datA, _clkB, _enB, _wEnB, _nAddrB, _datB] = args
    bb = case runParse (pack $ I.unindent $ pre ++ portA ++ portB ++ post) of
       Success t    -> Right (meta TDecl, BBTemplate t)
       _            -> Left "internal error: parse fail"
    pre = [I.i|
        // trueDualPortBlockRam begin
        // Shared memory
        logic [~SIZE[~TYP[11]]-1:0] ~GENSYM[mem][0] [~LIT[1]-1:0];

        ~SIGD[~GENSYM[data_slow][1]][11];
        ~SIGD[~GENSYM[data_fast][2]][16];|]

    portA = case termToData wmA of
      Right WriteFirst -> [I.i|
        // Port A WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[7]) begin
            if(~ARG[8]) begin
                ~SYM[1] <= ~SYM[0][~ARG[10]];
                if(~ARG[9]) begin
                    ~SYM[1] <= ~ARG[11];
                    ~SYM[0][~ARG[10]] <= ~ARG[11];
                end
            end
        end|]
      Right ReadFirst -> [I.i|
        // Port A ReadFirst
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[7]) begin
                if(~ARG[8]) begin
                    ~SYM[1] <= ~SYM[0][~ARG[10]];
                    if(~ARG[9]) begin
                        ~SYM[0][~ARG[10]] <= ~ARG[11];
                    end
                end
            end|]
      Right NoChange -> [I.i|
        // Port A NoChange
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[7]) begin
                if(~ARG[8]) begin
                    if(~ARG[9]) begin
                        ~SYM[0][~ARG[10]] <= ~ARG[11];
                    end else begin
                        ~SYM[1] <= ~SYM[0][~ARG[10]];
                    end
                end
            end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ showPpr t

    portB = case termToData wmB of
      Right WriteFirst -> [I.i|
        // Port B WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[12]) begin
            if(~ARG[13]) begin
                ~SYM[2] <= ~SYM[0][~ARG[15]];
                if(~ARG[14]) begin
                    ~SYM[2] <= ~ARG[16];
                    ~SYM[0][~ARG[15]] <= ~ARG[16];
                end
            end
        end|]
      Right ReadFirst -> [I.i|
        // Port B ReadFirst
            always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[12]) begin
                if(~ARG[13]) begin
                    ~SYM[2] <= ~SYM[0][~ARG[15]];
                    if(~ARG[14]) begin
                        ~SYM[0][~ARG[15]] <= ~ARG[16];
                    end
                end
            end|]
      Right NoChange -> [I.i|
        // Port B NoChange
            always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[12]) begin
                if(~ARG[13]) begin
                    if(~ARG[14]) begin
                        ~SYM[0][~ARG[15]] <= ~ARG[16];
                    end else begin
                        ~SYM[2] <= ~SYM[0][~ARG[15]];
                    end
                end
            end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ showPpr t

    post = [I.i|
      assign ~RESULT = {~SYM[1], ~SYM[2]};
      // end trueDualPortBlockRam"|]

trueDualPortBlockRamVHDL :: BlackBoxFunction
trueDualPortBlockRamVHDL _isD _primName args _ty = return bb
  where
    meta bbKi = emptyBlackBoxMeta{bbKind=bbKi}
    [_,_,_,_,_,_,_,_,_,Left wmA,Left wmB,_clkA, _enA, _wEnA, _nAddrA, _datA, _clkB, _enB, _wEnB, _nAddrB, _datB] = args
    bb = case runParse (pack $ I.unindent $ pre ++ portA ++ portB ++ post) of
       Success t    -> Right (meta TDecl, BBTemplate t)
       _            -> Left "internal error: parse fail"
    pre = [I.i|
        -- trueDualPortBlockRam begin
        ~GENSYM[~RESULT_trueDualPortBlockRam][1] : block
          -- Shared memory
          type mem_type is array (~LIT[1]-1 downto 0 ) of ~TYP[11];
          shared variable mem : mem_type;
          signal ~GENSYM[data_slow][2] : ~TYP[11];
          signal ~GENSYM[data_fast][3] : ~TYP[16];
        begin
        |]
    portA = case termToData wmA of
      Right WriteFirst -> [I.i|
        -- Port A WriteFirst
        process(~ARG[7])
        begin
            if(rising_edge(~ARG[7])) then
                  if(~ARG[8]) then
                    if(~ARG[9]) then
                        mem(to_integer(~ARG[10])) := ~ARG[11];
                    end if;
                    ~SYM[2] <= mem(to_integer(~ARG[10]));
                end if;
            end if;
        end process;|]
      Right ReadFirst -> [I.i|
        -- Port A ReadFirst
        process(~ARG[7])
        begin
            if(rising_edge(~ARG[7])) then
                  if(~ARG[8]) then
                    ~SYM[2] <= mem(to_integer(~ARG[10]));
                    if(~ARG[9]) then
                        mem(to_integer(~ARG[10])) := ~ARG[11];
                    end if;
                end if;
            end if;
        end process;|]
      Right NoChange -> [I.i|
        -- Port A NoChange
        process(~ARG[7])
        begin
            if(rising_edge(~ARG[7])) then
                  if(~ARG[8]) then
                    if(~ARG[9]) then
                        mem(to_integer(~ARG[10])) := ~ARG[11];
                    else
                        ~SYM[2] <= mem(to_integer(~ARG[10]));
                    end if;
                end if;
            end if;
        end process;|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ showPpr t

    portB = case termToData wmB of
      Right WriteFirst -> [I.i|
        -- Port A WriteFirst
        process(~ARG[12])
        begin
            if(rising_edge(~ARG[12])) then
                  if(~ARG[13]) then
                    if(~ARG[14]) then
                        mem(to_integer(~ARG[15])) := ~ARG[16];
                    end if;
                    ~SYM[3] <= mem(to_integer(~ARG[15]));
                end if;
            end if;
        end process;|]
      Right ReadFirst -> [I.i|
        -- Port A ReadFirst
        process(~ARG[12])
        begin
            if(rising_edge(~ARG[12])) then
                  if(~ARG[13]) then
                    ~SYM[3] <= mem(to_integer(~ARG[15]));
                    if(~ARG[14]) then
                        mem(to_integer(~ARG[15])) := ~ARG[16];
                    end if;
                end if;
            end if;
        end process;|]
      Right NoChange -> [I.i|
        -- Port A NoChange
        process(~ARG[12])
        begin
            if(rising_edge(~ARG[12])) then
                  if(~ARG[13]) then
                    if(~ARG[14]) then
                        mem(to_integer(~ARG[15])) := ~ARG[16];
                    else
                        ~SYM[3] <= mem(to_integer(~ARG[15]));
                    end if;
                end if;
            end if;
        end process;|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ showPpr t
    post = [I.i|
      ~RESULT <= (~SYM[2], ~SYM[3]);
      end block;
      -- end trueDualPortBlockRam"|]
