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
    [
        _nAddr           -- tyVar
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
      , Left regA        -- 7
      , Left regB        -- 8
      , _clkA            -- 9
      , _enA             -- 10
      , _wEnA            -- 11
      , _nAddrA          -- 12
      , _datA            -- 13
      , _clkB            -- 14
      , _enB             -- 15
      , _wEnB            -- 16
      , _nAddrB          -- 17
      , _datB            -- 18
      ] = args

    bb = case runParse (pack $ I.unindent $ pre ++ assignRegA ++ assignRegB ++ portA ++ portB ++ post) of
       Success t    -> Right (meta TDecl, BBTemplate t)
       _            -> Left "internal error: parse fail"
    pre = [I.i|
        // trueDualPortBlockRam begin
        // Shared memory
        reg [~SIZE[~TYP[13]]-1:0] ~GENSYM[mem][0] [~LIT[1]-1:0];

        reg ~SIGD[~GENSYM[data_slow][1]][13];
        reg ~SIGD[~GENSYM[data_fast][2]][18];

        // optional output registers, unused if the option is turned off
        reg ~SIGD[~GENSYM[otp_reg_slow][3]][13];
        reg ~SIGD[~GENSYM[otp_reg_fast][4]][18];|]

    regA' = case termToData regA of
      Right t -> t
      Left t -> error $ "Couldn't generate trueDualPortBlockRam output register A assignment, type is " ++ showPpr t

    regB' = case termToData regB of
      Right t -> t
      Left t -> error $ "Couldn't generate trueDualPortBlockRam output register B assignment, type is " ++ showPpr t

    assignRegA = if (not regA') then [I.i| |] else [I.i|
        // Assign output register A
        always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[9]) begin
          ~SYM[3] <= ~SYM[1];
        end|]

    assignRegB = if (not regB') then [I.i| |] else [I.i|
        // Assign output register B
        always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[14]) begin
          ~SYM[4] <= ~SYM[2];
        end|]

    portA = case termToData wmA of
      Right WriteFirst -> [I.i|
        // Port A WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[9]) begin
            if(~ARG[10]) begin
                ~SYM[1] <= ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI];
                if(~ARG[11]) begin
                    ~SYM[1] <= ~ARG[13];
                    ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI] <= ~ARG[13];
                end
            end
        end|]
      Right ReadFirst -> [I.i|
        // Port A ReadFirst
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[9]) begin
                if(~ARG[10]) begin
                    ~SYM[1] <= ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI];
                    if(~ARG[11]) begin
                        ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI] <= ~ARG[13];
                    end
                end
            end|]
      Right NoChange -> [I.i|
        // Port A NoChange
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[9]) begin
                if(~ARG[10]) begin
                    if(~ARG[11]) begin
                        ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI] <= ~ARG[13];
                    end else begin
                        ~SYM[1] <= ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI];
                    end
                end
            end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ showPpr t

    portB = case termToData wmB of
      Right WriteFirst -> [I.i|
        // Port B WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[14]) begin
            if(~ARG[15]) begin
                ~SYM[2] <= ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI];
                if(~ARG[16]) begin
                    ~SYM[2] <= ~ARG[18];
                    ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI] <= ~ARG[18];
                end
            end
        end|]
      Right ReadFirst -> [I.i|
        // Port B ReadFirst
            always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[14]) begin
                if(~ARG[15]) begin
                    ~SYM[2] <= ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI];
                    if(~ARG[16]) begin
                        ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI] <= ~ARG[18];
                    end
                end
            end|]
      Right NoChange -> [I.i|
        // Port B NoChange
            always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[14]) begin
                if(~ARG[15]) begin
                    if(~ARG[16]) begin
                        ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI] <= ~ARG[18];
                    end else begin
                        ~SYM[2] <= ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI];
                    end
                end
            end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ showPpr t

    post = case (regA', regB') of
      (False, False) -> [I.i|
        assign ~RESULT = {~SYM[1], ~SYM[2]};
        // end trueDualPortBlockRam |]
      (False, True) -> [I.i|
        assign ~RESULT = {~SYM[1], ~SYM[4]};
        // end trueDualPortBlockRam |]
      (True, False) -> [I.i|
        assign ~RESULT = {~SYM[3], ~SYM[2]};
        // end trueDualPortBlockRam |]
      (True, True) -> [I.i|
        assign ~RESULT = {~SYM[3], ~SYM[4]};
        // end trueDualPortBlockRam |]

trueDualPortBlockRamSystemVerilog :: BlackBoxFunction
trueDualPortBlockRamSystemVerilog _isD _primName args _ty = return bb
  where
    meta bbKi = emptyBlackBoxMeta{bbKind=bbKi}
    [_,_,_,_,_,_,_,_,_,Left wmA,Left wmB,Left regA,Left regB,_clkA, _enA, _wEnA, _nAddrA, _datA, _clkB, _enB, _wEnB, _nAddrB, _datB] = args
    bb = case runParse (pack $ I.unindent $ pre ++ assignRegA ++ assignRegB ++ portA ++ portB ++ post) of
       Success t    -> Right (meta TDecl, BBTemplate t)
       _            -> Left "internal error: parse fail"
    pre = [I.i|
        // trueDualPortBlockRam begin
        // Shared memory
        logic [~SIZE[~TYP[13]]-1:0] ~GENSYM[mem][0] [~LIT[1]-1:0];

        ~SIGD[~GENSYM[data_slow][1]][13];
        ~SIGD[~GENSYM[data_fast][2]][18];

        // optional output registers, unused if the option is turned off
        ~SIGD[~GENSYM[otp_reg_slow][3]][13];
        ~SIGD[~GENSYM[otp_reg_fast][4]][18];|]

    regA' = case termToData regA of
      Right t -> t
      Left t -> error $ "Couldn't generate trueDualPortBlockRam output register A assignment, type is " ++ showPpr t

    regB' = case termToData regB of
      Right t -> t
      Left t -> error $ "Couldn't generate trueDualPortBlockRam output register B assignment, type is " ++ showPpr t

    assignRegA = if (not regA') then [I.i| |] else [I.i|
        // Assign output register A
        always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[9]) begin
          ~SYM[3] <= ~SYM[1];
        end|]

    assignRegB = if (not regB') then [I.i| |] else [I.i|
        // Assign output register B
        always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[14]) begin
          ~SYM[4] <= ~SYM[2];
        end|]

    portA = case termToData wmA of
      Right WriteFirst -> [I.i|
        // Port A WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[9]) begin
            if(~ARG[10]) begin
                ~SYM[1] <= ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI];
                if(~ARG[11]) begin
                    ~SYM[1] <= ~ARG[13];
                    ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI] <= ~ARG[13];
                end
            end
        end|]
      Right ReadFirst -> [I.i|
        // Port A ReadFirst
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[9]) begin
                if(~ARG[10]) begin
                    ~SYM[1] <= ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI];
                    if(~ARG[11]) begin
                        ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI] <= ~ARG[13];
                    end
                end
            end|]
      Right NoChange -> [I.i|
        // Port A NoChange
            always @(~IF~ACTIVEEDGE[Rising][2]~THENposedge~ELSEnegedge~FI ~ARG[9]) begin
                if(~ARG[10]) begin
                    if(~ARG[11]) begin
                        ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI] <= ~ARG[13];
                    end else begin
                        ~SYM[1] <= ~SYM[0][~IF~SIZE[~TYP[12]]~THEN~ARG[12]~ELSE0~FI];
                    end
                end
            end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ showPpr t

    portB = case termToData wmB of
      Right WriteFirst -> [I.i|
        // Port B WriteFirst
        always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[14]) begin
            if(~ARG[15]) begin
                ~SYM[2] <= ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI];
                if(~ARG[16]) begin
                    ~SYM[2] <= ~ARG[18];
                    ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI] <= ~ARG[18];
                end
            end
        end|]
      Right ReadFirst -> [I.i|
        // Port B ReadFirst
            always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[14]) begin
                if(~ARG[15]) begin
                    ~SYM[2] <= ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI];
                    if(~ARG[16]) begin
                        ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI] <= ~ARG[18];
                    end
                end
            end|]
      Right NoChange -> [I.i|
        // Port B NoChange
            always @(~IF~ACTIVEEDGE[Rising][3]~THENposedge~ELSEnegedge~FI ~ARG[14]) begin
                if(~ARG[15]) begin
                    if(~ARG[16]) begin
                        ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI] <= ~ARG[18];
                    end else begin
                        ~SYM[2] <= ~SYM[0][~IF~SIZE[~TYP[17]]~THEN~ARG[17]~ELSE0~FI];
                    end
                end
            end|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ showPpr t

    post = case (regA', regB') of
      (False, False) -> [I.i|
        assign ~RESULT = {~SYM[1], ~SYM[2]};
        // end trueDualPortBlockRam |]
      (False, True) -> [I.i|
        assign ~RESULT = {~SYM[1], ~SYM[4]};
        // end trueDualPortBlockRam |]
      (True, False) -> [I.i|
        assign ~RESULT = {~SYM[3], ~SYM[2]};
        // end trueDualPortBlockRam |]
      (True, True) -> [I.i|
        assign ~RESULT = {~SYM[3], ~SYM[4]};
        // end trueDualPortBlockRam |]

trueDualPortBlockRamVHDL :: BlackBoxFunction
trueDualPortBlockRamVHDL _isD _primName args _ty = return bb
  where
    meta bbKi = emptyBlackBoxMeta{bbKind=bbKi}
    [_,_,_,_,_,_,_,_,_,Left wmA,Left wmB,Left regA,Left regB,_clkA, _enA, _wEnA, _nAddrA, _datA, _clkB, _enB, _wEnB, _nAddrB, _datB] = args
    bb = case runParse (pack $ I.unindent $ pre ++ assignRegA ++ assignRegB ++ portA ++ portB ++ post) of
       Success t    -> Right (meta TDecl, BBTemplate t)
       _            -> Left "internal error: parse fail"
    pre = [I.i|
        -- trueDualPortBlockRam begin
        ~GENSYM[~RESULT_trueDualPortBlockRam][1] : block
          -- Shared memory
          type mem_type is array (~LIT[1]-1 downto 0 ) of ~TYP[13];
          shared variable mem : mem_type;
          signal ~GENSYM[data_slow][2] : ~TYP[13];
          signal ~GENSYM[data_fast][3] : ~TYP[18];
          -- optional output registers, unused if the option is turned off
          signal ~GENSYM[otp_reg_slow][4] : ~TYP[13];
          signal ~GENSYM[otp_reg_fast][5] : ~TYP[18];
        begin
        |]
    regA' = case termToData regA of
      Right t -> t
      Left t -> error $ "Couldn't generate trueDualPortBlockRam output register A assignment, type is " ++ showPpr t
    regB' = case termToData regB of
      Right t -> t
      Left t -> error $ "Couldn't generate trueDualPortBlockRam output register B assignment, type is " ++ showPpr t
    assignRegA = if (not regA') then [I.i| |] else [I.i|
        -- Assign output register A
        process(~ARG[9])
        begin
            if(rising_edge(~ARG[9])) then
                ~SYM[4] <= ~SYM[2];
            end if;
        end process;|]
    assignRegB = if (not regB') then [I.i| |] else [I.i|
        -- Assign output register B
        process(~ARG[14])
        begin
            if(rising_edge(~ARG[14])) then
                ~SYM[5] <= ~SYM[3];
            end if;
        end process;|]
    portA = case termToData wmA of
      Right WriteFirst -> [I.i|
        -- Port A WriteFirst
        process(~ARG[9])
        begin
            if(rising_edge(~ARG[9])) then
                  if(~ARG[10]) then
                    ~SYM[2] <= mem(~IF~SIZE[~TYP[12]]~THENto_integer(~ARG[12])~ELSE0~FI);
                    if(~ARG[11]) then
                        ~SYM[2] <= ~ARG[13];
                        mem(~IF~SIZE[~TYP[12]]~THENto_integer(~ARG[12])~ELSE0~FI) := ~ARG[13];
                    end if;
                end if;
            end if;
        end process;|]
      Right ReadFirst -> [I.i|
        -- Port A ReadFirst
        process(~ARG[9])
        begin
            if(rising_edge(~ARG[9])) then
                  if(~ARG[10]) then
                    ~SYM[2] <= mem(~IF~SIZE[~TYP[12]]~THENto_integer(~ARG[12])~ELSE0~FI);
                    if(~ARG[11]) then
                        mem(~IF~SIZE[~TYP[12]]~THENto_integer(~ARG[12])~ELSE0~FI) := ~ARG[13];
                    end if;
                end if;
            end if;
        end process;|]
      Right NoChange -> [I.i|
        -- Port A NoChange
        process(~ARG[9])
        begin
            if(rising_edge(~ARG[9])) then
                  if(~ARG[10]) then
                    if(~ARG[11]) then
                        mem(~IF~SIZE[~TYP[12]]~THENto_integer(~ARG[12])~ELSE0~FI) := ~ARG[13];
                    else
                        ~SYM[2] <= mem(~IF~SIZE[~TYP[12]]~THENto_integer(~ARG[12])~ELSE0~FI);
                    end if;
                end if;
            end if;
        end process;|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port A template, type is " ++ showPpr t

    portB = case termToData wmB of
      Right WriteFirst -> [I.i|
        -- Port B WriteFirst
        process(~ARG[14])
        begin
            if(rising_edge(~ARG[14])) then
                  if(~ARG[15]) then
                    ~SYM[3] <= mem(~IF~SIZE[~TYP[17]]~THENto_integer(~ARG[17])~ELSE0~FI);
                    if(~ARG[16]) then
                        ~SYM[3] <= ~ARG[18];
                        mem(~IF~SIZE[~TYP[17]]~THENto_integer(~ARG[17])~ELSE0~FI) := ~ARG[18];
                    end if;
                end if;
            end if;
        end process;|]
      Right ReadFirst -> [I.i|
        -- Port B ReadFirst
        process(~ARG[14])
        begin
            if(rising_edge(~ARG[14])) then
                  if(~ARG[15]) then
                    ~SYM[3] <= mem(~IF~SIZE[~TYP[17]]~THENto_integer(~ARG[17])~ELSE0~FI);
                    if(~ARG[16]) then
                        mem(~IF~SIZE[~TYP[17]]~THENto_integer(~ARG[17])~ELSE0~FI) := ~ARG[18];
                    end if;
                end if;
            end if;
        end process;|]
      Right NoChange -> [I.i|
        -- Port B NoChange
        process(~ARG[14])
        begin
            if(rising_edge(~ARG[14])) then
                  if(~ARG[15]) then
                    if(~ARG[16]) then
                        mem(~IF~SIZE[~TYP[17]]~THENto_integer(~ARG[17])~ELSE0~FI) := ~ARG[18];
                    else
                        ~SYM[3] <= mem(~IF~SIZE[~TYP[17]]~THENto_integer(~ARG[17])~ELSE0~FI);
                    end if;
                end if;
            end if;
        end process;|]
      Left t -> error $ "Couldn't generate trueDualPortBlockRam port B template, type is " ++ showPpr t
    post = case (regA', regB') of
      (False, False) -> [I.i|
        ~RESULT <= (~SYM[2], ~SYM[3]);
        end block;
        -- end trueDualPortBlockRam|]
      (False, True) -> [I.i|
        ~RESULT <= (~SYM[2], ~SYM[5]);
        end block;
        -- end trueDualPortBlockRam|]
      (True, False) -> [I.i|
        ~RESULT <= (~SYM[4], ~SYM[3]);
        end block;
        -- end trueDualPortBlockRam|]
      (True, True) -> [I.i|
        ~RESULT <= (~SYM[4], ~SYM[5]);
        end block;
        -- end trueDualPortBlockRam|]
