{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}

module Clash.Primitives.Sized.Vector where

import Control.Monad.State
import Data.Semigroup.Monad
import Data.Text.Lazy (pack)
import Data.Text.Prettyprint.Doc.Extra
import Text.Trifecta.Result

import Clash.Backend
import Clash.Netlist.BlackBox
import Clash.Netlist.BlackBox.Parser
import Clash.Netlist.BlackBox.Types
import Clash.Netlist.Types
import Clash.Netlist.Util

import qualified Data.String.Interpolate as I
import qualified Data.String.Interpolate.Util as I

indexIntVerilog ::  BlackBoxFunction
indexIntVerilog _isD _resId _primName args _ty = return ((meta,) <$> bb)
 where
  meta = BlackBoxMeta
       { bbOutputReg = False
       , bbKind      = bbKi
       , bbLibrary   = []
       , bbImports   = []
       , bbIncludes  = []
       }

  bbKi = case args of
    [_nTy,_aTy,_kn,_v,Left ix]
      | isLiteral ix -> TExpr
    _ -> TDecl

  bb = case args of
    [_nTy,_aTy,_kn,_v,Left ix] | isLiteral ix ->
      Right (BBFunction "Clash.Primitives.Sized.Vector" 0 indexIntVerilogTF)
    _ ->
      BBTemplate <$> case runParse (pack (I.unindent bbText)) of
        Success t -> Right t
        _         -> Left "internal error: parse fail"

  bbText = [I.i|
    // index begin
    ~IF~SIZE[~TYP[1]]~THENwire ~TYPO ~GENSYM[vecArray][0] [0:~LIT[0]-1];
    genvar ~GENSYM[i][2];
    ~GENERATE
    for (~SYM[2]=0; ~SYM[2] < ~LIT[0]; ~SYM[2]=~SYM[2]+1) begin : ~GENSYM[mk_array][3]
      assign ~SYM[0][(~LIT[0]-1)-~SYM[2]] = ~VAR[vecFlat][1][~SYM[2]*~SIZE[~TYPO]+:~SIZE[~TYPO]];
    end
    ~ENDGENERATE
    assign ~RESULT = ~SYM[0][~ARG[2]];~ELSEassign ~RESULT = ~ERRORO;~FI
    // index end|]

indexIntVerilogTF :: TemplateFunction
indexIntVerilogTF = TemplateFunction used valid indexIntVerilogTemplate
 where
  used  = [1,2]
  valid = const True

indexIntVerilogTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
indexIntVerilogTemplate bbCtx = getMon $ case typeSize vTy of
  0 -> hdlTypeErrValue rTy
  _ -> case vec of
    Identifier i mM -> case mM of
      Just m ->
           expr False (Identifier i (Just (Nested m (Indexed (vTy,10,ixI ix)))))
      _ -> expr False (Identifier i (Just (Indexed (vTy,10,ixI ix))))
    _ -> error ("Expected Identifier: " ++ show vec)
 where
  [  _kn
   , (vec, vTy, _)
   , (ix, _, _)
   ] = bbInputs bbCtx

  (_,rTy) = bbResult bbCtx

  ixI :: Expr ->  Int
  ixI ix0 = case ix0 of
          Literal _ (NumLit i) -> fromInteger i
          BlackBoxE "GHC.Types.I#" _ _ _ _ ixCtx _ ->
            let (ix1,_,_) = head (bbInputs ixCtx)
            in  ixI ix1
          _ -> error ("Unexpected literal" ++ show ix)
