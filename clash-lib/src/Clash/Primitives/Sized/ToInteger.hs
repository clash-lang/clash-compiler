{-|
  Copyright   :  (C) 2020,2022 QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox implementations for "Clash.Sized.Internal.*.toInteger#".
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
module Clash.Primitives.Sized.ToInteger
  ( bvToIntegerVerilog
  , bvToIntegerVHDL
  , indexToIntegerVerilog
  , indexToIntegerVHDL
  , signedToIntegerVerilog
  , signedToIntegerVHDL
  , unsignedToIntegerVerilog
  , unsignedToIntegerVHDL
  )
where

import qualified Control.Lens as Lens
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (pack)
import System.IO (hPutStrLn, stderr)
import Text.Trifecta.Result (Result(Success))

#if MIN_VERSION_ghc(9,8,0)
import GHC.Unit.Module.Warnings (emptyWarningCategorySet)
import GHC.Utils.Error
  (DiagOpts(..), mkPlainDiagnostic, mkPlainMsgEnvelope, pprLocMsgEnvelopeDefault)
import GHC.Utils.Outputable
  (blankLine, empty, int, integer, showSDocUnsafe, text, ($$), ($+$), (<+>),
   defaultSDocContext )
import qualified GHC.Utils.Outputable as Outputable
import GHC.Types.Error (DiagnosticReason (WarningWithoutFlag))
import GHC.Types.SrcLoc (isGoodSrcSpan)
#elif MIN_VERSION_ghc(9,6,0)
import GHC.Utils.Error
  (DiagOpts(..), mkPlainDiagnostic, mkPlainMsgEnvelope, pprLocMsgEnvelopeDefault)
import GHC.Utils.Outputable
  (blankLine, empty, int, integer, showSDocUnsafe, text, ($$), ($+$), (<+>),
   defaultSDocContext )
import qualified GHC.Utils.Outputable as Outputable
import GHC.Types.Error (DiagnosticReason (WarningWithoutFlag))
import GHC.Types.SrcLoc (isGoodSrcSpan)
#elif MIN_VERSION_ghc(9,4,0)
import GHC.Utils.Error
  (DiagOpts(..), mkPlainDiagnostic, mkPlainMsgEnvelope, pprLocMsgEnvelope)
import GHC.Utils.Outputable
  (blankLine, empty, int, integer, showSDocUnsafe, text, ($$), ($+$), (<+>),
   defaultSDocContext )
import qualified GHC.Utils.Outputable as Outputable
import GHC.Types.Error (DiagnosticReason (WarningWithoutFlag))
import GHC.Types.SrcLoc (isGoodSrcSpan)
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Error (mkPlainWarnMsg, pprLocMsgEnvelope)
import GHC.Utils.Outputable
  (blankLine, empty, int, integer, showSDocUnsafe, text, ($$), ($+$), (<+>))
import qualified GHC.Utils.Outputable as Outputable
import GHC.Types.SrcLoc (isGoodSrcSpan)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Session (unsafeGlobalDynFlags)
import GHC.Utils.Error (mkPlainWarnMsg, pprLocErrMsg)
import GHC.Utils.Outputable
  (blankLine, empty, int, integer, showSDocUnsafe, text, ($$), ($+$), (<+>))
import qualified GHC.Utils.Outputable as Outputable
import GHC.Types.SrcLoc (isGoodSrcSpan)
#else
import DynFlags (unsafeGlobalDynFlags)
import ErrUtils (mkPlainWarnMsg, pprLocErrMsg)
import Outputable
  (blankLine, empty, int, integer, showSDocUnsafe, text, ($$), ($+$), (<+>))
import qualified Outputable
import SrcLoc (isGoodSrcSpan)
#endif

import Clash.Annotations.Primitive (HDL (Verilog,VHDL))
import Clash.Core.Type (Type (LitTy), LitTy (NumTy))
import Clash.Netlist.BlackBox.Parser (runParse)
import Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta (bbKind), TemplateKind (TExpr),
   emptyBlackBoxMeta)
import Clash.Netlist.Types
  (BlackBox (BBTemplate), HWType (..), curCompNm, intWidth)
import Clash.Util (clogBase)

bvToIntegerVerilog, bvToIntegerVHDL, indexToIntegerVerilog,
  indexToIntegerVHDL,  signedToIntegerVerilog, signedToIntegerVHDL,
  unsignedToIntegerVerilog, unsignedToIntegerVHDL :: BlackBoxFunction

bvToIntegerVerilog = toIntegerBB Verilog (BitVector 0)
bvToIntegerVHDL = toIntegerBB VHDL (BitVector 0)
indexToIntegerVerilog = toIntegerBB Verilog (Index 0)
indexToIntegerVHDL = toIntegerBB VHDL (Index 0)
signedToIntegerVerilog = toIntegerBB Verilog (Signed 0)
signedToIntegerVHDL = toIntegerBB VHDL (Signed 0)
unsignedToIntegerVerilog = toIntegerBB Verilog (Unsigned 0)
unsignedToIntegerVHDL = toIntegerBB VHDL (Unsigned 0)

toIntegerBB :: HDL -> HWType -> BlackBoxFunction
toIntegerBB hdl hty _isD _primName args _ty = do
  case args of
    (Right (LitTy (NumTy i)):_) -> do
      iw <- Lens.view intWidth
      let i1 = width i
      when (fromInteger i1 > iw) $ do
        (_,sp) <- Lens.use curCompNm
        let srcInfo1 | isGoodSrcSpan sp = srcInfo
                     | otherwise        = empty
#if MIN_VERSION_ghc(9,8,0)
            opts     = DiagOpts mempty mempty emptyWarningCategorySet emptyWarningCategorySet False False Nothing defaultSDocContext
            diag     = mkPlainDiagnostic WarningWithoutFlag [] (warnMsg i1 iw $+$ blankLine $+$ srcInfo1)
            warnMsg1 = mkPlainMsgEnvelope opts sp diag
            warnMsg2 = pprLocMsgEnvelopeDefault warnMsg1
#elif MIN_VERSION_ghc(9,6,0)
            opts     = DiagOpts mempty mempty False False Nothing defaultSDocContext
            diag     = mkPlainDiagnostic WarningWithoutFlag [] (warnMsg i1 iw $+$ blankLine $+$ srcInfo1)
            warnMsg1 = mkPlainMsgEnvelope opts sp diag
            warnMsg2 = pprLocMsgEnvelopeDefault warnMsg1
#elif MIN_VERSION_ghc(9,4,0)
            opts     = DiagOpts mempty mempty False False Nothing defaultSDocContext
            diag     = mkPlainDiagnostic WarningWithoutFlag [] (warnMsg i1 iw $+$ blankLine $+$ srcInfo1)
            warnMsg1 = mkPlainMsgEnvelope opts sp diag
            warnMsg2 = pprLocMsgEnvelope warnMsg1
#elif MIN_VERSION_ghc(9,2,0)
            warnMsg1 = mkPlainWarnMsg sp (warnMsg i1 iw $+$ blankLine $+$ srcInfo1)
            warnMsg2 = pprLocMsgEnvelope warnMsg1
#else
            warnMsg1 = mkPlainWarnMsg unsafeGlobalDynFlags sp (warnMsg i1 iw $+$ blankLine $+$ srcInfo1)
            warnMsg2 = pprLocErrMsg warnMsg1
#endif

        liftIO (hPutStrLn stderr (showSDocUnsafe warnMsg2))
    _ -> return ()
  return ((meta,) <$> bb)
 where
  meta = emptyBlackBoxMeta{bbKind=TExpr}

  bb = BBTemplate <$> case runParse (pack bbText) of
         Success t -> Right t
         _         -> Left "internal error: parse fail"

  bbText = case hdl of
    VHDL -> case hty of
      BitVector {} -> "~IF~SIZE[~TYP[1]]~THENsigned(std_logic_vector(resize(unsigned(~ARG[1]),~SIZE[~TYPO])))~ELSEto_signed(0,64)~FI"
      Index {}     -> "~IF~SIZE[~TYP[0]]~THENsigned(std_logic_vector(resize(~ARG[0],~SIZE[~TYPO])))~ELSEto_signed(0,64)~FI"
      Signed {}    -> "~IF~SIZE[~TYP[0]]~THENresize(~ARG[0],~SIZE[~TYPO])~ELSEto_signed(0,64)~FI"
      Unsigned {}  -> "~IF~SIZE[~TYP[0]]~THENsigned(std_logic_vector(resize(~ARG[0],~SIZE[~TYPO])))~ELSEto_signed(0,64)~FI"
      _            -> error "internal error"
    _ -> case hty of
      BitVector {} -> "~IF~SIZE[~TYP[1]]~THEN~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[1]]]~THEN$unsigned(~VAR[bv][1][0+:~SIZE[~TYPO]])~ELSE$unsigned({{(~SIZE[~TYPO]-~SIZE[~TYP[1]]) {1'b0}},~VAR[bv][1]})~FI~ELSE~SIZE[~TYPO]'sd0~FI"
      Index {}     -> "~IF~SIZE[~TYP[0]]~THEN~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[0]]]~THEN$unsigned(~VAR[i][0][0+:~SIZE[~TYPO]])~ELSE$unsigned({{(~SIZE[~TYPO]-~SIZE[~TYP[0]]) {1'b0}},~VAR[i][0]})~FI~ELSE~SIZE[~TYPO]'sd0~FI"
      Signed {}    -> "~IF~SIZE[~TYP[0]]~THEN~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[0]]]~THEN$signed(~VAR[i][0][0+:~SIZE[~TYPO]])~ELSE$signed({{(~SIZE[~TYPO]-~SIZE[~TYP[0]]) {~VAR[i][0][~SIZE[~TYP[0]]-1]}},~VAR[i][0]})~FI~ELSE~SIZE[~TYPO]'sd0~FI"
      Unsigned {}  -> "~IF~SIZE[~TYP[0]]~THEN~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[0]]]~THEN$unsigned(~VAR[i][0][0+:~SIZE[~TYPO]])~ELSE$unsigned({{(~SIZE[~TYPO]-~SIZE[~TYP[0]]) {1'b0}},~VAR[i][0]})~FI~ELSE~SIZE[~TYPO]'sd0~FI"
      _            -> error "internal error"

  tyName = case hty of
    BitVector {} -> text "BitVector"
    Index {} -> text "Index"
    Signed {} -> text "Signed"
    Unsigned {} -> text "Unsigned"
    _ -> error "internal error"

  width i = case hty of
    Index {} -> maybe 0 toInteger (clogBase 2 i)
    _ -> i

  warnMsg i iw =
   tyName Outputable.<> text ".toInteger: Integer width," <+> int iw Outputable.<>
   text ", is smaller than" <+> tyName <+> text "width," <+> integer i Outputable.<>
   text ". Dropping MSBs." $+$
   text "Are you using 'fromIntegral' to convert between types?" <+>
   text "Use 'bitCoerce' instead."

  srcInfo =
   text "NB: The source location of the error is not exact, only indicative, as it is acquired after optimisations." $$
   text "The actual location of the error can be in a function that is inlined." $$
   text "To prevent inlining of those functions, annotate them with a NOINLINE pragma."
