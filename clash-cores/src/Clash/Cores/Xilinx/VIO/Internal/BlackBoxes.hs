{-|
  Copyright   :  (C) 2022-2023, Google Inc
                     2022,      QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox implementation for primitives in "Clash.Cores.Xilinx.VIO".
-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.VIO.Internal.BlackBoxes (vioProbeBBF) where

import Prelude

import GHC.Stack (HasCallStack)

import Data.Foldable (fold)
import Data.List.Infinite((...), Infinite((:<)))
import Data.String.Interpolate (__i)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import Text.Show.Pretty (ppShow)

import qualified Data.List.Infinite as Infinite
import qualified Data.Text as T (Text, pack, concat)

import Control.Arrow (first)
import Control.Monad (when, zipWithM)
import Control.Monad.State (State)
import Control.Exception (assert)

import qualified Clash.Netlist.Id as Id
import qualified Clash.Primitives.DSL as DSL
import Clash.Backend (Backend)
import Clash.Core.Var (Attr' (StringAttr'))
import Clash.Netlist.Expr (bits, fromBits)
import Clash.Netlist.Types
  ( Size
  , Expr(..)
  , HWType(..)
  , EntityOrComponent(..)
  , TemplateFunction(..)
  , BlackBoxContext(..)
  , BlackBox(BBFunction)
  )
import Clash.Netlist.BlackBox.Types
  ( TemplateKind(..)
  , RenderVoid(..)
  , BlackBoxFunction
  , BlackBoxMeta(..)
  , emptyBlackBoxMeta
  )

import Clash.Cores.Xilinx.Internal
  ( TclPurpose(..)
  , IpConfig(..)
  , defIpConfig
  , property
  , renderTcl
  )

vioProbeBBF :: HasCallStack => BlackBoxFunction
vioProbeBBF _isD _primName _args _resTys = pure $ Right (bbMeta, bb)
 where
   bbMeta :: BlackBoxMeta
   bbMeta = emptyBlackBoxMeta
     { bbKind = TDecl
     , bbRenderVoid = RenderVoid
     , bbIncludes =
         [ ( ("vioProbe", "clash.tcl")
           , BBFunction (show 'vioProbeTclTF) 0 vioProbeTclTF
           )
         ]
     }

   bb :: BlackBox
   bb = BBFunction (show 'vioProbeTF) 0 vioProbeTF

usedArguments :: [Int]
usedArguments = (inputNames : outputNames : initOutValues : clock : inputProbes)
 where
  (    _knownDomain
    :< _vioConstraint
    :< inputNames
    :< outputNames
    :< initOutValues
    :< clock
    :< (Infinite.take 8096 -> inputProbes)
    ) = ((0::Int)...) -- This function is polyvariadic so in theory it supports an
                      -- unlimited number of arguments. To prevent evaluation loops
                      -- when forcing this argument to NF we limit it to a modest
                      -- 8096 input ports.

vioProbeTF :: HasCallStack => TemplateFunction
vioProbeTF =
  TemplateFunction
    usedArguments
    -- 'validateVioProbeBCC' already produces string describing
    -- failing checks, but 'TemplateFunction' cannot handle this
    -- yet. This is prepared to get updated easily as soon as the
    -- feature gets implemented in clash-lib.
    (maybe True error . validateVioProbeBBC)
    vioProbeBBTF

checkNameCollision :: HasCallStack => T.Text -> DSL.TExpr -> DSL.TExpr
checkNameCollision userName tExpr@(DSL.TExpr _ (Identifier (Id.toText -> name) Nothing))
  | userName == name = tExpr
  | otherwise = error [__i|
      Tried create a signal called '#{userName}', but identifier generation returned
      '#{name}'. Refusing to instantiate VIO with unreliable probe names.
  |]
checkNameCollision _ tExpr = error [__i|
  Internal error: Expected 'TExpr' with the following form:

    TExpr _ (Identifier _ Nothing)

  got:

    #{ppShow tExpr}
|]

vioProbeBBTF :: (Backend s, HasCallStack) => BlackBoxContext -> State s Doc
vioProbeBBTF bbCtx
  | (   _knownDomainDom
      : _vioConstraint
      : (DSL.getVec -> Just userInputNameExprs)
      : (DSL.getVec -> Just userOutputNameExprs)
      : _initialOutputProbeValues
      : clk
      : inputProbes
      ) <- map fst $ DSL.tInputs bbCtx
  , Just userInputNames <- mapM (fmap T.pack . DSL.getStr) userInputNameExprs
  , Just userOutputNames <- mapM (fmap T.pack . DSL.getStr) userOutputNameExprs
  , [vioProbeName] <- bbQsysIncName bbCtx
  , Right (inTys, outTys) <- probesFromTypes bbCtx
  , [tResult] <- map DSL.ety (DSL.tResults bbCtx)
  = do
      when (length inTys /= length userInputNames) $
        error [__i|
          Number of input names did not match number of input probes. Expected
          #{length inTys} input name(s), got #{length userInputNames}. Got input
          name(s):

            #{ppShow userInputNames}
        |]

      when (length outTys /= length userOutputNames) $
        error [__i|
          Number of output names did not match number of output probes. Expected
          #{length outTys} output name(s), got #{length userOutputNames}. Got output
          name(s):

            #{ppShow userOutputNames}
        |]

      vioProbeInstName <- Id.makeBasic (vioName (bbCtxName bbCtx))

      let
        inPs = filter ((> (0 :: Int)) . DSL.tySize . DSL.ety) inputProbes
        inNames = map (T.pack . ("probe_in" <>) . show) [0 :: Int, 1..]
        outNames = map (T.pack . ("probe_out" <>) . show) [0 :: Int, 1..]
        inBVs = map (BitVector . (fromInteger . DSL.tySize)) inTys
        outBVs = map (BitVector . (fromInteger . DSL.tySize)) outTys

      DSL.declarationReturn bbCtx "vio_inst_block" $ do
        DSL.compInBlock vioProbeName (("clk", Bit) : zip inNames inBVs)
          $ case tResult of
              Void{} -> []
              _      -> zip outNames outBVs

        inProbes <-
          case inPs of
            [ singleInput ] ->
              case DSL.ety singleInput of
                Vector{}  -> DSL.unvec "vec" singleInput
                Product{} -> DSL.deconstructProduct singleInput inNames
                _         -> pure <$> DSL.assign (head inNames) singleInput
            _ -> zipWithM DSL.assign inNames inPs

        outProbes <- zipWithM DSL.declare outNames outTys
        outProbesBV <- zipWithM toNameCheckedBv userOutputNames outProbes
        inProbesBV <- zipWithM toNameCheckedBv userInputNames inProbes

        DSL.instDecl Empty (Id.unsafeMake vioProbeName) vioProbeInstName []
          (("clk", clk) : zip inNames inProbesBV)
          (zip outNames outProbesBV)

        case tResult of
          Vector{}  -> pure <$> DSL.vec outProbes
          Product{} -> pure $ pure $ DSL.constructProduct tResult outProbes
          _         -> pure outProbes

  | otherwise = error $ "vioProbeBBTF, bad bbCtx: " <> show bbCtx
 where
  toNameCheckedBv x =
    -- The HDL attribute 'KEEP' is added to the signals connected to the
    -- probe ports so they are not optimized away by the synthesis tool.
    fmap (checkNameCollision x) . DSL.toBvWithAttrs [StringAttr' "KEEP" "true"] x

  -- Return user-friendly name given a context name hint. Note that we ignore
  -- @__VOID_TDECL_NOOP__@. It is created by 'mkPrimitive' whenever a user hint
  -- is _not_ given and the primitive returns a zero-width type.
  --
  -- XXX: Is the input every 'Nothing' for non-recursive calls? It looks like
  --      Clash always picks a context hint.
  vioName :: Maybe T.Text -> T.Text
  vioName Nothing = "vio_inst"
  vioName (Just "result") = vioName Nothing
  vioName (Just "__VOID_TDECL_NOOP__") = vioName Nothing
  vioName (Just s) = s

vioProbeTclTF :: HasCallStack => TemplateFunction
vioProbeTclTF =
  TemplateFunction
    usedArguments
    -- 'validateVioProbeBCC' already produces string describing
    -- failing checks, but 'TemplateFunction' cannot handle this
    -- yet. This is prepared to get updated easily as soon as the
    -- feature gets implemented in clash-lib.
    (maybe True error . validateVioProbeBBC)
    vioProbeTclBBTF

vioProbeTclBBTF ::
  forall s.
  (HasCallStack, Backend s) =>
  BlackBoxContext ->
  State s Doc
vioProbeTclBBTF bbCtx
  | ( _knownDomainDom
    : _vioConstraint
    : _inputNames
    : _outputNames
    : initialOutputProbeValues
    : _clk
    : _inputProbes
    ) <- map fst $ DSL.tInputs bbCtx
  , [vioProbeName] <- bbQsysIncName bbCtx
  , Right (inTys, outTys) <- probesFromTypes bbCtx
  = do

    let
      outIVals = case DSL.ety initialOutputProbeValues of
        CustomProduct{} ->
          -- TODO: support custom bit representations for S&P types
          error "Custom bit representations are not supported within VIOs yet"
        _ ->
          constantProbeValues
            (DSL.ety initialOutputProbeValues)
            (DSL.eex initialOutputProbeValues)

    when (length outIVals /= length outTys) $
      error "Internal: Unexpected Structure (this should not happen)"

    pure $ renderTcl $ pure $ IpConfigPurpose
      $ (defIpConfig "vio" "3.0" vioProbeName)
           { properties = vioProbeProperties inTys outTys outIVals
           }
  | otherwise = error $ "vioProbeBBTF, bad bbCtx: " <> show bbCtx
 where
  vioProbeProperties inTys outTys outIVals =
    [ property @Int "C_NUM_PROBE_IN" $ length inTys
    , property @Int "C_NUM_PROBE_OUT" $ length outTys
    ] <>
    [ property @Int (T.concat ["C_PROBE_IN", T.pack $ show i, "_WIDTH"]) n
    | (i,t) <- zip [0 :: Int, 1..] inTys
    , let n = fromInteger $ DSL.tySize t
    ] <>
    [ property @Int (T.concat ["C_PROBE_OUT", T.pack $ show i, "_WIDTH"]) n
    | (i,t) <- zip [0 :: Int, 1..] outTys
    , let n = fromInteger $ DSL.tySize t
    ] <>
    [ property @Int (T.concat ["C_PROBE_OUT", T.pack $ show i, "_INIT_VAL"]) v
    | (i, v) <- zip [0 :: Int, 1..] outIVals
    ]

-- | Turns a known to be constant value 'Expr' of some known 'HWType'
-- into a list of initial probe values, where the probe distribution
-- is determined according to the 'HWType'.
constantProbeValues :: HasCallStack => HWType -> Expr -> [Int]
constantProbeValues ty expr = case bits (DSL.tySize ty) expr of
  Left failedExpr -> error [__i|
    Clash failed to determine a constant value for one of the expressions
    given as the inital output probe values. The failing expression is:

      #{ppShow failedExpr}

    The complete expression is:

      #{ppShow expr}

    As a quick fix: it may help to leverage the template haskell function
    $(lift ...) to get rid of this error message.
    |]
  Right x ->
    let res = map fromBits $ multi ty $ fold x
    in assert (all (<= 256) res) res

 where
  -- distributes the given bitstream to multiple probes
  multi :: HWType -> [Bool] -> [[Bool]]
  multi t bs = assert (length bs == DSL.tySize t) $ case t of
    Product _ _ ts -> bs `splitAts` map DSL.tySize ts
    Vector s t'    -> bs `splitAts` replicate s (DSL.tySize t')
    Void _         -> []
    _              -> [bs]

  -- splits the given bitstream according to the given sizes
  splitAts :: [Bool] -> [Size] -> [[Bool]]
  splitAts bs =
    reverse . fst . foldl (\(a, xs) m -> first (:a) $ splitAt m xs) ([], bs)

-- | Blackbox context validation.
validateVioProbeBBC :: BlackBoxContext -> Maybe String
validateVioProbeBBC bbCtx = case probesFromTypes bbCtx of
  Left err -> Just err
  Right _  -> Nothing

-- | Determines the number of specified input/output probes from the
-- argument and result types, every argument is mapped to a single
-- probe. See 'Clash.Cores.Xilinx.VIO' for the details on how result
-- types are mapped to probes.
probesFromTypes :: BlackBoxContext -> Either String ([HWType], [HWType])
probesFromTypes Context{..} = do
  is <- case map (\(_,x,_) -> x) bbInputs of
    (   _knownDomainDom
      : _VIOConstraint
      : _inputNames
      : _outputNames
      : _clk
      : _initialOutputProbeValues
      : xs
      ) -> inputProbesFromTypes xs
    _   -> Left "Internal: Unexpected Structure (this should not happen)"

  os <- case map snd bbResults of
    [t] -> probesFromSingleOrProductType t
    _   -> Left "Multiple result primitives are not supported"

  return (is, os)

 where
  inputProbesFromTypes xs
    | length xs > 256 = Left errProbeLimit
    | otherwise       = case xs of
        [t] -> probesFromSingleOrProductType t
        _   -> mapM singleProbe $ filter ((> (0 :: Int)) . DSL.tySize) xs

  probesFromSingleOrProductType = \case
    Vector s t        -> multibleProbes $ replicate s t
    Product _ _ xs    -> multibleProbes xs
    BiDirectional _ t -> probesFromSingleOrProductType t
    Annotated _ t     -> probesFromSingleOrProductType t
    Void{}            -> return []
    t                 -> pure <$> singleProbe t

  multibleProbes ts = do
    ps <- filter ((>= (1 :: Int)) . DSL.tySize) <$> mapM singleProbe ts
    when (length ps > 256) $ Left errProbeLimit
    return ps

  singleProbe ty =
    let s :: Int
        s = DSL.tySize ty
    in if s < 0 || s > 256
       then Left errProbeRange
       else return ty

  errProbeLimit = "At most 256 input/output probes are supported."
  errProbeRange = "Probe signals must be been between 1 and 256 bits wide."
