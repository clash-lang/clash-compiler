{-|
  Copyright   :  (C) 2022-2023, Google Inc
                     2022,      QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Black box implementation for primitives in "Clash.Cores.Xilinx.Ila".
-}

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.Ila.Internal where

import Prelude
import qualified Clash.Prelude as C

import Control.Monad (when, zipWithM)
import Control.Monad.State (State)
import Data.Either (lefts, rights)
import Data.List (zip4, group)
import Data.List.Infinite((...), Infinite((:<)))
import Data.Proxy (Proxy(..))
import Data.String.Interpolate (__i)
import Data.Maybe (isJust)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat, SomeNat(..), someNatVal)
import Language.Haskell.TH.Syntax (Lift)
import Text.Show.Pretty (ppShow)

import qualified Control.Lens as Lens
import qualified Data.List.Infinite as Infinite
import qualified Data.Text as T

import Clash.Annotations.SynthesisAttributes (Attr(StringAttr))
import Clash.Backend (Backend)
import Clash.Netlist.Types
import Clash.Netlist.BlackBox.Types
import Clash.Core.TermLiteral (TermLiteral(..), deriveTermLiteral, termToDataError)
import Clash.Core.TermLiteral.TH (deriveTermToData)
import Clash.Core.Type (Type(LitTy), LitTy(NumTy), coreView)
import Clash.Sized.Vector (Vec)

import qualified Clash.Netlist.Id as Id
import qualified Clash.Primitives.DSL as DSL
import qualified Clash.Util.Interpolate as I

import Clash.Cores.Xilinx.Internal
  ( TclPurpose(..)
  , IpConfig(..)
  , defIpConfig
  , property
  , renderTcl
  )

-- | Number of samples to store
data Depth
  = D1024
  | D2048
  | D4096
  | D8192
  | D16384
  | D32768
  | D65536
  | D131072
  deriving (Show, Lift)

depthToWord :: Depth -> Word
depthToWord = \case
  D1024   -> 1024
  D2048   -> 2048
  D4096   -> 4096
  D8192   -> 8192
  D16384  -> 16384
  D32768  -> 32768
  D65536  -> 65536
  D131072 -> 131072

data ProbeType
  = DataAndTrigger
  -- ^ Probe can be used for data collection and to trigger data capture
  | Data
  -- ^ Probe can only be used for data collection
  | Trigger
  -- ^ Probe can only be used to trigger data capture
  deriving (Eq, Show, Lift, Enum)

-- | Configures the static properties of an 'Clash.Cores.Xilinx.Ila.ila'. Note
-- that most properties (triggers, number of samples before/after trigger, ...)
-- are configured at runtime using Vivado. When applicable, configuration fields
-- will refer to the names of configuration labels mentioned in the product
-- guide.
--
-- Use 'Clash.Cores.Xilinx.Ila.ilaConfig' to construct this with some sensible
-- defaults.
data IlaConfig n = IlaConfig
  { probeNames :: Vec n String
  -- ^ Probe names. Clash will error if it cannot generate names passed here.
  , depth :: Depth
  -- ^ Number of samples to store. Corresponds to @C_DATA_DEPTH@.
  , captureControl :: Bool
  -- ^ Whether probes marked 'Trigger' or 'DataAndTrigger' can be used to control
  -- data capture. That is, a trigger marks the start of data collection, while
  -- capture control marks when to sample. Corresponds to @C_EN_STRG_QUAL@.
  , stages :: C.Index 7
  -- ^ Number of registers to insert at each probe. Supported values: 0-6.
  -- Corresponds to @C_INPUT_PIPE_STAGES@.
  , comparators :: Either Int (Vec n Int)
  -- ^ Comparators available at each probe. If 'Left', all probes will get the
  -- same number of comparators. If 'Right', each probe gets a configurable
  -- number of comparators. Supported values: 2 - 16. Corresponds to
  -- @C_PROBE<n>_MU_CNT@
  --
  -- __N.B.__: Xilinx strongly recommends to use the same number of comparators
  --           for every probe (without explanation).
  , probeTypes :: Either ProbeType (Vec n ProbeType)
  -- ^ Purpose of probe. If 'Left', all probes will be set to the same type. If
  -- 'Right', each probe type can be set individually. Also see 'ProbeType'.
  -- Corresponds to @C_PROBE<n>_TYPE@.
  , advancedTriggers :: Bool
  -- ^  Whether state machines can be used to describe trigger logic.
  -- Corresponds to @C_ADV_TRIGGER@.
  }
  deriving (Show, Lift)

-- XXX: I'd move this 'deriveTermLiteral' up, but Template Haskell complains..
deriveTermLiteral ''ProbeType
deriveTermLiteral ''Depth
instance KnownNat n => TermLiteral (IlaConfig n) where
  termToData = $(deriveTermToData ''IlaConfig)

probeTypesVec :: KnownNat n => IlaConfig n -> Vec n ProbeType
probeTypesVec = either C.repeat id . probeTypes

comparatorsVec :: KnownNat n => IlaConfig n -> Vec n Int
comparatorsVec = either C.repeat id . comparators

-- | Are all values in a list equal? If so, return the element.
areEqual :: Eq a => [a] -> Maybe a
areEqual = \case { [x:_] -> Just x; _ -> Nothing } . group

ilaBBF :: HasCallStack => BlackBoxFunction
ilaBBF _isD _primName args _resTys = Lens.view tcCache >>= go
 where
  go tcm
    | _:_:_:config:_ <- lefts args
    , _:_:(coreView tcm -> LitTy (NumTy n)):_ <- rights args
    , Just (SomeNat (Proxy :: Proxy n)) <- someNatVal n
    = case termToDataError @(IlaConfig n) config of
        Left s -> error ("ilaBBF, bad config:\n" <> s)
        Right c -> pure $ Right (bbMeta c, bb c)
    | otherwise = error $ "ilaBBF, bad args:\n" <> ppShow args

  bbMeta :: KnownNat n => IlaConfig n -> BlackBoxMeta
  bbMeta config = emptyBlackBoxMeta
    { bbKind = TDecl
    , bbRenderVoid = RenderVoid
    , bbIncludes =
        [ ( ("ila", "clash.tcl")
          , BBFunction (show 'ilaTclTF) 0 (ilaTclTF config)
          )
        ]
    }

  bb :: KnownNat n => IlaConfig n -> BlackBox
  bb config = BBFunction (show 'ilaTF) 0 (ilaTF config)

usedArguments :: [Int]
usedArguments = ilaConfig : clock : inputProbes
 where
  (    _knownDomain
    :< _ilaConstraint
    :< _1nConstraint
    :< ilaConfig
    :< clock
    :< (Infinite.take 8096 -> inputProbes)
    ) = (0...) -- This function is polyvariadic so in theory it supports an
               -- unlimited number of arguments. To prevent evaluation loops
               -- when forcing this argument to NF we limit it to a modest
               -- 8096 input ports.

ilaTF :: (HasCallStack, KnownNat n) => IlaConfig n -> TemplateFunction
ilaTF config = TemplateFunction usedArguments (const True) (ilaBBTF config)

checkNameCollision :: HasCallStack => T.Text -> DSL.TExpr -> DSL.TExpr
checkNameCollision userName tExpr@(DSL.TExpr _ (Identifier (Id.toText -> name) Nothing))
  | userName == name = tExpr
  | otherwise = error [I.i|
      Tried create a signal called '#{userName}', but identifier generation
      returned '#{name}'. Refusing to instantiate Ila with unreliable probe
      names.
  |]
checkNameCollision _ tExpr = error [I.i|
  Internal error: Expected 'TExpr' with the following form:

    TExpr _ (Identifier _ Nothing)

  got:

    #{ppShow tExpr}
|]

ilaBBTF ::
  forall s n .
  (Backend s, KnownNat n, HasCallStack) =>
  IlaConfig n ->
  BlackBoxContext ->
  State s Doc
ilaBBTF config bbCtx
  | (   _knownDomainDom
      : _ilaConstraint
      : _1nConstraint
      : _ilaConfig
      : clk
      : inputs
      ) <- map fst $ DSL.tInputs bbCtx
  , [ilaName] <- bbQsysIncName bbCtx
  , let inTys = map DSL.ety inputs
  = do
      let userInputNames = T.pack <$> C.toList (probeNames config)

      when (length inTys /= C.natToNum @n) $
        error [I.i|
          Number of input names did not match number of input probes. Expected
          #{length inTys} input name(s), got #{length userInputNames}. Got input
          name(s):

            #{ppShow userInputNames}
        |]

      ilaInstName <- Id.makeBasic (getIlaName (bbCtxName bbCtx))

      let
        inPs = filter ((> (0 :: Int)) . DSL.tySize . DSL.ety) inputs
        inNames = map (T.pack . ("probe" <>) . show) [(0 :: Int)..]
        inBVs = map (BitVector . (fromInteger . DSL.tySize . DSL.ety)) inPs

      DSL.declarationReturn bbCtx "ila_inst_block" $ do
        DSL.compInBlock ilaName (("clk", Bit) : zip inNames inBVs) []

        inProbes <- zipWithM DSL.assign inNames inPs
        inProbesBV <- zipWithM toNameCheckedBv userInputNames inProbes

        DSL.instDecl
          Empty
          (Id.unsafeMake ilaName)
          ilaInstName
          [] -- Generics / parameters
          (("clk", clk) : zip inNames inProbesBV)
          [] -- outputs

        pure []

  | otherwise = error $ "ilaBBTF, bad bbCtx: " <> ppShow bbCtx
 where
  -- The HDL attribute 'KEEP' is added to the signals connected to the
  -- probe ports so they are not optimized away by the synthesis tool.
  keepAttrs = [StringAttr "KEEP" "true"]

  toNameCheckedBv nameHint inProbe =
    checkNameCollision nameHint <$>
      DSL.toBvWithAttrs keepAttrs nameHint inProbe

  -- Return user-friendly name given a context name hint. Note that we ignore
  -- @__VOID_TDECL_NOOP__@. It is created by 'mkPrimitive' whenever a user hint
  -- is _not_ given and the primitive returns a zero-width type.
  getIlaName :: Maybe T.Text -> T.Text
  getIlaName Nothing = "ila_inst"
  getIlaName (Just "result") = getIlaName Nothing
  getIlaName (Just "__VOID_TDECL_NOOP__") = getIlaName Nothing
  getIlaName (Just s) = s

ilaTclTF :: (HasCallStack, KnownNat n) => IlaConfig n -> TemplateFunction
ilaTclTF config = TemplateFunction usedArguments (const True) (ilaTclBBTF config)

ilaTclBBTF ::
  forall s n .
  (HasCallStack, KnownNat n, Backend s) =>
  IlaConfig n ->
  BlackBoxContext ->
  State s Doc
ilaTclBBTF config@IlaConfig{..} bbCtx
  | [ilaName] <- bbQsysIncName bbCtx
  , (   _knownDomainDom
    : _IlaConstraint
    : _1nConstraint
    : _ilaConfig
    : _clk
    : inputs
    ) <- map fst $ DSL.tInputs bbCtx
  , let inTys = map DSL.ety inputs
  = pure $ renderTcl $ pure $ IpConfigPurpose $
      (defIpConfig "ila" "6.2" ilaName){properties=properties inTys}
  | otherwise = error $ "ilaBBTF, bad bbCtx:\n\n" <> ppShow bbCtx
 where
  probesTypesL = C.toList (probeTypesVec config)
  compsL = C.toList (comparatorsVec config)
  sameMu = areEqual compsL

  properties inTys = globalProperties inTys <> portProperties inTys

  globalProperties inTys =
    [ property @Int  "C_NUM_OF_PROBES" (length inTys)
    , property @Word "C_INPUT_PIPE_STAGES" (fromIntegral stages)
    , property @Word "C_DATA_DEPTH" (depthToWord depth)
    , property @Bool "ALL_PROBE_SAME_MU" (isJust sameMu)
    , property @Int  "C_EN_STRG_QUAL" (if captureControl then 1 else 0)
    , property @Bool "C_TRIGIN_EN" False
    , property @Bool "C_ADV_TRIGGER" advancedTriggers
    ] <>
    [ property @Int "ALL_PROBE_SAME_MU_CNT" mu | Just mu <- [sameMu]
    ]

  portProperties inTys = concat $
    [ [ property @Int [__i|C_PROBE#{i}_WIDTH|] width
      , property @Int [__i|C_PROBE#{i}_TYPE|] (fromEnum probeType)
      , property @Int [__i|C_PROBE#{i}_MU_CNT|] compC
      ]
    | (i, ty, probeType, compC) <- zip4 [(0 :: Int)..] inTys probesTypesL compsL
    , let width = fromInteger $ DSL.tySize ty
    ]
