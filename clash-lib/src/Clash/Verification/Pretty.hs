{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
                  2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Verification
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Verification.Pretty
  ( pprPslProperty
  , pprSvaProperty
  , pprYosysSvaProperty

  -- * Debugging functions
  , pprProperty
  ) where

import           Clash.Annotations.Primitive      (HDL(..))
import           Clash.Signal.Internal            (ActiveEdge, ActiveEdge(..))
import           Clash.Verification.Internal      hiding (assertion)
import           Clash.Netlist.Types              (Declaration(..), Seq(..), Expr, CommentOrDirective(..), Sensitivity(..))
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)
import qualified Data.Text as Text                (pack)

data Symbol
  = TImpliesOverlapping
  | TImplies
  | Implies
  | BiImplies
  | Not
  | And
  | Or
  | To
  | Equals
  -- + [] ?
  | Assign
  | Is

------------------------------------------
--                 UTIL                 --
------------------------------------------
-- | Collapse constructs such as `next (next a)` down to `next[2] a`
squashBefore :: Assertion' a -> [Assertion' a]
squashBefore (CvBefore e1 e2) = e1s ++ e2s
 where
  e1s = case squashBefore e1 of {[] -> [e1]; es -> es}
  e2s = case squashBefore e2 of {[] -> [e2]; es -> es}
squashBefore _ = []

parensIf :: Bool -> Text -> Text
parensIf True s = "(" <> s <> ")"
parensIf False s = s

---------------------------------------
--                PSL                --
---------------------------------------
pslBinOp
  :: HDL
  -> Bool
  -> Symbol
  -> Assertion' Text
  -> Assertion' Text
  -> Text
pslBinOp hdl parens op e1 e2 =
  parensIf parens (e1' <> symbol hdl op <> e2')
 where
  e1' = pprPslAssertion hdl True e1
  e2' = pprPslAssertion hdl True e2

pslEdge :: HDL -> ActiveEdge -> Text -> Text
pslEdge SystemVerilog activeEdge clkId = pslEdge Verilog activeEdge clkId
pslEdge Verilog Rising clkId = "posedge " <> clkId
pslEdge Verilog Falling clkId = "negedge " <> clkId
pslEdge VHDL Rising clkId = "rising_edge(" <> clkId <> ")"
pslEdge VHDL Falling clkId = "falling_edge(" <> clkId <> ")"

-- | Taken from IEEE Std 1850-2010a, Annex B.1, p149
symbol :: HDL -> Symbol -> Text
symbol SystemVerilog = symbol Verilog
symbol Verilog = \case
  TImpliesOverlapping -> "|->"
  TImplies  -> "|=>"
  Implies   -> "->"
  BiImplies -> "<->"
  Not       -> "!"
  And       -> "&&"
  Or        -> "||"
  To        -> ":"
  Assign    -> "<="
  Is        -> "="
  Equals    -> "=="

symbol VHDL = \case
  TImpliesOverlapping -> "|->"
  TImplies  -> "|=>"
  Implies   -> " -> "
  BiImplies -> " <-> "
  Not       -> "not"
  And       -> " and "
  Or        -> " or "
  To        -> " to "
  Assign    -> "<="
  Is        -> "is"
  Equals    -> "="

-- | Pretty print Property. Doesn't print valid HDL, but can be used for
-- debugging purposes.
pprProperty :: Property dom -> Declaration
pprProperty (Property prop0) =
  let prop1 = fromMaybe "__autogen__" . fst <$> prop0 in
  pprPslProperty VHDL "prop" "clk" Rising prop1

pprPslProperty
  :: HDL
  -- ^ HDL to generate PSL expression for
  -> Text
  -- ^ Property name
  -> Text
  -- ^ Clock name
  -> ActiveEdge
  -- ^ Edge property should be sensitive to
  -> Property' Text
  -- ^ Assertion / Cover statement
  -> Declaration
pprPslProperty hdl propName clkId edge assertion = TickDecl . Comment $
  "psl property " <> propName <> " " <> symbol hdl Is <> "\n" <>
  "(" <> prop <> ") @(" <> pslEdge hdl edge clkId <> ")" <>
  ";\n" <> "psl " <> coverOrAssert <> " " <>
  propName <> ";"
 where
  (coverOrAssert, prop) =
    case assertion of
      CvCover e -> ("cover", pprPslAssertion hdl False e)
      CvAssert e -> ("assert", pprPslAssertion hdl False e)
      CvAssume e -> ("assume", pprPslAssertion hdl False e)

pprPslAssertion :: HDL -> Bool -> Assertion' Text -> Text
pprPslAssertion hdl parens e =
  case e of
    (CvPure p) -> p

    -- ModelSim/QuastaSim doesn't support booleans in PSL. Anytime we want to
    -- use a boolean literal we use (0 == 0) or (0 == 1) instead.
    (CvLit False) -> parensIf parens ("0" <> symbol hdl Equals <> "1")
    (CvLit True) -> parensIf parens ("0" <> symbol hdl Equals <> "0")

    (CvNot e1) ->
      parensIf parens (symbol hdl Not <> " " <> pprPslAssertion hdl True e1)
    (CvAnd e1 e2) -> pslBinOp1 And e1 e2
    (CvOr e1 e2) -> pslBinOp1 Or e1 e2
    (CvImplies e1 e2) -> pslBinOp1 Implies e1 e2

    (CvToTemporal e1) -> "{" <> pprPslAssertion hdl False e1 <> "}"

    (CvNext 0 e1) -> pprPslAssertion hdl parens e1
    (CvNext 1 e1) -> " ## " <> pprPslAssertion hdl True e1
    (CvNext n e1) -> " ##" <> Text.pack (show n) <> " " <> pprPslAssertion hdl False e1

    (CvBefore _ _) -> "{" <> afters1 <> "}"
     where
      afters0 = map (pprPslAssertion hdl False) (squashBefore e)
      afters1 = foldl1 (\e1 e2 -> e1 <> "; " <> e2) afters0

    (CvTemporalImplies 0 e1 e2) -> pslBinOp1 TImpliesOverlapping e1 e2
    (CvTemporalImplies 1 e1 e2) -> pslBinOp1 TImplies e1 e2
    (CvTemporalImplies n e1 e2) -> pslBinOp1 TImplies e1 (CvNext n e2)

    (CvAlways e1) -> "always " <> pprPslAssertion hdl True e1
    (CvNever e1) -> "never " <> pprPslAssertion hdl True e1
    (CvEventually e1) -> "eventually! " <> pprPslAssertion hdl True e1
 where
  pslBinOp1 = pslBinOp hdl True


---------------------------------------
--                SVA                --
---------------------------------------
svaEdge :: ActiveEdge -> Text -> Text
svaEdge Rising clkId = "posedge " <> clkId
svaEdge Falling clkId = "negedge " <> clkId

svaBinOp
  :: Bool
  -> Symbol
  -> Assertion' Text
  -> Assertion' Text
  -> Text
svaBinOp parens op e1 e2 =
  parensIf parens (e1' <> symbol SystemVerilog op <> e2')
 where
  e1' = pprSvaAssertion True e1
  e2' = pprSvaAssertion True e2

pprSvaAssertion :: Bool -> Assertion' Text -> Text
pprSvaAssertion parens e =
  case e of
    (CvPure p) -> p
    (CvLit False) -> "false"
    (CvLit True) -> "true"

    (CvNot e1) ->
      parensIf parens (symbol' Not <> pprSvaAssertion True e1)
    (CvAnd e1 e2) -> svaBinOp1 And e1 e2
    (CvOr e1 e2) -> svaBinOp1 Or e1 e2
    (CvImplies e1 e2) -> svaBinOp1 Implies e1 e2

    (CvToTemporal e1) -> "{" <> pprSvaAssertion False e1 <> "}"

    (CvNext 0 e1) -> pprSvaAssertion parens e1
    (CvNext n e1) -> "nexttime[" <> Text.pack (show n) <> "] " <> pprSvaAssertion False e1

    (CvBefore _ _) -> "{" <> afters1 <> "}"
     where
      afters0 = map (pprSvaAssertion False) (squashBefore e)
      afters1 = foldl1 (\e1 e2 -> "(" <> e1 <> ") ##1 (" <> e2 <> ")") afters0

    (CvTemporalImplies 0 e1 e2) -> svaBinOp1 TImpliesOverlapping e1 e2
    (CvTemporalImplies 1 e1 e2) -> svaBinOp1 TImplies e1 e2
    (CvTemporalImplies n e1 e2) -> svaBinOp1 TImplies e1 (CvNext n e2)

    (CvAlways e1) -> "always (" <> pprSvaAssertion False e1 <> ")"
    (CvNever _e) -> error "'never' not supported in SVA"
    (CvEventually e1) -> "s_eventually (" <> pprSvaAssertion False e1 <> ")"
 where
  svaBinOp1 = svaBinOp parens
  symbol' = symbol SystemVerilog

pprSvaProperty
  :: Text
  -- ^ Property name
  -> Text
  -- ^ Clock name
  -> ActiveEdge
  -- ^ Edge property should be sensitive to
  -> Property' Text
  -- ^ Assertion / Cover statement
  -> Declaration
pprSvaProperty propName clkId edge assertion = TickDecl . Comment $
  propName <> ": " <> coverOrAssert <> " property (@(" <>
  svaEdge edge clkId <> ") " <> prop <> ");"
 where
  (coverOrAssert, prop) =
    case assertion of
      CvCover e -> ("cover", pprSvaAssertion False e)
      CvAssert e -> ("assert", pprSvaAssertion False e)
      CvAssume e -> ("assume", pprSvaAssertion False e)

---------------------------------------
--     Yosys Formal Extensions       --
---------------------------------------

-- | Generate something like:
-- @always @(posedge clk_i) isOn: cover (result);@
pprYosysSvaProperty
  :: Text
  -- ^ Property name
  -> Expr
  -- ^ Clock expression
  -> ActiveEdge
  -- ^ Edge property should be sensitive to
  -> Property' Text
  -- ^ Assertion / Cover statement
  -> Declaration
pprYosysSvaProperty propName clk edge assertion = ConditionalDecl
  "FORMAL"
  [Seq [Always (ClockEdge clk edge) [] [SeqDecl (TickDecl directive)]]]
 where
  directive = Directive
    (propName <> ": " <> coverOrAssert <> " property (" <> prop <> ")")

  (coverOrAssert, prop) = case assertion of
    CvCover  e -> ("cover", pprSvaAssertion False e)
    CvAssert e -> ("assert", pprSvaAssertion False e)
    CvAssume e -> ("assume", pprSvaAssertion False e)
