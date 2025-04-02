{-# LANGUAGE OverloadedStrings #-}

module Clash.Primitives.Verification (checkBBF) where

import Data.Either


import qualified Control.Lens                    as Lens
import           Control.Monad.State             (State, MonadState (get))
import           Data.List.Infinite              (Infinite(..), (...))
import           Data.Maybe                      (listToMaybe)
import           Data.Monoid                     (Ap(getAp))
import           Data.Text.Prettyprint.Doc.Extra (Doc)
import qualified Data.Text                       as Text
import           GHC.Stack                       (HasCallStack)

import           Clash.Annotations.Primitive     (HDL(..))
import           Clash.Backend
  (Backend, blockDecl, hdlKind)
import           Clash.Core.HasType
import           Clash.Core.Term                 (Term(Var), varToId)
import           Clash.Core.TermLiteral          (termToDataError)
import           Clash.Util                      (indexNote)
import           Clash.Netlist                   (mkExpr)
import           Clash.Netlist.Util              (stripVoid)
import qualified Clash.Netlist.Id                as Id
import           Clash.Netlist.Types
  (BlackBox(BBFunction), TemplateFunction(..), BlackBoxContext, Identifier,
   NetlistMonad, Declaration(Assignment, NetDecl), Usage(Cont),
   HWType(Bool, KnownDomain), NetlistId(..),
   DeclarationType(Concurrent), tcCache, bbInputs, Expr(Identifier), bindings)
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(TDecl), RenderVoid(..),
   emptyBlackBoxMeta)

import           Clash.Verification.Internal
import           Clash.Verification.Pretty
import Control.Lens ((^.))

checkBBF :: BlackBoxFunction
checkBBF _isD _primName args _ty = do
  bindingMap <- (^. bindings) <$> get
  case litArgs bindingMap of
    Left err -> pure (Left err)
    Right (propName, renderAs, cvProperty0) -> do
      cvProperty1 <- mapM (uncurry bindMaybe) cvProperty0
      let decls = concatMap snd cvProperty1
          cvProperty2 = fmap fst cvProperty1
      pure (Right (meta, bb (checkTF decls (clkExpr, clkId) propName renderAs cvProperty2)))
 where
  -- TODO: Improve error handling; currently errors don't indicate what
  -- TODO: blackbox generated them.
  _knownDomainArg
    :< clkArg
    :< _rstArg
    :< propNameArg
    :< renderAsArg
    :< propArg
    :< _ = ((0 :: Int)...)
  (Id.unsafeFromCoreId -> clkId) = varToId (indexNote "clk" (lefts args) clkArg)
  clkExpr = Identifier clkId Nothing

  litArgs bindingMap = do
    propName <- termToDataError bindingMap (indexNote "propName" (lefts args) propNameArg)
    renderAs <- termToDataError bindingMap (indexNote "renderAs" (lefts args) renderAsArg)
    cvProperty <- termToDataError bindingMap (indexNote "propArg" (lefts args) propArg)
    Right (propName, renderAs, cvProperty)

  bb = BBFunction "Clash.Primitives.Verification.checkTF" 0
  meta = emptyBlackBoxMeta {bbKind=TDecl, bbRenderVoid=RenderVoid}

  bindMaybe
    :: Maybe String
    -- ^ Hint for new identifier
    -> Term
    -- ^ Term to bind. Does not bind if it's already a reference to a signal
    -> NetlistMonad (Identifier, [Declaration])
    -- ^ ([new] reference to signal, [declarations need to get it in scope])
  bindMaybe _ (Var vId) = pure (Id.unsafeFromCoreId vId, [])
  bindMaybe Nothing t = bindMaybe (Just "s") t
  bindMaybe (Just nm) t = do
    tcm <- Lens.view tcCache
    newId <- Id.make (Text.pack nm)
    (expr0, decls) <- mkExpr False Concurrent (NetlistId newId (inferCoreTypeOf tcm t)) t
    pure
      ( newId
      , decls ++ [sigDecl Bool newId, Assignment newId Cont expr0] )

  -- Simple wire without comment
  sigDecl :: HWType -> Identifier -> Declaration
  sigDecl typ nm = NetDecl Nothing nm typ

checkTF
  :: [Declaration]
  -> (Expr, Identifier)
  -> Text.Text
  -> RenderAs
  -> Property' Identifier
  -> TemplateFunction
checkTF decls clk propName renderAs prop =
  TemplateFunction [] (const True) (checkTF' decls clk propName renderAs prop)

checkTF'
  :: forall s
   . (HasCallStack, Backend s)
  => [Declaration]
  -- ^ Extra decls needed
  -> (Expr, Identifier)
  -- ^ Clock
  -> Text.Text
  -- ^ Prop name
  -> RenderAs
  -> Property' Identifier
  -> BlackBoxContext
  -> State s Doc
checkTF' decls (clk, clkId) propName renderAs prop bbCtx = do
  blockName <- Id.makeBasic (propName <> "_block")
  getAp (blockDecl blockName (renderedPslProperty : decls))

 where
  hdl = hdlKind (undefined :: s)

  edge =
    case bbInputs bbCtx of
      (_, stripVoid -> KnownDomain _nm _period e _rst _init _polarity, _):_ -> e
      _ -> error $ "Unexpected first argument: " ++ show (listToMaybe (bbInputs bbCtx))

  renderedPslProperty = case renderAs of
    PSL          -> psl
    SVA          -> sva
    AutoRenderAs -> case hdl of
      SystemVerilog -> sva
      _             -> psl
    YosysFormal -> case hdl of
      VHDL -> psl
      _    -> ysva

   where
    sva = pprSvaProperty propName (Id.toText clkId) edge (fmap Id.toText prop)
    ysva = pprYosysSvaProperty propName clk edge (fmap Id.toText prop)
    psl = pprPslProperty hdl propName (Id.toText clkId) edge (fmap Id.toText prop)
