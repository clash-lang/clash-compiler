{-|
  Copyright  :  (C) 2019     , Myrtle Software Ltd
                    2021     , Ellie Hermaszewska
                    2020-2025, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Primitives.Verification (checkBBF, bindMaybe) where

import Data.Either


import           Control.Monad.State             (State)
import           Data.List.Infinite              (Infinite(..), (...))
import           Data.Maybe                      (listToMaybe)
import           Data.Monoid                     (Ap(getAp))
import           Data.Text.Prettyprint.Doc.Extra (Doc)
import           Data.Text                       (Text)
import           GHC.Stack                       (HasCallStack)

import           Clash.Annotations.Primitive     (HDL(..))
import           Clash.Backend
  (Backend, blockDecl, hdlKind)
import           Clash.Core.HasType       (inferCoreTypeOf)
import           Clash.Core.Syntax        (Distinct, Term(Var))
import           Clash.Core.TermLiteral
  (liftTermToData#, termToDataError, termToDataErrorWith)
import           Clash.Util                      (indexNote)
import           Clash.Netlist.Scoped            (mkExpr)
import           Clash.Netlist.Types
  (BlackBoxEnv, BlackBoxFunction, NetlistId(..), bbLocalIdentifier, bbScope,
   bbTypeEnv, BlackBox(BBFunction), TemplateFunction(..), BlackBoxContext, Identifier,
   NetlistMonad, Declaration(NetDecl),
   HWType(Bool, KnownDomain),
   DeclarationType(Concurrent), bbInputs, Expr(Identifier))
import           Clash.Netlist.Util              (stripVoid, contAssign)
import qualified Clash.Netlist.Id                as Id
import           Clash.Netlist.BlackBox.Types
  (BlackBoxMeta(..), TemplateKind(TDecl), RenderVoid(..), emptyBlackBoxMeta)

import           Clash.Verification.Internal
import           Clash.Verification.Pretty

checkBBF :: BlackBoxFunction
checkBBF env _isD _primName args _ty =
  case litArgs of
    Left err -> pure (Left err)
    Right (propName, renderAs, cvProperty0) -> do
      clkBind <- bindMaybe env (Just "clk") (indexNote "clk" (lefts args) clkArg)
      cvProperty1 <- mapM (uncurry (bindMaybe env)) cvProperty0
      let decls = concatMap snd cvProperty1
          cvProperty2 = fmap fst cvProperty1
      pure (Right (meta, bb (checkTF decls clkBind propName renderAs cvProperty2)))
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

  litArgs = do
    propName <- termToDataError (indexNote "propName" (lefts args) propNameArg)
    renderAs <- termToDataError (indexNote "renderAs" (lefts args) renderAsArg)
    -- The leaves of the property are terms, bound by 'bindMaybe'
    cvProperty <-
      termToDataErrorWith "Property' (Maybe Text, Term)"
        (liftTermToData# (liftTermToData# Right))
        (indexNote "propArg" (lefts args) propArg)
    Right (propName, renderAs, cvProperty)

  bb = BBFunction "Clash.Primitives.Verification.checkTF" 0
  meta = emptyBlackBoxMeta {bbKind=TDecl, bbRenderVoid=RenderVoid}

-- | Bind a term to a signal, unless it is a reference to a signal already
bindMaybe
  :: (HasCallStack, Distinct n)
  => BlackBoxEnv n
  -- ^ The context of the term
  -> Maybe Text
  -- ^ Hint for new identifier
  -> Term n
  -- ^ Term to bind. Does not bind if it's already a reference to a signal
  -> NetlistMonad (Identifier, [Declaration])
  -- ^ ([new] reference to signal, [declarations need to get it in scope])
bindMaybe env _ (Var _ _ nm) = pure (bbLocalIdentifier env nm, [])
bindMaybe env Nothing t = bindMaybe env (Just "s") t
bindMaybe env (Just nm) t = do
  newId <- Id.make nm
  let ty = inferCoreTypeOf (bbTypeEnv env) (bbScope env) t
  (expr0, decls) <- mkExpr env False Concurrent (NetlistId newId ty) t
  assn <- contAssign newId expr0
  pure
    ( newId
    , decls ++ [sigDecl Bool newId, assn] )
 where
  -- Simple wire without comment
  sigDecl :: HWType -> Identifier -> Declaration
  sigDecl typ nm1 = NetDecl Nothing nm1 typ

checkTF
  :: [Declaration]
  -> (Identifier, [Declaration])
  -> Text
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
  -> (Identifier, [Declaration])
  -- ^ Clock
  -> Text
  -- ^ Prop name
  -> RenderAs
  -> Property' Identifier
  -> BlackBoxContext
  -> State s Doc
checkTF' decls (clkId, clkDecls) propName renderAs prop bbCtx = do
  blockName <- Id.makeBasic (propName <> "_block")
  getAp (blockDecl blockName (clkDecls <> (renderedPslProperty : decls)))

 where
  hdl = hdlKind (undefined :: s)
  clk = Identifier clkId Nothing

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
