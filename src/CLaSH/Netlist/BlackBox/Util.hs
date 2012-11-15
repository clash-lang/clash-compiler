module CLaSH.Netlist.BlackBox.Util where

import Control.Monad.State
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.HashMap.Lazy     as HashMap
import qualified Data.Label.PureM      as LabelM
import qualified Data.Text.Lazy        as Text
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Text.Hastache         as Hastache
import qualified Text.Hastache.Context as Hastache

import CLaSH.Netlist.BlackBox.Types as BB
import CLaSH.Primitives.Types as P

renderBlackBox ::
  Primitive
  -> BlackBoxContext
  -> BlackBoxMonad BSL.ByteString
renderBlackBox p bbCtx = do
  let hastacheCtx = Hastache.mkGenericContext bbCtx
  tmpl <- Hastache.hastacheStr (Hastache.defaultConfig)
            (template p) hastacheCtx
  return tmpl

addContext ::
  BS.ByteString
  -> BlackBoxMonad BS.ByteString
addContext f = do
  tLvlCtx <- fmap Hastache.mkGenericContext $ LabelM.gets topLevelCtx
  res <- Hastache.hastacheStr Hastache.defaultConfig f tLvlCtx
  bbM <- fmap (HashMap.lookup res) $ LabelM.gets prims
  case bbM of
    Just p@(P.BlackBox {}) -> do
      let newC = Context (Text.pack "BB_OUT") [] [] [] addContext addInput addOutput renderFun
      LabelM.puts renderContext (Just (p,newC))
      return BS.empty
    _ -> error $ "No blackbox found: " ++ show bbM

addInput ::
  BS.ByteString
  -> BlackBoxMonad BS.ByteString
addInput i = do
  tLvlCtx <- fmap Hastache.mkGenericContext $ LabelM.gets topLevelCtx
  res <- Hastache.hastacheStr Hastache.defaultConfig i tLvlCtx
  LabelM.modify renderContext
    (\x -> case x of
      Just (p,c) -> let c' = c {BB.inputs = BB.inputs c ++ [decodeUtf8 res] }
                    in Just (p,c')
      Nothing    -> Nothing
    )
  return BS.empty

addOutput ::
  BS.ByteString
  -> BlackBoxMonad BS.ByteString
addOutput o = do
  tLvlCtx <- fmap Hastache.mkGenericContext $ LabelM.gets topLevelCtx
  res <- Hastache.hastacheStr Hastache.defaultConfig o tLvlCtx
  LabelM.modify renderContext
    (\x -> case x of
      Just (p,c) -> let c' = c {BB.result = decodeUtf8 res }
                    in Just (p,c')
      Nothing    -> Nothing
    )
  return BS.empty

renderFun ::
  BS.ByteString
  -> BlackBoxMonad BS.ByteString
renderFun f = do
  rctx <- LabelM.gets renderContext
  case rctx of
    Just (p,ctx) -> do
      let ctx' = Hastache.mkGenericContext ctx
      res <- Hastache.hastacheStr Hastache.defaultConfig (template p) ctx'
      return (BSL.toStrict res)
    Nothing -> error "No render context set"
