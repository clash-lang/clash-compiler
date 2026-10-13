{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities of the rewrite monad: running a rewrite session, signaling
  change, counting transformations, and the caches of the rewrite state. The
  utilities on terms are in "Clash.Rewrite.Scoped.Util".
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Rewrite.Util
  ( zoomExtra
  , bumpCounter
  , runRewriteSession
  , setChanged
  , changed
  , isFromInt
  , runWithHWTypeCache
  ) where

import           Control.Lens                ((^.), Lens')
import qualified Control.Lens                as Lens
import qualified Control.Monad.State.Strict  as State
import qualified Control.Monad.Writer        as Writer
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.List                   (sortOn)
import qualified Data.Monoid                 as Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Clash.Debug                 (traceIf)
import           Clash.Driver.Types
  (ClashEnv (..), ClashOpts (..), DebugOpts (..), TransformationInfo (..))
import           Clash.Netlist.Types         (HWMap)
import           Clash.Rewrite.Types

-- | Lift an action working in the '_extra' state to the 'RewriteMonad'
zoomExtra :: State.State extra a -> RewriteMonad extra a
zoomExtra m = R $ \_ s w ->
  let (a, st') = State.runState m (_extra s)
   in pure (a, s { _extra = st' }, w)

-- | Strictly bump a counter in 'RewriteState', typically 'transformTriedCounters'
-- or 'transformAppliedCounters'.
bumpCounter
  :: String
  -> Lens' (RewriteState extra) (HashMap Text Word)
  -> RewriteMonad extra ()
bumpCounter name l = do
  counters <- Lens.use l
  -- Note: Using $! to force thunks to prevent a gigantic pile of +1s in memory
  Lens.assign l $! HashMap.insertWith (+) (Text.pack name) 1 counters

-- | Evaluate a RewriteSession to its inner monad.
runRewriteSession :: RewriteEnv
                  -> RewriteState extra
                  -> RewriteMonad extra a
                  -> IO a
runRewriteSession r s m = do
  (a, s', _) <- runR m r s

  let
    triedTransformationsMessage =
      ("Clash: Tried transformations:\n" ++ Text.unpack (showCounters (s' ^. transformTriedCounters)))
    appliedTransformationsMessage =
      ("Clash: Applied transformations:\n" ++ Text.unpack (showCounters (s' ^. transformAppliedCounters)))

  traceIf (dbg_countTransformations (opt_debug (envOpts (_clashEnv r))))
    (triedTransformationsMessage ++ "\n" ++ appliedTransformationsMessage) $
    traceIf (None < dbg_transformationInfo (opt_debug (envOpts (_clashEnv r))))
      ("Clash: Applied " ++ show (s' ^. transformCounter) ++ " transformations")
      pure a
  where
    showCounters =
      Text.unlines
        . map (\(nm,cnt) -> nm <> ": " <> Text.pack (show cnt))
        . sortOn snd
        . HashMap.toList

-- | Notify that a transformation has changed the expression
setChanged :: RewriteMonad extra ()
setChanged = Writer.tell (Monoid.Any True)

-- | Identity function that additionally notifies that a transformation has
-- changed the expression
changed :: a -> RewriteMonad extra a
changed val = do
  Writer.tell (Monoid.Any True)
  return val

-- | Whether a primitive is one of the @fromInteger@ conversions of the sized
-- number types
isFromInt :: Text -> Bool
isFromInt nm = nm == "Clash.Sized.Internal.BitVector.fromInteger##" ||
               nm == "Clash.Sized.Internal.BitVector.fromInteger#" ||
               nm == "Clash.Sized.Internal.Index.fromInteger#" ||
               nm == "Clash.Sized.Internal.Signed.fromInteger#" ||
               nm == "Clash.Sized.Internal.Unsigned.fromInteger#"

-- | Run a Core-type to HWType translation action against the session-wide
-- translation cache ('hwTypeCache').
runWithHWTypeCache :: State.State HWMap a -> RewriteMonad extra a
runWithHWTypeCache m = do
  cache0 <- Lens.use hwTypeCache
  let (a, !cache1) = State.runState m cache0
  hwTypeCache Lens..= cache1
  pure a
