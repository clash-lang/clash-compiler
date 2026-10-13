{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The rewrite history of @-fclash-debug-history@: a file of 'RewriteStep's,
  which @clash-term@ shows. It starts with the header of the files of Clash
  Core ('Clash.Core.Binary.putHeader').

  A transformation rewrites a subterm, which may mention variables bound
  around it. A step stores closed terms: the term before and the term after
  the transformation are abstracted over the free variables of both, the type
  variables first (see 'closeRewriteStep').
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Clash.Rewrite.History
  ( startRewriteHistory
  , recordRewriteStep
  , closeRewriteStep
  , decodeRewriteHistory
  ) where

import           Control.Concurrent.MVar     (MVar, newMVar, withMVar)
import           Control.Exception           (evaluate)
import qualified Control.Lens                as Lens
import           Control.Monad               (forM_)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Binary                 (encode, get)
import           Data.Binary.Get             (runGetOrFail)
import           Data.Binary.Put             (runPut)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           GHC.Stack                   (HasCallStack)
import           System.IO.Unsafe            (unsafePerformIO)

import           Clash.Driver.Types          (DebugOpts (..))
import           Clash.Rewrite.Types         (RewriteMonad, RewriteStep (..), debugOpts)

import           Clash.Core.Binary    (getHeader, putHeader)
import           Clash.Core.Subst     (Closure (..), closeOver, freeVarOccs)
import           Clash.Core.Syntax
import           Clash.Core.Term      (mkLams, mkTyLams)
import           Clash.Rewrite.Util   (dependencySortTyVars)

-- | Start a rewrite history: a file with just the header
startRewriteHistory :: FilePath -> IO ()
startRewriteHistory file = BL.writeFile file (runPut putHeader)

-- | The steps of a rewrite history. Errors if it doesn't start with the
-- header, or if a step can't be decoded.
decodeRewriteHistory :: HasCallStack => BL.ByteString -> [RewriteStep]
decodeRewriteHistory bs0 = case runGetOrFail getHeader bs0 of
  Left (_, _, err) -> error ("decodeRewriteHistory: " <> err)
  Right (bs1, _, ()) -> go bs1
 where
  go bs
    | BL.null bs = []
    | otherwise = case runGetOrFail get bs of
        Left (_, off, err) -> error ("decodeRewriteHistory: " <> err <> ", at byte " <> show off)
        Right (rest, _, st) -> st : go rest

-- | Append a step to the rewrite history, if @-fclash-debug-history@ is on
recordRewriteStep
  :: HasCallStack
  => String
  -- ^ Name of the transformation
  -> String
  -- ^ Name of the binding it rewrites
  -> Term n
  -- ^ The term before the transformation
  -> Term n
  -- ^ The term after it
  -> RewriteMonad extra ()
recordRewriteStep name bndr before after = do
  opts <- Lens.view debugOpts
  forM_ (dbg_historyFile opts) $ \file -> liftIO $ do
    -- Encode the step before opening the file, and append steps one at a
    -- time: top entities are normalized concurrently, and a file can't be
    -- opened for appending twice
    step <- evaluate (BL.toStrict (encode (closeRewriteStep name bndr before after)))
    withMVar historyLock $ \_ -> BS.appendFile file step

-- | Serializes the appends to the rewrite history, see 'recordRewriteStep'
historyLock :: MVar ()
historyLock = unsafePerformIO (newMVar ())
{-# NOINLINE historyLock #-}

-- | A step of the rewrite history, see the module header
closeRewriteStep
  :: forall n
   . HasCallStack
  => String
  -- ^ Name of the transformation
  -> String
  -- ^ Name of the binding it rewrites
  -> Term n
  -- ^ The term before the transformation
  -> Term n
  -- ^ The term after it
  -> RewriteStep
closeRewriteStep name bndr before after =
  case closeOver (dependencySortTyVars tvs) ids step of
    Just st -> st
    Nothing -> error "closeRewriteStep: can't abstract over the free variables"
 where
  -- The free variables of both terms, in the order of their first occurrence
  (tvs, ids) = freeVarOccs (App before after)

  step
    :: (DExt 'VoidS i, DExt i l)
    => Scope l -> Tele TyBinder 'VoidS i -> Tele IdBinder i l -> Closure n l -> RewriteStep
  step _ tvTele idTele cl =
    RewriteStep
      { t_name = name
      , t_bndrS = bndr
      , t_free = teleLength tvTele + teleLength idTele
      , t_before = close before
      , t_after = close after
      }
   where
    close e = case closureTerm cl e of
      Just e1 -> mkTyLams (mkLams e1 idTele) tvTele
      Nothing -> error "closeRewriteStep: a term mentions other variables"
