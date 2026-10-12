{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Compare the netlists that Netlist generation on Clash Core
  ('Clash.Netlist.genNetlist') and on scoped Clash Core
  ('Clash.Netlist.Scoped.genNetlist') generate for the normalized bindings of
  a design (@\<design\>.net.bin@, see @clash-profile-netlist-prepare@), for
  every HDL. The component maps are compared structurally, through their
  'Show' instances. Both run in one process, on the same bindings.

  TODO(Phase 7): remove, together with Netlist generation on Clash Core.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module NetlistDiff
  ( compareNetlists
  ) where

import           Control.Exception            (SomeException, displayException, evaluate, try)
import           Control.Monad                (unless)
import           Data.Binary                  (decode)
import qualified Data.ByteString.Lazy         as B
import qualified Data.Map.Ordered             as OMap
import           Data.Maybe                   (fromMaybe)
import           Data.Proxy                   (Proxy (..))
import qualified Data.Text                    as Text
import           System.FilePath              ((</>))
import           System.IO                    (hFlush, stdout)
import           System.IO.Unsafe             (unsafePerformIO)

import           Clash.Annotations.Primitive  (HDL (..))
import           Clash.Backend
import           Clash.Backend.SystemVerilog  (SystemVerilogState)
import           Clash.Backend.Verilog        (VerilogState)
import           Clash.Backend.VHDL           (VHDLState)
import           Clash.Core.Name              (nameOcc)
import           Clash.Core.Var               (varName)
import           Clash.Driver.Types
  (ClashEnv (..), ClashOpts (..), defClashOpts)
-- For the orphan 'Binary' instance of 'TopEntityT'
import           Clash.Driver.Manifest        ()
import           Clash.GHC.Evaluator          (evaluator)
import           Clash.GHC.NetlistTypes       (ghcTypeToHWType)
import qualified Clash.Netlist                as Legacy
import qualified Clash.Netlist.Scoped         as Scoped
import           Clash.Netlist.Types
  (Component (..), ComponentMap, SomeBackend (..))
import           Clash.Rewrite.Scoped.Globals (fromClashBindings)

import           RestorePrimitives            (restorePrimMap)
import           SerialiseInstances           (NetlistInputs)

-- | Compare the netlists of a design for every HDL. Gives whether they are
-- equal.
compareNetlists
  :: Int
  -- ^ How many characters of context to print around a difference
  -> FilePath
  -- ^ The normalized bindings of the design (@\<design\>.net.bin@)
  -> IO Bool
compareNetlists ctxLen design = do
  (bm, topEntities, primMap, tcm, reprs, topEntity, domains) <-
    decode <$> B.readFile design :: IO NetlistInputs
  (prims, unrestored) <- restorePrimMap primMap
  let env = ClashEnv
        { envOpts = defClashOpts {opt_errorExtra = True}
        , envTyConMap = tcm
        , envTupleTyCons = mempty
        , envPrimitives = prims
        , envCustomReprs = reprs
        , envDomains = domains
        }
      globals = fromClashBindings bm
      topEntityMap = Legacy.topEntityEnv topEntities
      topEntityS = Text.unpack (nameOcc (varName topEntity))
      modName = takeWhile (/= '.') topEntityS

      run
        :: forall s. Backend s => Proxy s -> HDL -> IO Bool
      run Proxy hdl = do
        let be = setModName (Text.pack modName) (initBackend @s (envOpts env))
            (compNames, seen) = Legacy.genTopNames (envOpts env) hdl topEntities
            ite = ifThenElseExpr be
            hdlDir = fromMaybe "." (opt_hdlDir (envOpts env)) </>
                       Clash.Backend.name be </> modName
            gen f = f env evaluator False globals topEntityMap compNames
                      (ghcTypeToHWType (opt_intWidth (envOpts env)))
                      ite (SomeBackend be) seen hdlDir Nothing topEntity
        (_, cmA, seenA) <- gen Legacy.genNetlist
        (_, cmB, seenB) <- gen Scoped.genNetlist
        let diffs = compareComponentMaps cmA cmB
            seenDiff = show seenA /= show seenB
        mapM_ (putStr . renderDiff ctxLen) (take 3 diffs)
        putStrLn $ design <> " (" <> show hdl <> "): "
          <> show (length (OMap.assocs cmA)) <> " and "
          <> show (length (OMap.assocs cmB)) <> " components, "
          <> show (length diffs) <> " different"
          <> (if seenDiff then ", different identifier sets" else "")
        hFlush stdout
        pure (null diffs && not seenDiff)

  unless (null unrestored) $
    putStrLn ("Primitives whose blackbox functions can't be restored: "
      <> Text.unpack (Text.intercalate (Text.pack ", ") unrestored))
  oks <- sequence
    [ run (Proxy @VHDLState) VHDL
    , run (Proxy @VerilogState) Verilog
    , run (Proxy @SystemVerilogState) SystemVerilog ]
  pure (and oks)

-- | The components that differ: by key, the first of their parts that
-- differ (the meta data, name, ports, or a declaration), on both sides
compareComponentMaps :: ComponentMap -> ComponentMap -> [(String, String, String)]
compareComponentMaps cmA cmB = align (OMap.assocs cmA) (OMap.assocs cmB)
 where
  align ((ka, va) : as) ((kb, vb) : bs)
    | ka == kb = case firstDiff (parts va) (parts vb) of
        Just (a, b) -> (show ka, a, b) : align as bs
        Nothing -> align as bs
    | otherwise = (show ka, "component " <> show ka, "component " <> show kb) : align as bs
  align as bs =
    [ (show k, "component", "<missing>") | (k, _) <- as ] <>
    [ (show k, "<missing>", "component") | (k, _) <- bs ]

  parts (meta, Component nm inps outps decls) =
    [ "meta: " <> safeShow meta
    , "name: " <> safeShow nm
    , "inputs: " <> safeShow inps
    , "outputs: " <> safeShow outps ] <>
    [ "declaration " <> show i <> ": " <> safeShow d | (i, d) <- zip [(0 :: Int) ..] decls ] <>
    [ "end" ]

  firstDiff (a : as) (b : bs)
    | a == b = firstDiff as bs
    | otherwise = Just (a, b)
  firstDiff [] [] = Nothing
  firstDiff as bs = Just (unwords (take 1 as), unwords (take 1 bs))

-- | Render a value, up to the first exception. Netlists contain thunks that
-- error when forced, e.g. the expressions of function arguments of blackboxes
-- (see 'Clash.Netlist.BlackBox.mkArgument'), that are never rendered.
safeShow :: Show a => a -> String
safeShow = go . show
 where
  go s = unsafePerformIO $ do
    r <- try (evaluate (forceHead s))
    pure $ case r of
      Left (e :: SomeException) ->
        "<exception: " <> takeWhile (/= '\n') (displayException e) <> ">"
      Right [] -> []
      Right (c : cs) -> c : go cs
  forceHead xs = case xs of
    c : _ -> c `seq` xs
    [] -> xs

-- | A difference between two renderings: the context around the first
-- character at which they differ
renderDiff :: Int -> (String, String, String) -> String
renderDiff n (k, a, b) =
  unlines
    [ "--- component " <> k <> ", first difference at character " <> show i <> " of:"
    , "  Clash Core:  ..." <> excerpt a
    , "  scoped Core: ..." <> excerpt b ]
 where
  i = length (takeWhile id (zipWith (==) a b))
  excerpt = take (2 * n) . drop (max 0 (i - n))
