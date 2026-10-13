{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Compares the pretty-printer of scoped Clash Core ("Clash.Core.Scoped.Pretty")
  with printing the conversion to Clash Core, on the bindings of serialized
  designs: the terms, with the default options and with everything hidden,
  and the types of the binders. TODO(Phase 7): remove, with the conversion.
-}

module Pretty (comparePretty) where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as B
import Data.Binary (decode)
import qualified Data.List as List
import System.IO (hFlush, stdout)

import Clash.Core.Pretty (PrettyOptions (..), showPpr, showPpr')
import Clash.Core.Scoped.Convert (toClashTerm, toClashType)
import Clash.Core.Scoped.NameMap (globalEnvElems)
import Clash.Core.Scoped.Pretty ()
import Clash.Core.Scoped.Syntax (GlobalId (..))
import Clash.Driver.Manifest ()
import Clash.Driver.Types (Binding (..), BindingMap)
import Clash.Rewrite.Scoped.Globals (fromClashBindings)

import SerialiseInstances (NetlistInputs, NormalizationInputs)

-- | Compare the printing of every binding of a design (@*.bin@, @*.net.bin@,
-- or the normalized bindings @*.norm.bin@), showing the first given number of
-- differences. Gives whether they all print the same.
comparePretty :: Int -> FilePath -> IO Bool
comparePretty nShow design = do
  bm <- loadBindings design
  let bs = globalEnvElems (fromClashBindings bm)
      diffs =
        [ (showPpr (globalIdName (bindingId b)) <> ": " <> what, a, c)
        | b <- bs
        , (what, a, c) <- printings b
        , a /= c ]
      printings b =
        [ ("type", showPpr' def (globalIdType (bindingId b))
                 , showPpr' def (toClashType (globalIdType (bindingId b))))
        , ("term", showPpr' def (bindingTerm b), showPpr' def (toClashTerm (bindingTerm b)))
        , ("term, hiding everything", showPpr' hidden (bindingTerm b)
                                    , showPpr' hidden (toClashTerm (bindingTerm b)))
        ]
  forM_ (take nShow diffs) $ \(what, a, c) ->
    putStr (unlines (what : firstDifference (lines a) (lines c)))
  let n = length diffs
  putStrLn (design <> ": " <> show (length bs) <> " bindings, " <> show n <> " differences")
  hFlush stdout
  pure (n == 0)
 where
  def = PrettyOptions True True True True
  hidden = PrettyOptions False False False False

loadBindings :: FilePath -> IO BindingMap
loadBindings f
  | ".norm.bin" `List.isSuffixOf` f = decode <$> B.readFile f
  | ".net.bin" `List.isSuffixOf` f = do
      (bm, _, _, _, _, _, _) <- decode <$> B.readFile f :: IO NetlistInputs
      pure bm
  | otherwise = do
      (bm, _, _, _, _, _, _, _) <- decode <$> B.readFile f :: IO NormalizationInputs
      pure bm

-- | The first line in which two texts differ, with its line number
firstDifference :: [String] -> [String] -> [String]
firstDifference = go (1 :: Int)
 where
  go i (a : as) (c : cs)
    | a == c = go (i + 1) as cs
    | otherwise = ["  line " <> show i <> ", scoped:", "    " <> a, "  converted:", "    " <> c]
  go i _ _ = ["  line " <> show i <> ": one of the texts ends here"]
