{-# LANGUAGE QuasiQuotes #-}

module Clash.Pretty where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Data.Maybe (fromMaybe)
import qualified System.Console.Terminal.Size as Terminal
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import qualified Clash.Util.Interpolate as I
import GHC.Stack (HasCallStack)

unsafeLookupEnvWord :: HasCallStack => String -> Word -> Word
unsafeLookupEnvWord key dflt =
  case unsafePerformIO (lookupEnv key) of
    Nothing -> dflt
    Just w -> flip fromMaybe (readMaybe w) $ error [I.i|
      'unsafeLookupEnvWord' tried to lookup #{key} in the environment. It found
      it, but couldn't interpret it to as a Word (positive Int). Found:

        #{w}
    |]

defaultPprWidth :: Int
defaultPprWidth =
  let dflt = max 80 (maybe 80 Terminal.width (unsafePerformIO Terminal.size)) in
  fromIntegral (unsafeLookupEnvWord "CLASH_PPR_WIDTH" dflt)

showDoc :: Doc ann -> String
showDoc =
  let layoutOpts = LayoutOptions (AvailablePerLine defaultPprWidth 0.6) in
  renderString . layoutPretty layoutOpts

removeAnnotations :: Doc ann -> Doc ()
removeAnnotations = reAnnotate $ const ()

-- | A variant of @Pretty@ that is not polymorphic on the type of annotations.
-- This is needed to derive instances from Clash's pretty printer (PrettyPrec),
-- which annotates documents with Clash-specific information and, therefore,
-- fixes the type of annotations.
class ClashPretty a where
  clashPretty :: a -> Doc ()

fromPretty :: Pretty a => a -> Doc ()
fromPretty = removeAnnotations . pretty
