module Clash.Pretty where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

showDoc :: Doc ann -> String
showDoc = renderString . layoutPretty (LayoutOptions (AvailablePerLine 80 0.6))

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
