-- source: https://github.com/clash-lang/clash-compiler/issues/236

module BundleMapRepeat where

import Clash.Prelude

topEntity :: Signal System (Unsigned 16) -> Signal System (Vec 17 (Unsigned 16))
topEntity x = bundle . map ($ x) $ repeat id
