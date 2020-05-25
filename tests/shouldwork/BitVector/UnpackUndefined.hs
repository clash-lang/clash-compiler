module UnpackUndefined where
import Clash.Prelude
import Clash.Sized.Internal.BitVector

-- https://github.com/clash-lang/clash-compiler/issues/804
state = unpack undefined#    :: Vec 3 Bit
topEntity a = register @System state a
