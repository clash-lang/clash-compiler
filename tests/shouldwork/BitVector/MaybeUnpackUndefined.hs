module MaybeUnpackUndefined where
import Clash.Prelude
import Clash.Sized.Internal.BitVector

state = maybeUnpack undefined#    :: Vec 3 Bit
topEntity a = register @System state a
