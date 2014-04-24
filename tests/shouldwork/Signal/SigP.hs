import CLaSH.Prelude

topEntity :: Signal (Bool, Bool) -> (Signal Bool, Signal Bool)
topEntity = unpack . register (False,False)
