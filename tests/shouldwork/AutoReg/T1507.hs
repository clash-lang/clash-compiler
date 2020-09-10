module T1507 where

import Clash.Prelude

topEntity ::
  SystemClockResetEnable =>
  Signal System (Maybe Int) ->
  Signal System (Maybe Int)
topEntity = autoReg  (errorX "foo bar")
