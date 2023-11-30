module XpmTestCommon where

import Clash.Explicit.Prelude

createDomain vXilinxSystem{vName="D3",  vPeriod=hzToPeriod 30e6}
createDomain vXilinxSystem{vName="D5",  vPeriod=hzToPeriod 50e6}
createDomain vXilinxSystem{vName="D10", vPeriod=hzToPeriod 100e6}
createDomain vXilinxSystem{vName="D11", vPeriod=hzToPeriod 110e6}
