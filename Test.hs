
import Clash.Explicit.Prelude
import Clash.Annotations.TH
import Clash.Cores.Xilinx.Xpm.Cdc.AsyncRst

createDomain vSystem{vName="HAD", vResetPolarity=ActiveHigh, vResetKind=Asynchronous, vInitBehavior=Defined}
createDomain vSystem{vName="HAU", vResetPolarity=ActiveHigh, vResetKind=Asynchronous, vInitBehavior=Unknown}
createDomain vSystem{vName="HSD", vResetPolarity=ActiveHigh, vResetKind=Synchronous,  vInitBehavior=Defined}
createDomain vSystem{vName="HSU", vResetPolarity=ActiveHigh, vResetKind=Synchronous,  vInitBehavior=Unknown}
createDomain vSystem{vName="LAD", vResetPolarity=ActiveLow,  vResetKind=Asynchronous, vInitBehavior=Defined}
createDomain vSystem{vName="LAU", vResetPolarity=ActiveLow,  vResetKind=Asynchronous, vInitBehavior=Unknown}
createDomain vSystem{vName="LSD", vResetPolarity=ActiveLow,  vResetKind=Synchronous,  vInitBehavior=Defined}
createDomain vSystem{vName="LSU", vResetPolarity=ActiveLow,  vResetKind=Synchronous,  vInitBehavior=Unknown}

top0 ::
  "src_clk" ::: Clock HAD ->
  "dst_clk" ::: Clock LAD ->
  "src_rst" ::: Reset HAD ->
  "dst_rst" ::: Reset LAD
top0 = xpmCdcAsyncRst
makeTopEntity 'top0
