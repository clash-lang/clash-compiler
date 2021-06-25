{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Floating.Annotations where

import Clash.Prelude

binTopAnn :: String -> TopEntity
binTopAnn name =
  Synthesize
    { t_name   = name
    , t_inputs =
          [ PortName "clk"
          , PortName "x"
          , PortName "y"
          ]
    , t_output = PortName "result"
    }

binEnTopAnn :: String -> TopEntity
binEnTopAnn name =
  Synthesize
    { t_name   = name
    , t_inputs =
          [ PortName "clk"
          , PortName "en"
          , PortName "x"
          , PortName "y"
          ]
    , t_output = PortName "result"
    }
