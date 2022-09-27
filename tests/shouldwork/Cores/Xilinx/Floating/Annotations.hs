{-|
Copyright  :  (C) 2021,      QBayLogic B.V.,
                  2022,      Google Inc.,
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

module Floating.Annotations where

import Clash.Prelude

binaryTopAnn :: String -> TopEntity
binaryTopAnn name =
  Synthesize
    { t_name   = name
    , t_inputs =
          [ PortName "clk"
          , PortName "x"
          , PortName "y"
          ]
    , t_output = PortName "result"
    }

binaryEnTopAnn :: String -> TopEntity
binaryEnTopAnn name =
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

unaryTopAnn :: String -> TopEntity
unaryTopAnn name =
  Synthesize
    { t_name   = name
    , t_inputs =
          [ PortName "clk"
          , PortName "x"
          ]
    , t_output = PortName "result"
    }

unaryEnTopAnn :: String -> TopEntity
unaryEnTopAnn name =
  Synthesize
    { t_name   = name
    , t_inputs =
          [ PortName "clk"
          , PortName "en"
          , PortName "x"
          ]
    , t_output = PortName "result"
    }
