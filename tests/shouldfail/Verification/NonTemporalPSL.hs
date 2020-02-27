{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module NonTemporalPSL where

import Clash.Prelude
import Clash.Verification.Internal
import qualified NonTemporal as NT

fails1 = NT.fails1 PSL
{-# ANN fails1 (defSyn "fails1") #-}

fails2 = NT.fails2 PSL
{-# ANN fails2 (defSyn "fails2") #-}

fails3 = NT.fails3 PSL
{-# ANN fails3 (defSyn "fails3") #-}

fails4 = NT.fails4 PSL
{-# ANN fails4 (defSyn "fails4") #-}

fails5 = NT.fails5 PSL
{-# ANN fails5 (defSyn "fails5") #-}

fails6 = NT.fails6 PSL
{-# ANN fails6 (defSyn "fails6") #-}

fails7 = NT.fails7 PSL
{-# ANN fails7 (defSyn "fails7") #-}

fails8 = NT.fails8 PSL
{-# ANN fails8 (defSyn "fails8") #-}

fails9 = NT.fails9 PSL
{-# ANN fails9 (defSyn "fails9") #-}

fails10 = NT.fails10 PSL
{-# ANN fails10 (defSyn "fails10") #-}

fails11 = NT.fails11 PSL
{-# ANN fails11 (defSyn "fails11") #-}

fails12 = NT.fails12 PSL
{-# ANN fails12 (defSyn "fails12") #-}

fails13 = NT.fails13 PSL
{-# ANN fails13 (defSyn "fails13") #-}
