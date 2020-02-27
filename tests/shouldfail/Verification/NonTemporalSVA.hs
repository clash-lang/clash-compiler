{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module NonTemporalSVA where

import Clash.Prelude
import Clash.Verification.Internal
import qualified NonTemporal as NT

-- Uncommented tests: SVA does not support "never" keyword

fails1 = NT.fails1 SVA
{-# ANN fails1 (defSyn "fails1") #-}

fails2 = NT.fails2 SVA
{-# ANN fails2 (defSyn "fails2") #-}

fails3 = NT.fails3 SVA
{-# ANN fails3 (defSyn "fails3") #-}

--fails4 = NT.fails4 SVA
--{-# ANN fails4 (defSyn "fails4") #-}

fails5 = NT.fails5 SVA
{-# ANN fails5 (defSyn "fails5") #-}

--fails6 = NT.fails6 SVA
--{-# ANN fails6 (defSyn "fails6") #-}
--
--fails7 = NT.fails7 SVA
--{-# ANN fails7 (defSyn "fails7") #-}
--
--fails8 = NT.fails8 SVA
--{-# ANN fails8 (defSyn "fails8") #-}

fails9 = NT.fails9 SVA
{-# ANN fails9 (defSyn "fails9") #-}

--fails10 = NT.fails10 SVA
--{-# ANN fails10 (defSyn "fails10") #-}
--
--fails11 = NT.fails11 SVA
--{-# ANN fails11 (defSyn "fails11") #-}
--
--fails12 = NT.fails12 SVA
--{-# ANN fails12 (defSyn "fails12") #-}

fails13 = NT.fails13 SVA
{-# ANN fails13 (defSyn "fails13") #-}
