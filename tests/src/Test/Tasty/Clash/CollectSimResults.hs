{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Tasty.Clash.CollectSimResults where
import Clash.Prelude
import Language.Haskell.TH.Lib

-- | Creates vector literal out of the first n samples of the given signal.
collectSimResults :: (KnownDomain dom, Lift a, NFDataX a) => Int -> Signal dom a -> ExpQ
collectSimResults cycles entity = listToVecTH (sampleN cycles entity)
