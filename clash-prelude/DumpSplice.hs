{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Language.Haskell.TH
import System.Environment (getArgs)
import Text.Show.Pretty

-- import Clash.Clocks
import Clash.Clocks.Deriving

-- deriveClocksInstances 16

out :: String
-- out = $(stringE . ppShow =<< deriveClocksInstances)
-- out = $(stringE . pprint =<< deriveClocksInstances)
out = $(stringE . pprint =<< deriveClocksSyncInstances)
-- out = $(stringE . ppShow =<< deriveClocksSyncInstance 3)

main :: IO ()
main = do
  fName <- fmap head getArgs
  writeFile fName out
