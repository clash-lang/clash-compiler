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

-- clk = 10 :: Int

-- play

-- deriveClocksInstances 16

-- playAST :: IO ()
-- playAST = putStrLn $(stringE . ppShow =<< play)

-- playRender :: IO ()
-- playRender = putStrLn $(stringE . pprint =<< play)

main :: IO ()
main = do
  fName <- fmap head getArgs
  writeFile fName $(stringE . ppShow =<< deriveClocksInstances 16)
