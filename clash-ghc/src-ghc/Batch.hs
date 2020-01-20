{-|
  Copyright   :  (C) 2013-2017, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Entry point for the @clash@ executable.
-}

module Main
  ( main -- :: IO ()
  ) where

import           System.Environment ( getArgs )
import           Clash.Main         ( defaultMain )

main :: IO ()
main = getArgs >>= defaultMain
