module Paths_clash_cosim where

-- HACK, see:
--   https://neilmitchell.blogspot.dk/2008/02/adding-data-files-using-cabal.html
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
