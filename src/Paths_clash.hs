module Paths_clash where

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . ("../" ++)
