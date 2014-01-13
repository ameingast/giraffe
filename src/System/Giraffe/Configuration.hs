module System.Giraffe.Configuration where

import           System.Giraffe.Types
import           System.Giraffe.Util

loadConfiguration :: IO Configuration
loadConfiguration =
    readConfiguration "config.hs" >>= \c -> case c of
        Nothing -> return defaultConfiguration
        Just configuration -> return configuration

readConfiguration :: FilePath -> IO (Maybe Configuration)
readConfiguration path =
    readFileSafe path >>= \contents -> case contents of
        Nothing -> return Nothing
        Just configuration -> (return . read) configuration

writeConfiguration :: FilePath -> Configuration -> IO ()
writeConfiguration path = writeFile path . show
