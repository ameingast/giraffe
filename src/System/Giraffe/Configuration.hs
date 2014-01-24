module System.Giraffe.Configuration where

import           Data.Monoid
import           System.Giraffe.Types
import           System.Giraffe.Util

loadConfiguration :: FilePath -> IO Configuration
loadConfiguration path =
    readConfiguration path >>= \c -> case c of
        Nothing -> return mempty
        Just configuration -> return configuration

readConfiguration :: FilePath -> IO (Maybe Configuration)
readConfiguration path =
    readFileSafe path >>= \contents -> case contents of
        Nothing -> return Nothing
        Just configuration -> (return . read) configuration

writeConfiguration :: FilePath -> Configuration -> IO ()
writeConfiguration path = writeFile path . show
