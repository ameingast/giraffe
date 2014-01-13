module Main (main) where

import           Network.Wai.Handler.Warp     (run)
import           System.Giraffe.Application   (handle)
import           System.Giraffe.Configuration
import           System.Giraffe.Tracker
import           System.Giraffe.Types

main :: IO ()
main = do
    config <- loadConfiguration
    tracker <- initTracker config :: IO InMemoryTracker
    run (cfgPort config) (handle tracker)
