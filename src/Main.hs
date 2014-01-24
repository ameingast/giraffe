module Main (main) where

import           Network.Wai.Handler.Warp     (run)
import           System.Giraffe.Application   (handle)
import           System.Giraffe.Tracker
import           System.Giraffe.Types

main :: IO ()
main = do
    tr <- createInMemoryTracker
    let cfg = inMemoryTrackerConfiguration tr
    run (cfgPort cfg) (handle tr)
