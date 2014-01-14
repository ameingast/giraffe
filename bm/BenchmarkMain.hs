module Main (main) where

import           System.Giraffe.ApplicationBenchmark (applicationBenchmark)
import           System.Giraffe.TrackerBenchmark     (trackerBenchmark)

main :: IO ()
main = do
    applicationBenchmark
    trackerBenchmark
