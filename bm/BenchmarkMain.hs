module Main (main) where

import           Criterion.Config
import           Criterion.Main
import           System.Giraffe.ApplicationBenchmark (applicationBenchmark)
import           System.Giraffe.TrackerBenchmark     (trackerBenchmark)

main :: IO ()
main = do
    applicationBms <- applicationBenchmark
    trackerBms <- trackerBenchmark

    defaultMainWith defaultConfig (return ()) $ applicationBms ++ trackerBms
