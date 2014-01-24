{-# LANGUAGE OverloadedStrings #-}

module System.Giraffe.TrackerBenchmark (trackerBenchmark) where

import           Criterion.Main
import           Data.Text              (Text, pack)
import           System.Giraffe.Tracker
import           System.Giraffe.Types
import           System.Random

invalidRequest :: InvalidRequest
invalidRequest = InvalidRequest "invalid request"

emptyScrapeRequest :: ScrapeRequest
emptyScrapeRequest = ScrapeRequest { scrapeRequestInfoHashes = [] }

fullScrapeRequest :: StdGen -> ScrapeRequest
fullScrapeRequest gen = ScrapeRequest
    { scrapeRequestInfoHashes = replicate 128 $ randomText 32 gen
    }

randomText :: Int -> StdGen -> Text
randomText size = pack . take size . randomRs ('a', 'z')

trackerBenchmark :: IO [Benchmark]
trackerBenchmark = do
    gen <- newStdGen
    tr <- createEmptyTracker

    return
        [ bgroup "Tracker/init"
            [ bench "createEmptyTracker" createEmptyTracker
            ]
        , bgroup "EmptyTracker/announce"
            []
        , bgroup "EmptyTracker/scrape"
            [ bench "emptyScrapeRequest" $ handleInMemoryScrape tr emptyScrapeRequest
            , bench "fullScrapeRequest" $ handleInMemoryScrape tr $ fullScrapeRequest gen
            ]
        , bgroup "EmptyTracker/invalid"
            [ bench "invalidRequest" $ handleInMemoryInvalid tr invalidRequest
            ]
        ]
