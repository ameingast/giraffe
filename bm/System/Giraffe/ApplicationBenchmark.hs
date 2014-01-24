{-# LANGUAGE OverloadedStrings #-}

module System.Giraffe.ApplicationBenchmark (applicationBenchmark) where

import           Criterion.Main
import           Network.Wai                (Request (..), defaultRequest)
import           System.Giraffe.Application (parseRequest)

scrapeRequest  :: Request
scrapeRequest = defaultRequest
    { pathInfo = ["scrape"]
    , queryString = [("info_hash", Just "1234567890")]
    }

announceRequest :: Request
announceRequest = defaultRequest
    { pathInfo = ["announce"]
    , queryString =
        [ ("id", Just "Id")
        , ("info_hash", Just "EE1HVltl8qboicpk")
        , ("port", Just "12345")
        , ("uploaded", Just "12345")
        , ("downloaded", Just "12345")
        , ("left", Just "12345")
        , ("no_peer_id", Just "true")
        , ("compact", Just "false")
        , ("ip", Just "0.0.0.0")
        , ("numwant", Just "42")
        , ("key", Just "8rXQfvgI38mIZzQJ")
        , ("event", Just "stopped")
        , ("trackerid", Just "Giraffe")
        , ("user", Just "user")
        ]
    }

invalidRequest :: Request
invalidRequest = defaultRequest
    { pathInfo = ["UnZQnWkMoBBdvTxwA4AEnzAvdAHxwjBRGJsPOncHmvrEGv51z888eiY4spIfVfG9"]
    }

applicationBenchmark :: IO [Benchmark]
applicationBenchmark = return [ bgroup "Application/parseRequest"
        [ bench "scrapeRequest" $ whnf parseRequest scrapeRequest
        , bench "announceRequest" $ whnf parseRequest announceRequest
        , bench "invalidRequest" $ whnf parseRequest invalidRequest
        ]
    ]
