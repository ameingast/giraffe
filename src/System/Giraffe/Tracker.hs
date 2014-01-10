{-# LANGUAGE OverloadedStrings #-}

module System.Giraffe.Tracker where

import           Data.Text             (append)
import           System.Giraffe.Types
import           System.Log.FastLogger

data InMemoryRequestHandler = InMemoryRequestHandler
    deriving (Show, Eq, Read, Ord)

instance RequestHandler InMemoryRequestHandler where
    handleAnnounceRequest _handler config _request = do
        pushLogStr (cfgLog config) "test"
        return AnnounceResponse {}

    handleScrapeRequest _handler config _request = do
        pushLogStr (cfgLog config) "scrape"
        return ScrapeResponse
            { scrapeResponseFiles = []
            , scrapeResponseFailure = Nothing
            }

    handleInvalidRequest _handler config (InvalidRequest msg) = do
        pushLogStr (cfgLog config) $ toLogStr ("Invalid request: " `append` msg)
        return InvalidResponse { invalidResponseMessage = msg }
