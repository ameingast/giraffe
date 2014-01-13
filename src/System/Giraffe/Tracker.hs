{-# LANGUAGE OverloadedStrings #-}

module System.Giraffe.Tracker where

import           Control.Concurrent.MVar
import           System.Giraffe.Types
import           System.Giraffe.Util

data InMemoryTracker = InMemoryTracker
    { inMemoryTrackerConfiguration :: Configuration
    , inMemoryTrackerPeers         :: MVar [Peer]
    , inMemoryTrackerTorrents      :: MVar [Torrent] }
    deriving (Eq)

instance Tracker InMemoryTracker where
    trackerConfiguration = inMemoryTrackerConfiguration
    handleAnnounceRequest = handleInMemoryAnnounce
    handleScrapeRequest = handleInMemoryScrape
    handleInvalidRequest = handleInMemoryInvalid

createInMemoryTracker :: Configuration -> IO InMemoryTracker
createInMemoryTracker config = do
    ts <- loadFromDiskIntoMVar "torrents.hs" []
    ps <- loadFromDiskIntoMVar "peers.hs" []
    return $ InMemoryTracker config ps ts

shutDownInMemoryTracker :: InMemoryTracker -> IO ()
shutDownInMemoryTracker tracker = do
    writeFromMVarOntoDisk "torrents.hs" (inMemoryTrackerTorrents tracker)
    writeFromMVarOntoDisk "peers.hs" (inMemoryTrackerPeers tracker)

handleInMemoryAnnounce :: InMemoryTracker -> AnnounceRequest -> IO AnnounceResponse
handleInMemoryAnnounce _ _ = return AnnounceResponse {}

handleInMemoryScrape :: InMemoryTracker -> ScrapeRequest -> IO ScrapeResponse
handleInMemoryScrape tr r = do
    let hs = scrapeRequestInfoHashes  r

    return ScrapeResponse
        { scrapeResponseFiles = []
        , scrapeResponseFailure = Nothing
        }

handleInMemoryInvalid :: InMemoryTracker -> InvalidRequest -> IO InvalidResponse
handleInMemoryInvalid _tracker (InvalidRequest msg) = do
    print msg
    return InvalidResponse { invalidResponseMessage = msg }
