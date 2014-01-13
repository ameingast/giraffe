{-# LANGUAGE OverloadedStrings #-}

module System.Giraffe.Tracker where

import           Control.Concurrent.MVar
import           Control.Monad           (liftM)
import qualified Data.Map                as M
import           System.Giraffe.Types
import           System.Giraffe.Util

data InMemoryTracker = InMemoryTracker
    { inMemoryTrackerConfiguration :: Configuration
    , inMemoryTrackerPeers         :: MVar (M.Map InfoHash Peer)
    , inMemoryTrackerTorrents      :: MVar (M.Map InfoHash Torrent)
    } deriving (Eq)

instance Tracker InMemoryTracker where
    trackerConfiguration = inMemoryTrackerConfiguration
    handleAnnounceRequest = handleInMemoryAnnounce
    handleScrapeRequest = handleInMemoryScrape
    handleInvalidRequest = handleInMemoryInvalid
    initTracker = createInMemoryTracker
    stopTracker = shutDownInMemoryTracker

createInMemoryTracker :: Configuration -> IO InMemoryTracker
createInMemoryTracker c = do
    let dir = cfgDataDirectory c
    ts <- loadFromDiskIntoMVar (dir ++ "torrents.hs") M.empty
    ps <- loadFromDiskIntoMVar (dir ++ "peers.hs") M.empty
    return $ InMemoryTracker c ps ts

shutDownInMemoryTracker :: InMemoryTracker -> IO ()
shutDownInMemoryTracker tr = do
    let dir = cfgDataDirectory (trackerConfiguration tr)
    writeFromMVarOntoDisk (dir ++ "torrents.hs") (inMemoryTrackerTorrents tr)
    writeFromMVarOntoDisk (dir ++ "peers.hs") (inMemoryTrackerPeers tr)

handleInMemoryAnnounce :: InMemoryTracker -> AnnounceRequest -> IO AnnounceResponse
handleInMemoryAnnounce tr r = do
    let c = trackerConfiguration tr
    let h = announceRequestInfoHash r

    lookupTorrent tr h >>= \t -> case t of
        Nothing ->
            -- TODO: handle error
            return undefined
        Just torrent -> do
            peers <- lookupPeersForTorrent tr torrent

            -- TODO: register client

            return AnnounceResponse
                { announceResponseInterval = cfgRequestInterval c
                , announceResponseMinimumInterval = Just (cfgMinRequestInterval c)
                , announceResponsePeers = peers
                , announceResponseFailureReason = Nothing
                , announceResponseWarningMessage = Nothing
                , announceResponseTrackerId = cfgTrackerId c
                , announceResponseComplete = torrentCompleted torrent
                , announceResponseIncomplete = torrentIncomplete torrent
                }

handleInMemoryScrape :: InMemoryTracker -> ScrapeRequest -> IO ScrapeResponse
handleInMemoryScrape tr r =
    liftM (ScrapeResponse . dropNothing)
        (mapM (findScrapeInfoFile tr) (scrapeRequestInfoHashes r))

findScrapeInfoFile :: InMemoryTracker -> InfoHash -> IO (Maybe ScrapeResponseFile)
findScrapeInfoFile tr h =
    lookupTorrent tr h >>= \t -> case t of
        Nothing ->
            return Nothing
        Just torrent ->
            return $ Just ScrapeResponseFile
                { scrapeResponseFileComplete = torrentCompleted torrent
                , scrapeResponseFileDownloaded = torrentDownloaded torrent
                , scrapeResponseFileIncomplete = torrentIncomplete torrent
                , scrapeResponseFileName = torrentFileName torrent
                }

lookupPeer :: InMemoryTracker -> PeerId -> IO (Maybe Peer)
lookupPeer tr h = liftM (M.lookup h) (readMVar (inMemoryTrackerPeers tr))

lookupTorrent :: InMemoryTracker -> InfoHash -> IO (Maybe Torrent)
lookupTorrent tr h = liftM (M.lookup h) (readMVar (inMemoryTrackerTorrents tr))

lookupPeersForTorrent :: InMemoryTracker -> Torrent -> IO [Peer]
lookupPeersForTorrent tr t =
    liftM dropNothing (mapM (lookupPeer tr) 
        (torrentSeederPeerIds t ++ torrentLeecherPeerIds t))

handleInMemoryInvalid :: InMemoryTracker -> InvalidRequest -> IO InvalidResponse
handleInMemoryInvalid _tracker (InvalidRequest msg) =
    return InvalidResponse { invalidResponseMessage = msg }
