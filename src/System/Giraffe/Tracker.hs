{-# LANGUAGE OverloadedStrings #-}

module System.Giraffe.Tracker where

import           Control.Concurrent.MVar
import           Control.Monad           (liftM, liftM2)
import           Data.Functor            ((<$>))
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Monoid             (mempty)
import           Data.Time.Clock.POSIX
import           System.Directory
import           System.Giraffe.Types
import           System.Giraffe.Util
import           System.Posix.Signals

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

    createDirectoryIfMissing True dir

    ts <- loadFromDiskIntoMVar (dir ++ "torrents.hs") M.empty
    ps <- loadFromDiskIntoMVar (dir ++ "peers.hs") M.empty

    let tr = InMemoryTracker c ps ts
    _ <- installHandler sigINT (sigIntHandler tr) Nothing
    return tr

sigIntHandler :: InMemoryTracker -> Handler
sigIntHandler tr = CatchOnce $ shutDownInMemoryTracker tr >> raiseSignal sigINT

createEmptyTracker :: IO InMemoryTracker
createEmptyTracker =
    liftM2 (InMemoryTracker mempty) (newMVar M.empty) (newMVar M.empty)

shutDownInMemoryTracker :: InMemoryTracker -> IO ()
shutDownInMemoryTracker tr = do
    let dir = cfgDataDirectory (trackerConfiguration tr)
    writeFromMVarOntoDisk (dir ++ "torrents.hs") (inMemoryTrackerTorrents tr)
    writeFromMVarOntoDisk (dir ++ "peers.hs") (inMemoryTrackerPeers tr)

handleInMemoryAnnounce :: InMemoryTracker -> AnnounceRequest -> IO AnnounceResponse
handleInMemoryAnnounce tr r = do
    let c = trackerConfiguration tr
    let h = announceRequestInfoHash r
    let peer = Peer (announceRequestInfoHash r) (announceRequestIp r) (announceRequestPort r) 0

    lookupTorrent tr h >>= \t -> case t of
        Nothing ->
            return AnnounceResponse
                { announceResponseInterval = cfgRequestInterval c
                , announceResponseMinimumInterval = Just (cfgMinRequestInterval c)
                , announceResponsePeers = []
                , announceResponseFailureReason = Just "No such torrent registered"
                , announceResponseWarningMessage = Nothing
                , announceResponseTrackerId = cfgTrackerId c
                , announceResponseComplete = 0
                , announceResponseIncomplete = 0
                }

        Just torrent -> do
            case announceRequestEvent r of
                Just AnnounceEventStarted ->
                    registerPeer tr peer torrent
                Just AnnounceEventCompleted ->
                    completePeer tr peer torrent
                Just AnnounceEventStopped ->
                    removePeerFromTorrent tr peer torrent
                Nothing ->
                    return ()

            peers <- lookupPeersForTorrent tr h

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

registerPeer :: InMemoryTracker -> Peer -> Torrent -> IO ()
registerPeer tr p t = do
    addPeer tr p
    modifyMVar_ (inMemoryTrackerTorrents tr) (return . addLeecherPeer t p)

addLeecherPeer :: Torrent -> Peer -> M.Map InfoHash Torrent -> M.Map InfoHash Torrent
addLeecherPeer t p =
    let h = torrentInfoHash t
    in M.insert h t
        { torrentIncomplete = torrentIncomplete t + 1
        , torrentLeecherPeerIds = peerId p : torrentLeecherPeerIds t
        }

completePeer :: InMemoryTracker -> Peer -> Torrent -> IO ()
completePeer tr p t = do
    addPeer tr p
    modifyMVar_ (inMemoryTrackerTorrents tr) (return . updateCompletePeer t p)

updateCompletePeer :: Torrent -> Peer -> M.Map InfoHash Torrent -> M.Map InfoHash Torrent
updateCompletePeer t p =
    let h = torrentInfoHash t
    in M.insert h t
        { torrentCompleted = torrentCompleted t + 1
        , torrentIncomplete = torrentIncomplete t - 1
        , torrentSeederPeerIds = peerId p : torrentSeederPeerIds t
        , torrentLeecherPeerIds = [ x | x <- torrentLeecherPeerIds t , x /= peerId p ]
        }

removePeerFromTorrent :: InMemoryTracker -> Peer -> Torrent -> IO ()
removePeerFromTorrent tr p t =
    modifyMVar_ (inMemoryTrackerTorrents tr) (return . updateRemovePeer t p)

updateRemovePeer :: Torrent -> Peer -> M.Map InfoHash Torrent -> M.Map InfoHash Torrent
updateRemovePeer t p =
    let h =  torrentInfoHash t
    in M.insert h t
        { torrentSeederPeerIds = [ x | x <- torrentSeederPeerIds t, x /= peerId p ]
        , torrentLeecherPeerIds = [ x | x <- torrentLeecherPeerIds t, x /= peerId p ]
        }

handleInMemoryScrape :: InMemoryTracker -> ScrapeRequest -> IO ScrapeResponse
handleInMemoryScrape tr r =
    liftM (ScrapeResponse . catMaybes)
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

addTorrent :: InMemoryTracker -> Torrent -> IO ()
addTorrent tr t =
    modifyMVar_ (inMemoryTrackerTorrents tr) $ return . M.insert (torrentInfoHash t) t

addPeer :: InMemoryTracker -> Peer-> IO ()
addPeer tr peer = do
    now <- round <$> getPOSIXTime
    modifyMVar_ (inMemoryTrackerPeers tr) $
        return . M.insert (peerId peer) peer { peerLastSeen = now }

removePeer :: InMemoryTracker -> PeerId -> IO ()
removePeer tr pid =
    modifyMVar_ (inMemoryTrackerPeers tr) $ return . M.delete pid

lookupTorrent :: InMemoryTracker -> InfoHash -> IO (Maybe Torrent)
lookupTorrent tr h = liftM (M.lookup h) (readMVar (inMemoryTrackerTorrents tr))

removeTorrent :: InMemoryTracker -> InfoHash -> IO ()
removeTorrent tr h =
    modifyMVar_ (inMemoryTrackerTorrents tr) $ return . M.delete h

lookupPeersForTorrent :: InMemoryTracker -> InfoHash -> IO [Peer]
lookupPeersForTorrent tr h = do
    peerIds <- lookupPeerIdsForTorrent tr h
    peers <- mapM (lookupPeer tr) peerIds
    return $ catMaybes peers

lookupPeerIdsForTorrent :: InMemoryTracker -> InfoHash -> IO [PeerId]
lookupPeerIdsForTorrent tr h =
    lookupTorrent tr h >>= \t -> case t of
        Nothing ->
            return []
        Just torrent->
            return $ torrentSeederPeerIds torrent ++ torrentLeecherPeerIds torrent

filterPeers :: Int -> [Peer] -> [Peer]
filterPeers amount xs | length xs <= amount = xs
filterPeers amount xs = take amount xs

handleInMemoryInvalid :: InMemoryTracker -> InvalidRequest -> IO InvalidResponse
handleInMemoryInvalid _tracker (InvalidRequest msg) =
    return InvalidResponse { invalidResponseMessage = msg }
