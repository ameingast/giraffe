{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Giraffe.TrackerTest where

import           Control.Monad                        (liftM3, liftM4)
import           Data.Maybe
import           Data.Text                            (Text, pack)
import           System.Giraffe.Tracker
import           System.Giraffe.Types
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

instance Arbitrary Text where
    arbitrary = fmap pack arbitrary

instance Arbitrary Peer where
    arbitrary = liftM4 Peer arbitrary arbitrary arbitrary arbitrary

instance Arbitrary Info where
    arbitrary = liftM4 SingleFileInfo arbitrary arbitrary arbitrary arbitrary

instance Arbitrary SingleFileInfoFile where
    arbitrary = liftM3 SingleFileInfoFile arbitrary arbitrary arbitrary

instance Arbitrary MetaInfo where
    arbitrary = do
        infoDictionary <- arbitrary
        announceUrl <- arbitrary
        announceList <- arbitrary
        creationDate <- arbitrary
        comment <- arbitrary
        createdBy <- arbitrary
        encoding <- arbitrary

        return $ MetaInfo infoDictionary announceUrl announceList
            creationDate comment createdBy encoding

instance Arbitrary MetaInfoEncoding where
    arbitrary = return UTF8

instance Arbitrary Torrent where
    arbitrary = do
        name <- arbitrary
        metaInfo <- arbitrary
        infoHash <- arbitrary
        fileName <- arbitrary
        size <- arbitrary
        let seederPeerIds = []
        let leecherPeerIds = []
        downloaded <- arbitrary
        completed <- arbitrary
        incomplete <- arbitrary
        return $ Torrent name metaInfo infoHash fileName size
            seederPeerIds leecherPeerIds downloaded completed incomplete

propTorrentState :: Torrent -> Property
propTorrentState t = monadicIO $ do
    tr <- run createEmptyTracker

    run $ addTorrent tr t
    someT <- run $ lookupTorrent tr (torrentInfoHash t)
    assert $ Just t == someT

    run $ removeTorrent tr (torrentInfoHash t)
    someT2 <- run $ lookupTorrent tr (torrentInfoHash t)
    assert $ isNothing someT2

propPeerState :: Peer -> Property
propPeerState  peer = monadicIO $ do
    tr <- run createEmptyTracker

    run $ addPeer tr peer
    fetchedPeer <- run $ lookupPeer tr (peerId peer)
    assert $ Just peer == fetchedPeer

    run $ removePeer tr (peerId peer)
    fetchedPeer2 <- run $ lookupPeer tr (peerId peer)
    assert $ isNothing fetchedPeer2

propPeerTorrentState :: Peer -> Torrent -> Property
propPeerTorrentState peer torrent = monadicIO $ do
    tr <- run createEmptyTracker

    run $ addTorrent tr torrent

    run $ registerPeer tr peer torrent
    peers <- run $ lookupPeersForTorrent tr (torrentInfoHash torrent)
    assert $ [peer] == peers

    run $ removePeerFromTorrent tr peer torrent
    peers2 <- run $ lookupPeersForTorrent tr (torrentInfoHash torrent)
    assert $ [] == peers2

trackerTest :: Test
trackerTest = testGroup "Tracker/State"
    [ testProperty "peerState" propPeerState
    , testProperty "torrentState" propTorrentState
    , testProperty "peerTorrentState" propPeerTorrentState
    ]
