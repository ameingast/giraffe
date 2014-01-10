{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Giraffe.ApplicationTest where

import           Data.Text                            (Text, pack)
import           Network.Wai                          (Request (..),
                                                       defaultRequest)
import           System.Giraffe.Application
import           System.Giraffe.Types
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

instance Arbitrary Text where
    arbitrary = fmap pack arbitrary

instance Arbitrary AnnounceEvent where
    arbitrary = return AnnounceEventStopped

instance Arbitrary ScrapeRequest where
    arbitrary = do
        infoHashes <- arbitrary
        return $ ScrapeRequest infoHashes

propParseScrapeRequest :: ScrapeRequest -> Bool
propParseScrapeRequest scrapeRequest = 
    let parsedRequest = (parseRequest . unParseScrapeRequest) scrapeRequest
    in parsedRequest == Scrape scrapeRequest

instance Arbitrary AnnounceRequest where
    arbitrary = do
        hash <- arbitrary
        aPeerId <- arbitrary
        port <- arbitrary
        ip <- arbitrary
        uploaded <- arbitrary
        downloaded <- arbitrary
        left <- arbitrary
        event <- arbitrary
        numWant <- arbitrary
        noPeerId <- arbitrary
        compact <- arbitrary
        key <- arbitrary
        trackerId <- arbitrary
        return $ AnnounceRequest hash aPeerId port ip uploaded downloaded left event
            numWant noPeerId compact key trackerId

propParseAnnounceRequest :: AnnounceRequest -> Bool
propParseAnnounceRequest r = 
    let parsedRequest = (parseRequest . unParseAnnounceRequest) r
    in parsedRequest == Announce r

propParseInvalidRequest :: [Text] -> Bool
propParseInvalidRequest pathInfos = parsedRequest == Invalid invalidRequest
    where
        parsedRequest = parseRequest $ defaultRequest { pathInfo = pathInfos }
        invalidRequest = InvalidRequest { invalidRequestMessage = "Malformed URL" }

applicationTest :: Test
applicationTest = testGroup "Application/parseRequest"
    [ testProperty "scrapeRequest" propParseScrapeRequest
    , testProperty "invalidRequest" propParseInvalidRequest
    , testProperty "announceRequest" propParseAnnounceRequest ]
