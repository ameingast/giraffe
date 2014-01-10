{-# LANGUAGE OverloadedStrings #-}

module System.Giraffe.Application 
    ( handle
    , parseRequest
    , unParseScrapeRequest
    , unParseAnnounceRequest
    ) where

import           Control.Monad        (liftM)
import           Data.BEncode         (BEncode (..), bPack)
import qualified Data.ByteString      as BS (ByteString)
import           Data.Text            (Text, pack)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Data.Text.Read       (decimal, signed)
import           Network.HTTP.Types   (QueryItem, Status, status200, status500)
import           Network.Wai          (Request (..), Response, responseLBS, defaultRequest)
import           System.Giraffe.Types

handle :: RequestHandler a => Configuration -> a -> Request -> IO Response
handle config handler request = do
    (status, content) <- processRequest config handler (parseRequest request)
    return $ responseLBS status [("content-type", "plain/text")] (bPack content)

processRequest :: RequestHandler a => Configuration -> a -> TrackerRequest -> IO (Status, BEncode)
processRequest config h request = dispatchRequest h config request >>= \r -> case r of
    Nothing ->
        return (status500, BString "Cannot encode response")
    Just response ->
        return (status200, response)

dispatchRequest :: RequestHandler a => a -> Configuration -> TrackerRequest -> IO (Maybe BEncode)
dispatchRequest h config (Announce r) = liftM encode (handleAnnounceRequest h config r)
dispatchRequest h config (Scrape r) = liftM encode (handleScrapeRequest h config r)
dispatchRequest h config (Invalid r) =  liftM encode (handleInvalidRequest h config r)

parseRequest :: Request -> TrackerRequest
parseRequest r =
    case parseScrapeRequest r of
        Nothing ->
            case parseAnnounceRequest r of
                Nothing ->
                    Invalid (InvalidRequest "Malformed URL")
                Just announceRequest ->
                    Announce announceRequest
        Just scrapeRequest ->
            Scrape scrapeRequest

isValidRequest :: Request -> Text -> Bool
isValidRequest r url =
    let path = pathInfo r
    in (not . null) path && url == head path

parseScrapeRequest :: Request -> Maybe ScrapeRequest
parseScrapeRequest r | not (isValidRequest r "scrape") = Nothing
parseScrapeRequest r =
    let qs = queryString r
    in Just ScrapeRequest
        { scrapeRequestInfoHashes = findAll qs "info_hash" }


unParseScrapeRequest :: ScrapeRequest -> Request
unParseScrapeRequest scrapeRequest = defaultRequest
    { pathInfo = ["scrape"]
    , queryString = infoHashes (scrapeRequestInfoHashes scrapeRequest)
    }
    where
        infoHashes = map (\h -> ("info_hash", Just $ encodeUtf8 h))

parseAnnounceRequest :: Request -> Maybe AnnounceRequest
parseAnnounceRequest r | not (isValidRequest r "announce") = Nothing
parseAnnounceRequest r = do
    let qs = queryString r

    somePeerId <- find qs "id"
    someInfoHash <- find qs "info_hash"
    somePort <- find qs "port" >>= parseInteger
    someUploaded <- find qs "uploaded" >>= parseInteger
    someDownloaded <- find qs "downloaded" >>= parseInteger
    someLeft <- find qs "left" >>= parseInteger
    someNoPeerId <- find qs "no_peer_id" >>= parseBool
    someCompact <- find qs "compact" >>= parseBool

    Just AnnounceRequest
        { announceRequestPeerId = somePeerId
        , announceRequestInfoHash = someInfoHash
        , announceRequestPort = somePort
        , announceRequestIp = find qs "ip"
        , announceRequestUploaded = someUploaded
        , announceRequestDownloaded = someDownloaded
        , announceRequestLeft = someLeft
        , announceRequestNumWant = find qs "numwant" >>= parseInteger
        , announceRequestNoPeerId = someNoPeerId
        , announceRequestCompact = someCompact
        , announceRequestKey = find qs "key"
        , announceRequestEvent = find qs "event" >>= parseEvent
        , announceRequestTrackerId = find qs "trackerid"
    }

unParseAnnounceRequest :: AnnounceRequest -> Request
unParseAnnounceRequest r = defaultRequest
    { pathInfo = ["announce"]
    , queryString =
        [ ("id", Just $ encodeUtf8 $ announceRequestPeerId r)
        , ("info_hash", Just $ encodeUtf8 $ announceRequestInfoHash r)
        , ("port", encodeShowBs $ announceRequestPort r)
        , ("ip", liftM encodeUtf8 $ announceRequestIp r)
        , ("uploaded", encodeShowBs $ announceRequestUploaded r)
        , ("downloaded", encodeShowBs $ announceRequestDownloaded r)
        , ("left", encodeShowBs $ announceRequestLeft r)
        , ("numwant", liftM (encodeUtf8 . pack . show) (announceRequestNumWant r))
        , ("no_peer_id", encodeShowBs $ announceRequestNoPeerId r)
        , ("compact", encodeShowBs $ announceRequestCompact r)
        , ("key", liftM encodeUtf8 $ announceRequestKey r)
        , ("event", liftM (encodeUtf8 . pack . show) (announceRequestEvent r))
        , ("trackerid", liftM encodeUtf8 $ announceRequestTrackerId r)
        ]
    }

encodeShowBs :: Show a => a -> Maybe BS.ByteString
encodeShowBs = Just . encodeUtf8 . pack . show

parseInteger :: Text -> Maybe Integer
parseInteger t = case signed decimal t of
    Left _ ->
        Nothing
    Right (value, _) ->
        Just value

parseBool :: Text -> Maybe Bool
parseBool "t" = Just True
parseBool "true" = Just True
parseBool "True" = Just True
parseBool "f" = Just False
parseBool "false" = Just False
parseBool "False" = Just False
parseBool _ = Nothing

parseEvent :: Text -> Maybe AnnounceEvent
parseEvent "started" = Just AnnounceEventStarted
parseEvent "stopped" = Just AnnounceEventStopped
parseEvent "completed" = Just AnnounceEventCompleted
parseEvent _ = Nothing

-- FIXME: potentially slow, exchange to hashmap if performance problems appear
find :: [QueryItem] -> BS.ByteString -> Maybe Text
find [] _ = Nothing
find ((key, value):_) t | t == key = liftM decodeUtf8 value
find (_:xs) t = find xs t

findAll :: [QueryItem] -> BS.ByteString -> [Text]
findAll [] _ = []
findAll ((key, Nothing):xs) t | t == key = findAll xs t
findAll ((key, Just value):xs) t | t == key = decodeUtf8 value : findAll xs t
findAll (_:xs) t = findAll xs t
