{-# LANGUAGE OverloadedStrings #-}

module System.Giraffe.Application
    ( bootstrap
    , handle
    , parseRequest
    , unParseScrapeRequest
    , unParseAnnounceRequest
    ) where

import           Control.Monad           (liftM)
import           Data.BEncode            (BEncode (..), bPack)
import qualified Data.ByteString         as BS (ByteString)
import           Data.Text               (Text, pack)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           Data.Text.Read          (decimal, signed)
import           Network.HTTP.Types      (QueryItem, Status, status200)
import           Network.Socket.Internal (SockAddr (..))
import           Network.Wai             (Request (..), Response,
                                          defaultRequest, responseLBS)
import           System.Giraffe.Types

bootstrap :: Tracker a => a -> IO ()
bootstrap _ = return ()

handle :: Tracker a => a -> Request -> IO Response
handle handler request = do
    (status, content) <- processRequest handler (parseRequest request)
    return $ responseLBS status [("content-type", "plain/text")] (bPack content)

processRequest :: Tracker a => a -> TrackerRequest -> IO (Status, BEncode)
processRequest h request =
    dispatchRequest h request >>= \r -> return (status200, r)

dispatchRequest :: Tracker a => a -> TrackerRequest -> IO BEncode
dispatchRequest h (Announce r) = liftM encode (handleAnnounceRequest h r)
dispatchRequest h (Scrape r) = liftM encode (handleScrapeRequest h r)
dispatchRequest h (Invalid r) = liftM encode (handleInvalidRequest h r)

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
unParseScrapeRequest scrapeRequest =
    let infoHashes = map (\h -> ("info_hash", Just $ encodeUtf8 h))
    in defaultRequest
        { pathInfo = ["scrape"]
        , queryString = infoHashes (scrapeRequestInfoHashes scrapeRequest)
        }

parseAnnounceRequest :: Request -> Maybe AnnounceRequest
parseAnnounceRequest r | not (isValidRequest r "announce") = Nothing
parseAnnounceRequest r = do
    let qs = queryString r

    somePeerId <- find qs "id"
    someInfoHash <- find qs "info_hash"
    someIp <- parseIp r
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
        , announceRequestIp = someIp
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

parseIp :: Request -> Maybe Text
parseIp r =
    case find (queryString r) "ip" of
        Just ip ->
            Just ip
        Nothing ->
            case remoteHost r of
                (SockAddrInet _ addr) ->
                    Just $ pack $ show addr
                _ ->
                    Nothing

unParseAnnounceRequest :: AnnounceRequest -> Request
unParseAnnounceRequest r = defaultRequest
    { pathInfo = ["announce"]
    , queryString =
        [ ("id", Just $ encodeUtf8 $ announceRequestPeerId r)
        , ("info_hash", Just $ encodeUtf8 $ announceRequestInfoHash r)
        , ("port", encodeShowBs $ announceRequestPort r)
        , ("ip", Just $ encodeUtf8 $ announceRequestIp r)
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
