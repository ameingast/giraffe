module Giraffe.Wai.HttpRequestParser(
  parseRequest,
  TrackerRequest(..)
) where

import Giraffe.Data.Announce
import Giraffe.Data.Scrape  

import Network.HTTP.Types(QueryItem)
import Network.Wai(Request, queryString)

import qualified Data.ByteString as BS

data TrackerRequest = 
  Announce AnnounceRequest |
  Scrape ScrapeRequest |
  InvalidRequest String

parseRequest :: Request -> TrackerRequest
parseRequest request 
  | isAnnounceRequest request = buildAnnounceRequest queryItems AnnounceRequest{}
  | isScrapeRequest request = buildScrapeRequest queryItems ScrapeRequest{}
  | otherwise = InvalidRequest "Malformed URL"
    where queryItems = queryString request

isAnnounceRequest :: Request -> Bool
isAnnounceRequest request = True -- url `startswith` "announce"

isScrapeRequest :: Request -> Bool
isScrapeRequest request = True -- url `startsWith` "scrape"

buildAnnounceRequest :: [QueryItem] -> AnnounceRequest -> TrackerRequest
buildAnnounceRequest xxs@((a, b):xs) request
  | xxs == [] = Announce request
  {--| key == "id" = undefined
  | key == "info_hash" = undefined
  | key == "peer_id" = undefined
  | key == "uploaded" = undefined
  | key == "downloaded" = undefined
  | key == "left" = undefined
  | key == "compact" = undefined
  | key == "event" = undefined
  | key == "ip" = undefined
  | key == "numwant" = undefined
  | key == "key" = undefined
  | key == "trackerid" = undefined
  | otherwise = InvalidRequest $ "Unsupported query parameter: " ++ key
    where
      key = a
      value = b--}

buildScrapeRequest :: [QueryItem] -> ScrapeRequest -> TrackerRequest
buildScrapeRequest xxs@((a, b):xs) request
  | xxs == [] = Scrape request
  {--| key == "id" && value == Nothing = InvalidRequest "No id value supplied."
  | key == "id" = undefined
  | key == "info_hash" && value == Nothing = InvalidRequest "No info_hash value supplied."
  | key == "info_hash" = undefined
    where
      key = a
      value = b--}