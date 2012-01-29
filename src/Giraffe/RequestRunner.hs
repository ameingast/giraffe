module Giraffe.RequestRunner (
  runRequest
) where

import Giraffe.Configuration
import Giraffe.Data
import Giraffe.Tracker.RequestHandling
import Giraffe.Wai.HttpRequestParser

import Data.BEncode
import Data.Enumerator.Internal(Iteratee)
import Network.HTTP.Types(status200)
import Network.Wai(Response (..), Request (..))
import Network.Wai.Handler.Warp(run)

import qualified Data.ByteString as BS
  
runRequest :: Request -> Iteratee BS.ByteString IO Response
runRequest request = do
  let parsedRequest = parseRequest request

  case parsedRequest of
    (Announce announceRequest) -> undefined
    (Scrape scrapeRequest) -> undefined
    (InvalidRequest info) ->
      return $ ResponseFile status200 [] info Nothing

  something <- processRequest parsedRequest

  return $ ResponseFile status200 [] "Main.hs" Nothing --("Content-type", "text/html")

processRequest :: TrackerRequest -> Iteratee BS.ByteString IO BEncode
processRequest request = 
  case request of
    (Announce announceRequest) ->
      undefined
      -- processAnnounceRequest announceRequest >>= return . encode
    (Scrape scrapeRequest) ->
      undefined 
      -- processScrapeRequest scrapeRequest >>= return . encode
    (InvalidRequest msg) -> 
      undefined