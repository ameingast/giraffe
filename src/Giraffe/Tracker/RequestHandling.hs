module Giraffe.Tracker.RequestHandling where

import Giraffe.Configuration
import Giraffe.Data.Announce
import Giraffe.Data.Scrape

class AnnounceRequestHandling a where
  handleAnnounceRequest :: a -> AnnounceRequest -> IO AnnounceResponse
  
class ScrapeRequestHandling a where
  handleScrapeRequest :: a -> ScrapeRequest -> IO ScrapeResponse

processAnnounceRequest :: AnnounceRequest -> IO AnnounceResponse
processAnnounceRequest = undefined

processScrapeRequest :: ScrapeRequest -> IO ScrapeResponse
processScrapeRequest = undefined