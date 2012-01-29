module Giraffe.Configuration where
  
data Configuration = Configuration {
  cfgTrackerId :: String,
  cfgTrackerVersion :: String,
  
  cfgRequestInterval :: Int,
  cfgMinRequestInterval :: Int,
  cfgRequestTimeout :: Integer,
  
  cfgHostName :: String,
  cfgPort :: Int,
  cfgTorrentDirectory :: FilePath,
  
  cfgAnnouncePrefix :: String,
  cfgScrapePrefix :: String
} deriving (Eq, Show)

defaultConfiguration :: Configuration 
defaultConfiguration = Configuration {
  cfgTrackerId = "Giraffe",
  cfgTrackerVersion = "0.1",
  
  cfgRequestInterval = 50,
  cfgMinRequestInterval = 50,
  cfgRequestTimeout = 10000,
  
  cfgHostName = "localhost",
  cfgPort = 8080,
  cfgTorrentDirectory = "data",
  
  cfgAnnouncePrefix = "/announce",
  cfgScrapePrefix = "/scrape"
}