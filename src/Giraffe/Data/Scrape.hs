module Giraffe.Data.Scrape (
  ScrapeRequest(..),
  ScrapeResponse(..),
  ScrapeResponseFile(..)
) where

import Giraffe.Util.BECoding(BEEncodable, encode)

import qualified Data.ByteString as BS(ByteString)

data ScrapeRequest = ScrapeRequest {
  -- | The scrape URL may be supplemented by the optional parameter info_hash, 
  -- a 20-byte value as described above. 
  -- This restricts the tracker's report to that particular torrent. 
  -- Otherwise stats for all torrents that the tracker is managing are 
  -- returned. 
  scrapeInfoHashes :: [BS.ByteString]
}

data ScrapeResponse = ScrapeResponse {
  -- | A dictionary containing one key/value pair for each torrent for which 
  -- there are stats. 
  -- 
  -- If info_hash was supplied and was valid, this dictionary will contain a 
  -- single key/value. Each key consists of a 20-byte binary info_hash.
  scrapeFiles :: [ScrapeResponseFile],
  
  -- | Human-readable error message as to why the request failed. 
  scrapeFailure :: Maybe String
}

data ScrapeResponseFile = ScrapeResponseFile {
  -- | The number of peers with the entire file, i.e. seeders.
  scrapeComplete :: Integer,
  
  -- | The total number of times the tracker has registered a completion 
  -- ("event=complete", i.e. a client finished downloading the torrent).
  scrapeDownloaded :: Integer,
  
  -- | The number of non-seeder peers, aka leechers.
  scrapeIncomplete :: Integer,
  
  -- | The torrents internal name, as specified by the "name" file in the 
  -- info section of the .torrent file
  scrapeName :: String
}

instance BEEncodable ScrapeResponse where
  encode (ScrapeResponse _ _) = undefined
  
instance BEEncodable ScrapeResponseFile where
  encode (ScrapeResponseFile _ _ _ _) = undefined