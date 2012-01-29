module Giraffe.Data.MetaInfo (
  MetaInfo(..)
) where

import Giraffe.Data.Info(Info)
import Giraffe.Util.BECoding(BEEncodable, encode)

import Data.Time.Clock(UTCTime)
import Network.URL(URL)

data MetaInfo = MetaInfo {
  -- | See 'Info'.
  metaInfoDictionary :: Info,
  
  -- | The announce URL of the tracker.
  metaInfoAnnounceUrl :: URL,
  
  -- | Optional. This is an extention to the official specification, 
  -- offering backwards-compatibility.
  metaInfoAnnounceList :: Maybe [[String]],
  
  -- | Optional. The creation time of the torrent, in standard UNIX epoch 
  -- format.
  metaInfoCreationDate :: Maybe UTCTime,
  
  -- | Optional. Free-form textual comments of the author.
  metaInfoComment :: Maybe String,
  
  -- | Optional. Name and version of the program used to create the torrent.
  metaInfoCreatedBy :: Maybe String,
  
  -- | Optional. The string encoding format used to generate the pieces part 
  -- of the info dictionary in the torrent.
  metaInfoEncoding :: Maybe MetaInfoEncoding
}

data MetaInfoEncoding = UTF8 deriving (Eq, Show)

instance BEEncodable MetaInfo where
  encode (MetaInfo _ _ _ _ _ _ _) = undefined