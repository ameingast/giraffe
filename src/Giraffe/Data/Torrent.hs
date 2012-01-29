module Giraffe.Data.Torrent (
  Torrent(..)
) where
  
import Giraffe.Data.MetaInfo(MetaInfo)
import qualified Data.ByteString as BS(ByteString)

data Torrent = Torrent {
  torrentName :: String,
  
  torrentMetaInfo :: MetaInfo,
  
  torrentInfoHash :: BS.ByteString,
  torrentFileName :: FilePath,
  torrentSize :: Integer
}