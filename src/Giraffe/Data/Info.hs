module Giraffe.Data.Info (
  Info(..)
) where

import Giraffe.Util.BECoding(BEEncodable, encode)

import qualified Data.ByteString as BS(ByteString)
  
-- | A dictionary that describes the file(s) of the torrent. 
-- There are two possible forms: 
--
--   * one for the case of a 'single-file' torrent with no directory structure
--
--   * one for the case of a 'multi-file' torrent.
data Info = SingleFileInfo {
  -- | Number of bytes in each piece.
  singleFileInfoPiecesLength :: Integer,
  
  -- | A string consisting of the concatenation of all 20-byte SHA1 hash 
  -- values, one per piece (byte string, i.e. not urlencoded).
  singleFileInfoPieces :: BS.ByteString,
  
  -- | Optional. If it is set to "1", the client MUST publish its presence to 
  -- get other peers ONLY via the trackers explicitly described in the 
  -- metainfo file. If this field is set to "0" or is not present, 
  -- the client may obtain peer from other means, e.g. PEX peer exchange, dht. 
  -- Here, "private" may be read as "no external peer source".
  singleFileInfoPrivate :: Bool,
  
  -- | The dictionary describing a single file.
  sinleFileInfo :: SingleFileInfoFile
  
} | MultiFileInfo {
  -- | See 'singleFileInfoPiecesLength'.
  multiFileInfoLength :: Integer,
  
  -- | See 'singleFileInfoPieces'.
  multiFileInfoPieces :: BS.ByteString,
  
  -- | See 'singleFileInfoPrivate'.
  multiFileInforPivate :: Bool,
  
  -- | The file path of the directory in which to store all the files. 
  -- This is purely advisory.
  multiFileInfoName :: String,
  multiFileInfoFiles :: [MultiFileInfoFile]
}

data SingleFileInfoFile = SingleFileInfoFile {
  -- | The filename. This is purely advisory.
  singleFileInfoName :: FilePath,

  -- | Optional. A 32-character hexadecimal string corresponding to the MD5 
  -- sum of the file. This is not used by BitTorrent at all, but it is 
  -- included by some programs for greater compatibility.
  singleFileInfoMd5Sum :: Maybe String,

  -- | Length of the file in bytes.
  singleFileInfoLength :: Integer
}

data MultiFileInfoFile = MultiFileInfoFile {
  -- | A list containing one or more string elements that together represent 
  -- the path and filename. Each element in the list corresponds to either a 
  -- directory name or (in the case of the final element) the filename. 
  -- For example, a the file "dir1/dir2/file.ext" would consist of three string
  -- elements: ["dir1", "dir2", "file.ext"].
  multiFileInfoFilePath :: [String],
  
  -- | The length of the file in bytes.
  multiInfoFileLength :: Integer,
  
  -- | See 'singleFileInfoMd5Sum'.
  multiInfoFileMd5Sum :: Maybe String
}

instance BEEncodable Info where
  encode (SingleFileInfo _ _ _ _) = undefined
  encode (MultiFileInfo _ _ _ _ _) = undefined