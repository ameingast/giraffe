{-# LANGUAGE OverloadedStrings #-}

module System.Giraffe.Types where

import           Data.BEncode         (BEncode (..))
import qualified Data.ByteString.Lazy as BS (fromChunks)
import qualified Data.Map             as M
import           Data.Text            (Text)
import           Data.Text.Encoding   (encodeUtf8)
import           System.Giraffe.Util

-- | The data type that maps http announce request.
data AnnounceRequest = AnnounceRequest
    { announceRequestInfoHash   :: InfoHash
    -- ^ A urlencoded 20-byte SHA1 hash of the value of the info key from the
    -- Metainfo file. Note that the value will be a bencoded dictionary.

    , announceRequestPeerId     :: PeerId
    -- ^ A urlencoded 20-byte string used as a unique ID for the client,
    -- generated by the client at startup. This is allowed to be any value,
    -- and may be binary data.
    -- There are currently no guidelines for generating this peer ID.

    , announceRequestPort       :: Integer
    -- ^ The port number that the client is listening on. Ports reserved for
    -- BitTorrent are typically 6881-6889. Clients may choose to give up if it
    -- cannot establish a port within this range.

    , announceRequestIp         :: Text
    -- ^ Optional. The true IP address of the client machine, in dotted quad format
    -- or rfc3513 defined hexed IPv6 address.
    --
    -- Notes: In general this parameter is not necessary as the address of the
    -- client can be determined from the IP address from which the HTTP request
    -- came. The parameter is only needed in the case where the IP address that
    -- the request came in on is not the IP address of the client.
    -- This happens if the client is communicating to the tracker through a
    -- proxy (or a transparent web proxy/cache.) It also is necessary when both
    -- the client and the tracker are on the same local side of a NAT gateway.
    -- The reason for this is that otherwise the tracker would give out the
    -- internal (RFC1918) address of the client, which is not routable.
    -- Therefore the client must explicitly state its (external, routable)
    -- IP address to be given out to external peers. Various trackers treat this
    -- parameter differently. Some only honor it only if the IP address that the
    -- request came in on is in RFC1918 space. Others honor it unconditionally,
    -- while others ignore it completely. In case of IPv6 address
    -- (e.g.: 2001:db8:1:2::100) it indicates only that client can communicate
    -- via IPv6.

    , announceRequestUploaded   :: Integer
    -- ^ The total amount uploaded (since the client sent the 'started'
    -- event to the tracker) in base ten ASCII. While not explicitly stated
    -- in the official specification, the concensus is that this should be the
    -- total number of bytes uploaded.

    , announceRequestDownloaded :: Integer
    -- ^ The total amount downloaded (since the client sent the 'started'
    -- event to the tracker) in base ten ASCII. While not explicitly stated
    -- in the official specification, the consensus is that this should be the
    -- total number of bytes downloaded.

    , announceRequestLeft       :: Integer
    -- ^ The number of bytes this client still has to download in base ten ASCII.

    , announceRequestEvent      :: Maybe AnnounceEvent
    -- ^ If specified, must be one of started, completed, stopped,
    -- (or empty which is the same as not being specified). If not specified,
    -- then this request is one performed at regular intervals.

    , announceRequestNumWant    :: Maybe Integer
    -- ^ Optional. Number of peers that the client would like to receive from
    -- the tracker. This value is permitted to be zero. If omitted,
    -- typically defaults to 50 peers.

    , announceRequestNoPeerId   :: Bool
    -- ^ Indicates that the tracker can omit peer id field in announce-list
    -- dictionary. This option is ignored if compact is enabled.

    , announceRequestCompact    :: Bool
    -- ^ Setting this to 1 indicates that the client accepts a compact response.
    -- The announce-list is replaced by a peers string with 6 bytes per peer.
    -- The first four bytes are the host (in network byte order), the last two
    -- bytes are the port (again in network byte order). It should be noted that
    -- some trackers only support compact responses (for saving bandwidth) and
    -- either refuse requests without "compact=1" or simply send a compact
    -- response unless the request contains "compact=0" (in which case they will
    -- refuse the request).

    , announceRequestKey        :: Maybe Text
    -- ^ Optional. An additional client identification mechanism that is not
    -- shared with any peers. It is intended to allow a client to prove their
    -- identity should their IP address change.

    , announceRequestTrackerId  :: Maybe Text
    -- ^ Optional. If a previous announce contained a tracker id,
    -- it should be set here.
    } deriving (Show, Eq, Read, Ord)

-- | The data type that maps announce responses created from
-- 'AnnounceRequest's.
data AnnounceResponse = AnnounceResponse
    { announceResponseInterval        :: Int
    -- ^ Interval in seconds that the client should wait between sending regular
    -- requests to the tracker.

    , announceResponseMinimumInterval :: Maybe Int
    -- ^ Optional. Minimum announce interval. If present clients must not
    -- reannounce more frequently than this.

    , announceResponsePeers           :: [Peer]
    -- ^ A list of peer dictionaries. See 'Peer'.

    , announceResponseFailureReason   :: Maybe Text
    -- ^ Optional. then no other keys may be present. The value is a
    -- human-readable error message as to why the request failed (string).

    , announceResponseWarningMessage  :: Maybe Text
    -- ^ Optional. Similar to failure reason, but the response still gets
    -- processed normally. The warning message is shown just like an error.

    , announceResponseTrackerId       :: Text
    -- ^ A string that the client should send back on its next announcements.

    , announceResponseComplete        :: Integer
    -- ^ Number of peers with the entire file, i.e. seeders.

    , announceResponseIncomplete      :: Integer
    -- ^ Number of non-seeder peers, aka leechers.
    } deriving (Show, Eq, Read, Ord)

instance BEEncodable AnnounceResponse where
    encode AnnounceResponse{} = undefined

data AnnounceEvent
    = AnnounceEventStarted
    -- ^ The first request to the tracker must include the event key with this value.

    | AnnounceEventCompleted
    -- ^ Must be sent to the tracker when the download completes.
    -- However, must not be sent if the download was already 100% complete when
    -- the client started. Presumably, this is to allow the tracker to increment
    -- the "completed downloads" metric based solely on this event.

    | AnnounceEventStopped
    -- ^Must be sent to the tracker if the client is shutting down gracefully.
    deriving (Eq, Read, Ord)

instance Show AnnounceEvent where
    show AnnounceEventStarted = "started"
    show AnnounceEventStopped = "stopped"
    show AnnounceEventCompleted = "completed"

-- | A dictionary that describes the file(s) of the torrent.
-- There are two possible forms:
--
--   * one for the case of a 'single-file' torrent with no directory structure
--
--   * one for the case of a 'multi-file' torrent.
data Info = SingleFileInfo
    { singleFileInfoPiecesLength :: Integer
    -- ^ Number of bytes in each piece.

    , singleFileInfoPieces       :: Text
    -- ^ A string consisting of the concatenation of all 20-byte SHA1 hash
    -- values, one per piece (byte string, i.e. not urlencoded).

    , singleFileInfoPrivate      :: Bool
    -- ^ Optional. If it is set to "1", the client MUST publish its presence to
    -- get other peers ONLY via the trackers explicitly described in the
    -- metainfo file. If this field is set to "0" or is not present,
    -- the client may obtain peer from other means, e.g. PEX peer exchange, dht.
    -- Here, "private" may be read as "no external peer source".

    , sinleFileInfo              :: SingleFileInfoFile
    -- ^ The dictionary describing a single file.
    } | MultiFileInfo
    { multiFileInfoLength  :: Integer
    -- ^ See 'singleFileInfoPiecesLength'.

    , multiFileInfoPieces  :: Text
    -- ^ See 'singleFileInfoPieces'.

    , multiFileInforPivate :: Bool
    -- ^ See 'singleFileInfoPrivate'.

    , multiFileInfoName    :: String
    -- ^ The file path of the directory in which to store all the files.
    -- This is purely advisory.

    , multiFileInfoFiles   :: [MultiFileInfoFile]
    } deriving (Show, Read, Eq, Ord)

data SingleFileInfoFile = SingleFileInfoFile
    { singleFileInfoName   :: FilePath
    -- ^ The filename. This is purely advisory.

    , singleFileInfoMd5Sum :: Maybe Text
    -- ^ Optional. A 32-character hexadecimal string corresponding to the MD5
    -- sum of the file. This is not used by BitTorrent at all, but it is
    -- included by some programs for greater compatibility.

    , singleFileInfoLength :: Integer
    -- ^ Length of the file in bytes.
    } deriving (Show, Eq, Read, Ord)

data MultiFileInfoFile = MultiFileInfoFile
    { multiFileInfoFilePath :: [Text]
    -- ^ A list containing one or more string elements that together represent
    -- the path and filename. Each element in the list corresponds to either a
    -- directory name or (in the case of the final element) the filename.
    -- For example, a the file "dir1/dir2/file.ext" would consist of three string
    -- elements: ["dir1", "dir2", "file.ext"].

    , multiInfoFileLength   :: Integer
    -- ^ The length of the file in bytes.

    , multiInfoFileMd5Sum   :: Maybe Text
    -- ^ See 'singleFileInfoMd5Sum'.
    } deriving (Show, Eq, Read, Ord)

instance BEEncodable Info where
    encode SingleFileInfo{} = undefined
    encode MultiFileInfo{} = undefined

data MetaInfo = MetaInfo
    { metaInfoDictionary   :: Info
    -- ^ See 'Info'.

    , metaInfoAnnounceUrl  :: Text
    -- ^ The announce URL of the tracker.

    , metaInfoAnnounceList :: Maybe [[Text]]
    -- ^ Optional. This is an extention to the official specification,
    -- offering backwards-compatibility.

    , metaInfoCreationDate :: Maybe Text
    -- ^ Optional. The creation time of the torrent, in standard UNIX epoch
    -- format.

    , metaInfoComment      :: Maybe Text
    -- ^ Optional. Free-form textual comments of the author.

    , metaInfoCreatedBy    :: Maybe Text
    -- ^ Optional. Name and version of the program used to create the torrent.

    , metaInfoEncoding     :: Maybe MetaInfoEncoding
    -- ^ Optional. The string encoding format used to generate the pieces part
    -- of the info dictionary in the torrent.
    } deriving (Show, Eq, Read, Ord)

data MetaInfoEncoding = UTF8 deriving (Eq, Show, Read, Ord)

instance BEEncodable MetaInfo where
    encode MetaInfo{} = undefined

data Peer = Peer
    { peerId   :: PeerId
    -- ^ The Peers self-selected ID, as described above for the tracker request.

    , peerIp   :: Text
    -- ^ The peers IP address either IPv6 (hexed) or IPv4 (dotted quad) or
    -- DNS name.

    , peerPort :: Integer
    -- ^ The peers port number.
    
    -- TODO: peer state linking the peer to many torrents (omitted during
    -- serialization)
    } deriving (Show, Eq, Read, Ord)

data ScrapeRequest = ScrapeRequest
    { scrapeRequestInfoHashes :: [InfoHash]
    -- ^ The scrape URL may be supplemented by the optional parameter info_hash,
    -- a 20-byte value as described above.
    -- This restricts the tracker's report to that particular torrent.
    -- Otherwise stats for all torrents that the tracker is managing are
    -- returned.
    } deriving (Show, Eq, Read, Ord)

data ScrapeResponse = ScrapeResponse
    { scrapeResponseFiles :: [ScrapeResponseFile]
    -- ^ A dictionary containing one key/value pair for each torrent for which
    -- there are stats.
    --
    -- If info_hash was supplied and was valid, this dictionary will contain a
    -- single key/value. Each key consists of a 20-byte binary info_hash.
    } deriving (Show, Eq, Read, Ord)

data ScrapeResponseFile = ScrapeResponseFile
    { scrapeResponseFileComplete   :: Integer
    -- ^ The number of peers with the entire file, i.e. seeders.

    , scrapeResponseFileDownloaded :: Integer
    -- ^ The total number of times the tracker has registered a completion
    -- ("event=complete", i.e. a client finished downloading the torrent).

    , scrapeResponseFileIncomplete :: Integer
    -- ^ The number of non-seeder peers, aka leechers.

    , scrapeResponseFileName       :: Text
    -- ^ The torrents internal name, as specified by the "name" file in the
    -- info section of the .torrent file
    } deriving (Show, Eq, Read, Ord)

instance BEEncodable ScrapeResponse where
    encode (ScrapeResponse files) =
        (BDict . M.fromList) [ ("files", BList $ map encode files) ]

instance BEEncodable ScrapeResponseFile where
    encode (ScrapeResponseFile complete downloaded incomplete fileName) =
        (BDict . M.fromList)
            [ ("complete", BInt complete)
            , ("downloaded", BInt downloaded)
            , ("incomplete", BInt incomplete)
            , ("name", BString $ strictTextToLazyByteString fileName)]

data InvalidRequest = InvalidRequest
    { invalidRequestMessage :: Text
    } deriving (Show, Eq, Read, Ord)

data InvalidResponse = InvalidResponse
    { invalidResponseMessage:: Text
    } deriving (Show, Eq, Read, Ord)

instance BEEncodable InvalidResponse where
    encode (InvalidResponse msg) = (BString . BS.fromChunks) [encodeUtf8 msg]

data Torrent = Torrent
    { torrentName           :: Text

    , torrentMetaInfo       :: MetaInfo

    , torrentInfoHash       :: InfoHash
    , torrentFileName       :: Text
    , torrentSize           :: Integer

    , torrentSeederPeerIds  :: [PeerId]
    , torrentLeecherPeerIds :: [PeerId]
    , torrentDownloaded     :: Integer
    , torrentCompleted      :: Integer
    , torrentIncomplete     :: Integer
    } deriving (Show, Eq, Read, Ord)

data TrackerRequest
    = Announce AnnounceRequest
    | Scrape ScrapeRequest
    | Invalid InvalidRequest
    deriving (Show, Eq, Read, Ord)

data Configuration = Configuration
    { cfgTrackerId          :: Text
    , cfgTrackerVersion     :: Text

    , cfgRequestInterval    :: Int
    , cfgMinRequestInterval :: Int
    , cfgRequestTimeout     :: Integer

    , cfgHostName           :: Text
    , cfgPort               :: Int
    , cfgDataDirectory      :: FilePath
    , cfgTorrentDirectory   :: FilePath

    , cfgAnnouncePrefix     :: Text
    , cfgScrapePrefix       :: Text
    } deriving (Show, Eq, Read, Ord)

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
    { cfgTrackerId = "Giraffe"
    , cfgTrackerVersion = "0.1"

    , cfgRequestInterval = 50
    , cfgMinRequestInterval = 50
    , cfgRequestTimeout = 10000

    , cfgHostName = "localhost"
    , cfgPort = 8080
    , cfgDataDirectory = "data/"
    , cfgTorrentDirectory = "data/torrents/"

    , cfgAnnouncePrefix = "/announce"
    , cfgScrapePrefix = "/scrape"
    }

class Tracker a where
    trackerConfiguration :: a -> Configuration

    handleAnnounceRequest :: a -> AnnounceRequest -> IO AnnounceResponse
    handleScrapeRequest :: a -> ScrapeRequest -> IO ScrapeResponse
    handleInvalidRequest :: a -> InvalidRequest -> IO InvalidResponse

    initTracker :: Configuration -> IO a
    stopTracker :: a -> IO ()

class BEEncodable a where
    encode :: a -> BEncode

class BEDecodable a where
    decode :: BEncode -> a

type InfoHash = Text

type PeerId = Text
