module Giraffe.Data.Peer (
  Peer(..)
) where

data Peer = Peer {
  -- | The Peers self-selected ID, as described above for the tracker request.
  peerId :: String,
  
  -- | The peers IP address either IPv6 (hexed) or IPv4 (dotted quad) or 
  -- DNS name.
  peerIp :: String,
  
  -- | The peers port number.
  peerPort :: Int
}