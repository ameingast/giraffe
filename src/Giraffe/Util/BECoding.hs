module Giraffe.Util.BECoding (
  BEEncodable(..),
  BEDecodable(..)
) where
  
import Data.BEncode(BEncode)
  
class BEEncodable a where
  encode :: a -> BEncode
  
class BEDecodable a where
  decode :: BEncode -> a