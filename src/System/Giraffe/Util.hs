module System.Giraffe.Util where

import           Control.Concurrent.MVar
import           Control.Exception       as E (IOException, catch)
import           Control.Monad           (liftM)
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE

-- | Read a file located at 'path' into a string.
-- If an exception occurs, Nothing is returned and the error is
-- printed to stdout.
readFileSafe :: FilePath -> IO (Maybe String)
readFileSafe path =
    liftM Just (readFile path) `E.catch` handler
    where
        handler :: E.IOException -> IO (Maybe String)
        handler e = print e >> return Nothing

fromMaybe :: Maybe a -> a -> a
fromMaybe (Just x) _ = x
fromMaybe Nothing y = y

readSafe :: Read a => String -> a -> a
readSafe s d =
    case reads s of
        [(x, _)] -> x
        _ -> d

writeFromMVarOntoDisk :: Show a => FilePath -> MVar a -> IO ()
writeFromMVarOntoDisk path var = readMVar var >>= writeFile path . show

loadFromDiskIntoMVar :: Read a => FilePath -> a -> IO (MVar a)
loadFromDiskIntoMVar path def =
    readFileSafe path >>= \c -> case c of
        Nothing ->
            newMVar def
        Just contents ->
            newMVar (readSafe contents def)

dropNothing :: [Maybe a] -> [a]
dropNothing xxs = dropNothing' xxs []
    where
        dropNothing' [] ys = ys
        dropNothing' (Nothing:xs) ys = dropNothing' xs ys
        dropNothing' (Just x:xs) ys = dropNothing' xs (x:ys)

strictTextToLazyByteString :: Text -> LBS.ByteString
strictTextToLazyByteString = TLE.encodeUtf8 . TL.fromStrict
