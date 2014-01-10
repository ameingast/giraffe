module Main (main) where

import           Network.Wai.Handler.Warp   (run)
import           System.Giraffe.Application (handle)
import           System.Giraffe.Tracker
import           System.Giraffe.Types

main :: IO ()
main = do
    let handler = undefined :: InMemoryRequestHandler
    config <- defaultConfiguration
    run (cfgPort config) (handle config handler)
