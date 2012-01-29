module Main (
  main
) where

import Giraffe.Configuration(defaultConfiguration, cfgPort)
import Giraffe.RequestRunner(runRequest)

import Network.Wai.Handler.Warp(run)

main :: IO ()
main =
  let port = cfgPort defaultConfiguration
  in run port runRequest