module Main (main) where

import           System.Giraffe.ApplicationTest
import           Test.Framework                 (defaultMain)

main :: IO ()
main = defaultMain [applicationTest]
