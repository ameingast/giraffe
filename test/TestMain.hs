module Main (main) where

import           System.Giraffe.ApplicationTest (applicationTest)
import           System.Giraffe.TrackerTest     (trackerTest)
import           Test.Framework                 (defaultMain)

main :: IO ()
main = defaultMain
    [ applicationTest
    , trackerTest
    ]
