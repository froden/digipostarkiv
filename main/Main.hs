
module Main where

import Sync
-- import Control.Concurrent

main :: IO ()
main = loop


loop :: IO ()
loop = do
    initLogging
    sync
    --threadDelay 10000000
    --loop