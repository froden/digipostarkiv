
module Main where

import Sync
-- import Control.Concurrent

main :: IO ()
main = loop


loop :: IO ()
loop = do
    putStrLn "sync"
    sync
    --threadDelay 10000000
    --loop