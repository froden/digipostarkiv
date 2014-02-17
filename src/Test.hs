{-# LANGUAGE OverloadedStrings #-}

module Test where


import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData

import Control.Monad    

test :: IO ()
test = withManager $ \m -> do
    let file = "æøå.txt"
    flip httpLbs m =<<
        (formDataBody [partBS "subject" file
            ,partFileSource "file" "sync/ææøø.txt"]
            $ fromJust $ parseUrl "http://localhost:8888")