{-# LANGUAGE OverloadedStrings #-}

module Http where

import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Data.ByteString

digipostV2Type :: ByteString
digipostV2Type = "application/vnd.digipost-v2+json"

contentTypeDigipost :: Header
contentTypeDigipost = ("Content-Type", digipostV2Type)

acceptDigipost :: Header
acceptDigipost = ("Accept", digipostV2Type)

setBody :: RequestBody m -> Request m -> Request m
setBody body req = req { requestBody = body }

setCookies :: Maybe CookieJar -> Request m -> Request m
setCookies cookies req = req { cookieJar = cookies}

setMethod :: Method -> Request m -> Request m
setMethod m req = req { method = m }

addHeader :: Header -> Request m -> Request m
addHeader hdr req = req { requestHeaders = hdr : requestHeaders req}

addHeaders :: [Header] -> Request m -> Request m
addHeaders hdrs req = req { requestHeaders = hdrs ++ requestHeaders req}