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

setBody :: RequestBody -> Request -> Request
setBody body req = req { requestBody = body }

setCookies :: Maybe CookieJar -> Request -> Request
setCookies cookies req = req { cookieJar = cookies}

setMethod :: Method -> Request -> Request
setMethod m req = req { method = m }

addHeader :: Header -> Request -> Request
addHeader hdr req = req { requestHeaders = hdr : requestHeaders req}

addHeaders :: [Header] -> Request -> Request
addHeaders hdrs req = req { requestHeaders = hdrs ++ requestHeaders req}