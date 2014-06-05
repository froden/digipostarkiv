{-# LANGUAGE OverloadedStrings #-}

module Http where

import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Data.ByteString

type Session = AccessToken

digipostV2Type :: ByteString
digipostV2Type = "application/vnd.digipost-v2+json"

contentTypeDigipost :: Header
contentTypeDigipost = ("Content-Type", digipostV2Type)

acceptDigipost :: Header
acceptDigipost = ("Accept", digipostV2Type)

setBody :: RequestBody -> Request -> Request
setBody body req = req { requestBody = body }

setCookies :: CookieJar -> Request -> Request
setCookies cookies req = req { cookieJar = Just cookies}

setMethod :: Method -> Request -> Request
setMethod m req = req { method = m }

addHeader :: Header -> Request -> Request
addHeader hdr req = req { requestHeaders = hdr : requestHeaders req}

addHeaders :: [Header] -> Request -> Request
addHeaders hdrs req = req { requestHeaders = hdrs ++ requestHeaders req}

setSession :: Session -> Request -> Request
setSession = addAccessToken

addAccessToken :: AccessToken -> Request -> Request
addAccessToken token = addHeader $ accessTokenHeader token

data AccessToken = AccessToken { accessToken :: ByteString, refreshToken :: ByteString } deriving (Show, Read)

accessTokenHeader :: AccessToken -> Header
accessTokenHeader token = ("Authorization", hdrValue)
	where hdrValue = "Bearer " `append` accessToken token
