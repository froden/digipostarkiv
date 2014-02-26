{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, DeriveDataTypeable, OverloadedStrings, FlexibleContexts #-}

module Api where

import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData
import Network.HTTP.Types.Header
import Network.HTTP.Types
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as L
import Control.Monad.Error
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Control.Monad.Trans.Resource
import Control.Failure
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Control.Exception
import Data.Typeable
import System.FilePath.Posix
import Control.Applicative

import Link
import Root
import qualified Document as D

data JsonParseException = JsonParseException L.ByteString deriving (Show, Typeable)

instance Exception JsonParseException

data Auth = Auth { username :: String, password :: String } deriving (Show, Generic)

instance FromJSON Auth
instance ToJSON Auth

digipostBaseUrl :: String
digipostBaseUrl = "https://www.digipost.no/post/api"

decodeOrThrow :: FromJSON a => L.ByteString -> ResourceT IO a
decodeOrThrow json = case decode json of
    Nothing -> liftIO $ throwIO $ JsonParseException json
    Just decoded -> return decoded

digipostV2Type :: B.ByteString
digipostV2Type = "application/vnd.digipost-v2+json"

authenticate :: Manager -> Auth -> ResourceT IO (Response L.ByteString)
authenticate manager auth = case auth2 (digipostBaseUrl ++ "/private/passwordauth") auth of
    Nothing -> error "Failed to create auth request"
    Just req -> httpLbs req manager

auth2 :: String -> Auth -> Maybe (Request m)
auth2 url auth = setBody body <$> addHeaders headers <$> reqMethod "POST" <$> parseUrl url
    where headers = [contentTypeDigipost, acceptDigipost]
          body = RequestBodyLBS $ encode auth

contentTypeDigipost :: Header
contentTypeDigipost = ("Content-Type", digipostV2Type)

acceptDigipost :: Header
acceptDigipost = ("Accept", digipostV2Type)

setBody :: RequestBody m -> Request m -> Request m
setBody body req = req { requestBody = body }

setCookies :: Maybe CookieJar -> Request m -> Request m
setCookies cookies req = req { cookieJar = cookies}

reqMethod :: Method -> Request m -> Request m
reqMethod m req = req { method = m }

addHeader :: Header -> Request m -> Request m
addHeader hdr req = req { requestHeaders = hdr : requestHeaders req}

addHeaders :: [Header] -> Request m -> Request m
addHeaders hdrs req = req { requestHeaders = hdrs ++ requestHeaders req}

getRoot :: Manager -> Maybe CookieJar -> ResourceT IO Root
getRoot manager cookies = do 
    req <- getRequest cookies digipostBaseUrl
    res <- httpLbs req manager
    decodeOrThrow $ responseBody res

getDocuments :: Maybe CookieJar -> Manager -> Link -> ResourceT IO D.Documents
getDocuments cookies manager docsLink = do
    req <- getRequest cookies $ uri docsLink
    res <- httpLbs req manager
    decodeOrThrow $ responseBody res

getRequest :: (Failure HttpException m, Functor m) => Maybe CookieJar -> String -> m (Request m')
getRequest cookies url = addHeader acceptDigipost <$> setCookies cookies <$> parseUrl url

downloadDocument :: Maybe CookieJar -> Manager -> FilePath -> D.Document -> ResourceT IO ()
downloadDocument cookies manager syncDir document = do
    let Just contentLink = linkWithRel "document_content" (D.link document)
    let targetFile = combine syncDir $ D.filename document
    req <- getRequest cookies $ uri contentLink
    res <- http req manager
    responseBody res $$+- sinkFile targetFile

downloadAll :: Maybe CookieJar -> Manager -> FilePath -> [D.Document] -> ResourceT IO [()]
downloadAll cookies manager syncDir = mapM download
    where download = downloadDocument cookies manager syncDir

uploadAll :: Maybe CookieJar -> Manager -> Link -> String -> [FilePath] -> ResourceT IO [()]
uploadAll cookies manager uploadLink token = mapM upload
    where upload = uploadFileMultipart cookies manager uploadLink token

uploadFileMultipart :: Maybe CookieJar -> Manager -> Link -> String -> FilePath -> ResourceT IO ()
uploadFileMultipart cookies manager uploadLink token file = do
    req <- parseUrl $ uri uploadLink
    let req2 = req { requestHeaders = [("Accept", "*/*")], cookieJar = cookies }
    multipartReq <- formDataBody [partBS "subject" (UTF8.fromString $ takeBaseName file),
                                  partBS "token" (pack token),
                                  partFileSource "file" file] req2
    res <- http multipartReq manager
    responseBody res $$+- sinkFile "temp.txt"
    return ()

