{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, DeriveDataTypeable, OverloadedStrings, FlexibleContexts #-}

module Api where

import Network.HTTP.Conduit
import Network.HTTP.Client.MultipartFormData
import Data.ByteString.Char8 (pack, unpack)
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

import Http
import Link
import Root
import qualified Document as D
import qualified Account as A

type SyncResult a = Either SyncError a

data SyncError = NotAuthenticated | HttpFailed HttpException deriving (Show)

instance Error SyncError

data ApiException = JsonParseException L.ByteString | AuthFailedException | Unknown deriving (Typeable)

instance Exception ApiException

instance Show ApiException where
    show (JsonParseException cause) = "Failed to parse json response: " ++ unpack (L.toStrict cause)
    show AuthFailedException = "Wrong username or password"
    show Unknown = "Unknown error"

data Auth = Auth { username :: String, password :: String } deriving (Show, Generic)

instance FromJSON Auth
instance ToJSON Auth

digipostBaseUrl :: String
digipostBaseUrl = "https://www.digipost.no/post/api"

decodeOrThrow :: FromJSON a => L.ByteString -> ResourceT IO a
decodeOrThrow json = case decode json of
    Nothing -> liftIO $ throwIO $ JsonParseException json
    Just decoded -> return decoded

getAccount :: Manager -> Session -> ResourceT IO (Root, A.Account)
getAccount manager session = do
    root <- getRoot manager session
    return (root, primaryAccount root)

getDocs :: Manager -> Session -> Link -> ResourceT IO [D.Document]
getDocs manager session docsLink = do
    allDocuments <- getDocuments session manager docsLink
    --we only want documents uploaded by the user
    return $ filter D.uploaded (D.document allDocuments)

authenticatePwd :: Manager -> Auth -> ResourceT IO CookieJar
authenticatePwd manager auth = do 
    authReq <- authRequest (digipostBaseUrl ++ "/private/passwordauth") auth
    authRes <- httpLbs authReq manager
    return $ responseCookieJar authRes

authRequest :: (Failure HttpException m, Functor m) => String -> Auth -> m Request
authRequest url auth = setBody body . addHeaders headers . setMethod "POST" <$> parseUrl url
    where headers = [contentTypeDigipost, acceptDigipost]
          body = RequestBodyLBS $ encode auth

getRequest :: (Failure HttpException m, Functor m) => Session -> String -> m Request
getRequest session url = addHeader acceptDigipost <$> setSession session <$> parseUrl url

getRoot :: Manager -> Session -> ResourceT IO Root
getRoot manager session = getRequest session digipostBaseUrl >>= getJson manager

getDocuments :: Session -> Manager -> Link -> ResourceT IO D.Documents
getDocuments session manager docsLink = getRequest session (uri docsLink) >>= getJson manager

getJson :: (FromJSON a) => Manager -> Request -> ResourceT IO a 
getJson manager req = httpLbs req manager >>= (decodeOrThrow . responseBody)

downloadDocument :: Session -> Manager -> FilePath -> D.Document -> ResourceT IO ()
downloadDocument session manager syncDir document = do
    req <- downloadDocRequest session document
    res <- http req manager
    responseBody res $$+- sinkFile targetFile
    where targetFile = combine syncDir $ D.filename document

downloadDocRequest :: (Failure HttpException m, Functor m) => Session -> D.Document -> m Request
downloadDocRequest session document = linkM >>= requestFromLink
    where linkM = linkWithRelM "document_content" links 
          requestFromLink = getRequest session . uri
          links = D.link document

downloadAll :: Session -> Manager -> FilePath -> [D.Document] -> ResourceT IO ()
downloadAll session manager syncDir = void . mapM download
    where download = downloadDocument session manager syncDir

uploadAll :: Session -> Manager -> Link -> String -> [FilePath] -> ResourceT IO ()
uploadAll session manager uploadLink token = void . mapM upload
    where upload = uploadFileMultipart session manager uploadLink token

uploadFileMultipart :: Session -> Manager -> Link -> String -> FilePath -> ResourceT IO ()
uploadFileMultipart session manager uploadLink token file = do
    req <- addHeader ("Accept", "*/*") <$> setSession session <$> parseUrl (uri uploadLink)
    multipartReq <- formDataBody [partBS "subject" (UTF8.fromString $ takeBaseName file),
                                  partBS "token" (pack token),
                                  partFileSource "file" file] req
    _ <- http multipartReq manager
    --check that responsebody is "OK"
    --responseBody res $$+- sinkFile "temp.txt"
    return ()


