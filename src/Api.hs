{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses, DeriveDataTypeable, OverloadedStrings, FlexibleContexts #-}

module Api where

import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData
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
import qualified Config as C

data ApiException = JsonParseException L.ByteString | AuthFailedException deriving (Typeable)

instance Exception ApiException

instance Show ApiException where
    show (JsonParseException cause) = "Failed to parse json response: " ++ unpack (L.toStrict cause)
    show AuthFailedException = "Wrong username or password"

data Auth = Auth { username :: String, password :: String } deriving (Show, Generic)

instance FromJSON Auth
instance ToJSON Auth

digipostBaseUrl :: String
digipostBaseUrl = "https://www.digipost.no/post/api"

decodeOrThrow :: FromJSON a => L.ByteString -> ResourceT IO a
decodeOrThrow json = case decode json of
    Nothing -> liftIO $ throwIO $ JsonParseException json
    Just decoded -> return decoded

getAccount :: Manager -> CookieJar -> ResourceT IO (Root, A.Account)
getAccount manager cookies = do
    root <- getRoot manager $ Just cookies
    return (root, primaryAccount root)

getDocs :: Manager -> CookieJar -> Link -> ResourceT IO [D.Document]
getDocs manager cookies docsLink = do
    allDocuments <- getDocuments (Just cookies) manager docsLink
    --we only want documents uploaded by the user
    return $ filter D.uploaded (D.document allDocuments)    

authFromConfig :: C.Config -> Auth
authFromConfig conf = Auth (C.username conf) (C.password conf)    

authenticate :: Manager -> Auth -> ResourceT IO CookieJar
authenticate manager auth = do 
    authReq <- authRequest (digipostBaseUrl ++ "/private/passwordauth") auth
    authRes <- httpLbs authReq manager
    return $ responseCookieJar authRes

authRequest :: (Failure HttpException m, Functor m) => String -> Auth -> m (Request m')
authRequest url auth = setBody body . addHeaders headers . setMethod "POST" <$> parseUrl url
    where headers = [contentTypeDigipost, acceptDigipost]
          body = RequestBodyLBS $ encode auth

getRequest :: (Failure HttpException m, Functor m) => Maybe CookieJar -> String -> m (Request m')
getRequest cookies url = addHeader acceptDigipost <$> setCookies cookies <$> parseUrl url

getRoot :: Manager -> Maybe CookieJar -> ResourceT IO Root
getRoot manager cookies = getRequest cookies digipostBaseUrl >>= getJson manager

getDocuments :: Maybe CookieJar -> Manager -> Link -> ResourceT IO D.Documents
getDocuments cookies manager docsLink = getRequest cookies (uri docsLink) >>= getJson manager

getJson :: (FromJSON a) => Manager -> Request (ResourceT IO) -> ResourceT IO a 
getJson manager req = httpLbs req manager >>= decodeOrThrow . responseBody

downloadDocument :: Maybe CookieJar -> Manager -> FilePath -> D.Document -> ResourceT IO ()
downloadDocument cookies manager syncDir document = do
    req <- downloadDocRequest cookies document
    res <- http req manager
    responseBody res $$+- sinkFile targetFile
    where targetFile = combine syncDir $ D.filename document

downloadDocRequest :: (Failure HttpException m, Functor m) => Maybe CookieJar -> D.Document -> m (Request m')
downloadDocRequest cookies document = linkM >>= requestFromLink
    where linkM = linkWithRelM "document_content" links 
          requestFromLink = getRequest cookies . uri
          links = D.link document

downloadAll :: Maybe CookieJar -> Manager -> FilePath -> [D.Document] -> ResourceT IO [()]
downloadAll cookies manager syncDir = mapM download
    where download = downloadDocument cookies manager syncDir

uploadAll :: Maybe CookieJar -> Manager -> Link -> String -> [FilePath] -> ResourceT IO [()]
uploadAll cookies manager uploadLink token = mapM upload
    where upload = uploadFileMultipart cookies manager uploadLink token

uploadFileMultipart :: Maybe CookieJar -> Manager -> Link -> String -> FilePath -> ResourceT IO ()
uploadFileMultipart cookies manager uploadLink token file = do
    req <- addHeader ("Accept", "*/*") <$> setCookies cookies <$> parseUrl (uri uploadLink)
    multipartReq <- formDataBody [partBS "subject" (UTF8.fromString $ takeBaseName file),
                                  partBS "token" (pack token),
                                  partFileSource "file" file] req
    http multipartReq manager
    --check that responsebody is "OK"
    --responseBody res $$+- sinkFile "temp.txt"
    return ()

