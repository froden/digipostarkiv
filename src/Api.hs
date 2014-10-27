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
import qualified ApiTypes as DP


data ApiException = JsonParseException L.ByteString | AuthFailedException | NoLinkFoundException String | Unknown deriving (Typeable)

instance Exception ApiException

instance Show ApiException where
    show (JsonParseException cause) = "Failed to parse json response: " ++ unpack (L.toStrict cause)
    show AuthFailedException = "Wrong username or password"
    show (NoLinkFoundException relation) = "No link found with relation: " ++ relation
    show Unknown = "Unknown error"

data Auth = Auth { username :: String, password :: String } deriving (Show, Generic)

instance FromJSON Auth
instance ToJSON Auth

digipostBaseUrl :: String
digipostBaseUrl = "https://www.digipost.no/post/api"

linkOrException :: String -> [DP.Link] -> IO DP.Link
linkOrException relation links =
    case DP.linkWithRel relation links of
        Just l -> return l
        Nothing -> throwIO $ NoLinkFoundException relation

decodeOrException :: FromJSON a => L.ByteString -> ResourceT IO a
decodeOrException json = case decode json of
    Nothing -> liftIO $ throwIO $ JsonParseException json
    Just decoded -> return decoded

getAccount :: Manager -> Session -> ResourceT IO (DP.Root, DP.Account, DP.Mailbox)
getAccount manager session = do
    root <- getRoot manager session
    return (root, DP.primaryAccount root, head $ DP.mailbox root)

getDocs :: Manager -> Session -> DP.Link -> ResourceT IO [DP.Document]
getDocs manager session docsLink = do
    allDocuments <- getDocuments session manager docsLink
    --we only want documents uploaded by the user
    return $ filter DP.uploaded (DP.document allDocuments)

authenticatePwd :: Manager -> Auth -> ResourceT IO CookieJar
authenticatePwd manager auth = do
    authReq <- authRequest (digipostBaseUrl ++ "/private/passwordauth") auth
    authRes <- httpLbs authReq manager
    return $ responseCookieJar authRes

authRequest :: String -> Auth -> ResourceT IO Request
authRequest url auth = setBody body . addHeaders headers . setMethod "POST" <$> parseUrl url
    where headers = [contentTypeDigipost, acceptDigipost]
          body = RequestBodyLBS $ encode auth

getRequest :: (Failure HttpException m, Functor m) => Session -> String -> m Request
getRequest session url = addHeader acceptDigipost <$> setSession session <$> parseUrl url

getRoot :: Manager -> Session -> ResourceT IO DP.Root
getRoot manager session = getRequest session digipostBaseUrl >>= getJson manager

getDocuments :: Session -> Manager -> DP.Link -> ResourceT IO DP.Documents
getDocuments session manager docsLink = getRequest session (DP.uri docsLink) >>= getJson manager

getFolder :: Session -> Manager -> DP.Link -> ResourceT IO DP.Folder
getFolder session manager folderLink = getRequest session (DP.uri folderLink) >>= getJson manager

getMailbox :: Session -> Manager -> DP.Link -> ResourceT IO DP.Mailbox
getMailbox session manager mboxLink = getRequest session (DP.uri mboxLink) >>= getJson manager

getJson :: (FromJSON a) => Manager -> Request -> ResourceT IO a
getJson manager req = httpLbs req manager >>= (decodeOrException . responseBody)

downloadDocument :: Session -> Manager -> FilePath -> DP.Document -> ResourceT IO ()
downloadDocument session manager targetFile document = do
    req <- downloadDocRequest session document
    res <- http req manager
    responseBody res $$+- sinkFile targetFile

downloadDocRequest :: Session -> DP.Document -> ResourceT IO Request
downloadDocRequest session document = contentLink >>= requestFromLink
    where contentLink = liftIO $ linkOrException "document_content" links
          requestFromLink = getRequest session . DP.uri
          links = DP.documentLinks document

downloadAll :: Session -> Manager -> FilePath -> [DP.Document] -> ResourceT IO ()
downloadAll session manager syncDir = void . mapM download
    where download = downloadDocument session manager syncDir

uploadAll :: Session -> Manager -> DP.Link -> String -> [FilePath] -> ResourceT IO ()
uploadAll session manager uploadLink token = void . mapM upload
    where upload = uploadFileMultipart session manager uploadLink token

uploadFileMultipart :: Session -> Manager -> DP.Link -> String -> FilePath -> ResourceT IO ()
uploadFileMultipart session manager uploadLink token file = do
    req <- addHeader ("Accept", "*/*") <$> setSession session <$> parseUrl (DP.uri uploadLink)
    let convertSpecialChars = map (\c -> if c == ':' then '/' else c)
    let subject = UTF8.fromString . convertSpecialChars . takeBaseName $ file
    multipartReq <- formDataBody [partBS "subject" subject,
                                  partBS "token" (pack token),
                                  partFileSource "file" file] req
    _ <- http multipartReq manager
    --check that responsebody is "OK"
    --responseBody res $$+- sinkFile "temp.txt"
    return ()

createFolder :: Session -> Manager -> DP.Link -> String -> String -> ResourceT IO DP.Folder
createFolder session manager createLink csrf folderName = do
    let jsonBody = encode (DP.Folder folderName "FOLDER" [] Nothing)
    let body = RequestBodyLBS jsonBody
    req <- setBody body <$>
           addHeaders [acceptDigipost, contentTypeDigipost, ("X-CSRFToken", pack csrf)] <$>
           setSession session <$>
           setMethod "POST" <$>
           parseUrl (DP.uri createLink)
    httpLbs req manager >>= (decodeOrException . responseBody)
