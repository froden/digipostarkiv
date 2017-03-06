{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Oauth where

import qualified Data.ByteString                 as BS
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import Control.Exception.Lifted
import System.FilePath.Posix
import System.Directory
import Text.Read
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Control.Monad.Trans.Resource
import Control.Monad.Except

import Network.OAuth.OAuth2

import Error
import qualified Http as HTTP
import DigipostKey


type URL = BS.ByteString

newtype AuthCode = AuthCode String deriving (Eq, Show)
newtype State = State String

loginUrl :: State -> URL
loginUrl (State state) = authorizationUrl digigpostKey `appendQueryParam` [("state", sToBS state)]

accessToken :: State -> AuthCode -> Manager -> ResourceT IO HTTP.AccessToken
accessToken (State state) (AuthCode code) manager = do
    let (url, body) = accessTokenUrl' digigpostKey (sToBS code) (Just "code")
    token <- liftIO $ doJSONPostRequest manager digigpostKey url (body ++ [("state", sToBS state)])
    case token of
        Right (AccessToken at (Just rt) _ _ _) -> return $ HTTP.AccessToken at rt
        Right _ -> throwIO NotAuthenticated
        Left _ -> throwIO NotAuthenticated

refreshAccessToken :: Manager -> HTTP.AccessToken -> IO HTTP.AccessToken
refreshAccessToken manager oldToken = do
    putStrLn $ "trying to refresh token " ++ show oldToken
    let oldRt = HTTP.refreshToken oldToken
    newToken <- fetchRefreshToken manager digigpostKey oldRt
    case newToken of
        Right (AccessToken at _ _ _ _) -> return $ HTTP.AccessToken at oldRt
        Left _ -> throwIO NotAuthenticated

storeAccessToken :: HTTP.AccessToken -> IO ()
storeAccessToken at = do
    userHome <- getHomeDirectory
    writeFile (accessTokenFile userHome) $ show at

loadAccessToken :: IO HTTP.AccessToken
loadAccessToken = catch readFileIfExists whenNotFound
    where
        readFileIfExists = do
            userHome <- getHomeDirectory
            content <- readFile (accessTokenFile userHome)
            case readMaybe content of
              Just at -> return at
              Nothing -> throwIO NotAuthenticated
        whenNotFound :: IOException -> IO HTTP.AccessToken
        whenNotFound _ = throwIO NotAuthenticated

handleTokenRefresh :: (Manager -> HTTP.AccessToken -> ResourceT IO a) -> HTTP.AccessToken -> Manager -> ResourceT IO a
handleTokenRefresh accessFunc token manager = catch (accessFunc manager token) handleException
    where
        handleException e@(HttpExceptionRequest _ (StatusCodeException response _)) =
            let
                status = responseStatus response
            in
                if status == status403 then
                    do
                        newToken <- liftIO $ refreshAccessToken manager token --TODO: exceptions?
                        liftIO $ storeAccessToken newToken
                        accessFunc manager newToken  --TODO: retry count??
                else if status == status401 then throw NotAuthenticated
                else throw $ HttpFailed e
        handleException e = throw $ HttpFailed e

removeAccessToken :: IO ()
removeAccessToken = getHomeDirectory >>= removeFile . accessTokenFile

accessTokenFile :: FilePath -> FilePath
accessTokenFile userHome = combine userHome ".digipostarkiv"

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
