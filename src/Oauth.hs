{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Oauth where

import qualified Data.ByteString                 as BS
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import Control.Exception
import System.FilePath.Posix
import System.Directory
import Text.Read

import Network.OAuth.OAuth2

import Error
import qualified Http as HTTP


type URL = BS.ByteString

newtype AuthCode = AuthCode String deriving (Eq, Show)
newtype State = State String

loginUrl :: State -> URL
loginUrl (State state) = authorizationUrl digigpostKey `appendQueryParam` [("state", sToBS state)]

accessToken :: State -> AuthCode -> IO HTTP.AccessToken
accessToken (State state) (AuthCode code) = do
    let (url, body) = accessTokenUrl' digigpostKey (sToBS code) (Just "code")
    token <- doJSONPostRequest digigpostKey url (body ++ [("state", sToBS state)])
    case token of
        Right (AccessToken at (Just rt)) -> return $ HTTP.AccessToken at rt
        Right _ -> throwIO NotAuthenticated
        Left _ -> throwIO NotAuthenticated

refreshAccessToken :: HTTP.AccessToken -> IO HTTP.AccessToken
refreshAccessToken oldToken = do
    putStrLn $ "trying to refresh token " ++ show oldToken
    let oldRt = HTTP.refreshToken oldToken
    newToken <- fetchRefreshToken digigpostKey oldRt
    case newToken of
        Right (AccessToken at _) -> return $ HTTP.AccessToken at oldRt
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

removeAccessToken :: IO ()
removeAccessToken = getHomeDirectory >>= removeFile . accessTokenFile

accessTokenFile :: FilePath -> FilePath
accessTokenFile userHome = combine userHome ".digipostarkiv"

digigpostKey :: OAuth2
digigpostKey = OAuth2 { oauthClientId = "5f2b683e74a84b38aea54ee9d0080276"
                      , oauthClientSecret = "gceQSc6Gw5g4IaJjg2pWAkNmxQcydkfOY3kHH59t1gQ"
                      , oauthCallback = Just "digipost://oauth"
                      , oauthOAuthorizeEndpoint = "https://www.digipost.no/post/api/oauth/authorize/new"
                      , oauthAccessTokenEndpoint = "https://www.digipost.no/post/api/oauth/accesstoken"
                      }

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
