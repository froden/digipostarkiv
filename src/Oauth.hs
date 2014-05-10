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

import Api
import qualified Http as HTTP


type URL = BS.ByteString

newtype AuthCode = AuthCode String deriving (Eq, Show)
newtype State = State String

loginUrl :: State -> URL
loginUrl (State state) = authorizationUrl digigpostKey `appendQueryParam` [("state", sToBS state)]

accessToken :: State -> AuthCode -> IO (SyncResult HTTP.AccessToken)
accessToken (State state) (AuthCode code) = do
    let (url, body, credentials) = accessTokenUrlDigipost digigpostKey (sToBS code)
    token <- doJSONPostRequest' url (body ++ [("state", sToBS state)]) (Just credentials)
    case token of
      Right (AccessToken at (Just rt)) -> return $ Right $ HTTP.AccessToken at rt
      Right _ -> return $ Left NotAuthenticated
      Left _ -> return $ Left NotAuthenticated

storeAccessToken :: HTTP.AccessToken -> IO ()
storeAccessToken at = do
    userHome <- getHomeDirectory
    writeFile (accessTokenFile userHome) $ show at

loadAccessToken :: IO (Maybe HTTP.AccessToken)
loadAccessToken = catch readFileIfExists whenNotFound
  where 
    readFileIfExists = do 
      userHome <- getHomeDirectory
      content <- readFile (accessTokenFile userHome) 
      return $ readMaybe content
    whenNotFound :: IOException -> IO (Maybe HTTP.AccessToken)
    whenNotFound _ = return Nothing

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


--due to bug in digipost
accessTokenUrlDigipost :: OAuth2
                  -> BS.ByteString
                  -> (URI, PostBody, (BS.ByteString, BS.ByteString))
accessTokenUrlDigipost oa code = case accessTokenUrl' oa code (Just "code") of
      (uri, body) -> (uri, body, credentials)
      where credentials = (oauthClientId oa, oauthClientSecret oa)


sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
