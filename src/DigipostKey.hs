{-# LANGUAGE OverloadedStrings #-}

module DigipostKey where

import Network.OAuth.OAuth2

digigpostKey :: OAuth2
digigpostKey = OAuth2 { oauthClientId = ""
                      , oauthClientSecret = ""
                      , oauthCallback = Just ""
                      , oauthOAuthorizeEndpoint = ""
                      , oauthAccessTokenEndpoint = ""
                      }