{-# LANGUAGE OverloadedStrings #-}

module DigipostKey where

import Network.OAuth.OAuth2

digigpostKey :: OAuth2
digigpostKey = OAuth2 { oauthClientId = "5f2b683e74a84b38aea54ee9d0080276"
                      , oauthClientSecret = "GfXhs9LtkdVuM4Dvyk7Ddhu1x9-oSkI6YQJIn7elf44"
                      , oauthCallback = Just "digipost://oauth"
                      , oauthOAuthorizeEndpoint = "https://www.digipost.no/post/api/oauth/authorize/new"
                      , oauthAccessTokenEndpoint = "https://www.digipost.no/post/api/oauth/accesstoken"
                      }