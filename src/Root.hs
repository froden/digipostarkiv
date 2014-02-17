{-# LANGUAGE DeriveGeneric #-}

module Root where

import Data.Aeson
import GHC.Generics (Generic)
import Account
import Link

data Root = Root { csrfToken :: String, primaryAccount :: Account, link :: [Link] } deriving (Show, Generic)

instance FromJSON Root
instance ToJSON Root