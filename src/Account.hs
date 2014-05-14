{-# LANGUAGE DeriveGeneric #-}

module Account where

import Data.Aeson
import GHC.Generics (Generic)
import Link

data Account = Account { fullName :: String, link :: [Link] } deriving (Show, Generic)

instance FromJSON Account
instance ToJSON Account