{-# LANGUAGE DeriveGeneric #-}

module Root where

import Data.Aeson
import GHC.Generics (Generic)
import Account
import Link
import Mailbox

data Root = Root { csrfToken :: String, primaryAccount :: Account, mailbox :: [Mailbox], link :: [Link] } deriving (Show, Generic)

instance FromJSON Root
instance ToJSON Root