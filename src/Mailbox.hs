{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, FlexibleContexts #-}

module Mailbox where

import Data.Aeson
import GHC.Generics (Generic)

import Folder
import Link

data Mailbox = Mailbox { folders :: Folders, link :: [Link] } deriving (Show, Generic)

instance FromJSON Mailbox
instance ToJSON Mailbox