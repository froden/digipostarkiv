{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, FlexibleContexts #-}

module Folder where

import Data.Aeson
import GHC.Generics (Generic)
import Data.List

import Document
import Link

data Folders = Folders { folder :: [Folder] } deriving (Show, Generic)
data Folder = Folder { name :: String, icon :: String, link :: [Link], documents :: Maybe Documents } deriving (Show, Generic)

instance FromJSON Folder
instance ToJSON Folder

instance FromJSON Folders
instance ToJSON Folders

foldersNotInList :: [FilePath] -> [Folder] -> [Folder]
foldersNotInList localFolders = filter (\f -> name f `notElem` localFolders)

foldersNotInRemoteList :: [FilePath] -> [Folder] -> [FilePath]
foldersNotInRemoteList localFolders = (localFolders \\) . map name
