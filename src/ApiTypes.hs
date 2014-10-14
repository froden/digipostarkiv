{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module ApiTypes where

import Data.Aeson
import Control.Applicative
import Control.Monad
import GHC.Generics (Generic)
import Data.List
import Data.Char

data Link = Link { rel :: String, uri :: String } deriving (Show, Read, Generic)
data Root = Root { csrfToken :: String, primaryAccount :: Account, mailbox :: [Mailbox], rootLinks :: [Link] } deriving (Show)
data Account = Account { fullName :: String, accountLinks :: [Link] } deriving (Show)
data Documents = Documents { document :: [Document] } deriving (Show, Read)
data Document = Document { subject :: String, origin :: String, fileType :: String, documentLinks :: [Link] } deriving (Show, Read)
data Folders = Folders { folder :: [Folder] } deriving (Show)
data Folder = Folder { folderName :: String, icon :: String, folderLinks :: [Link], documents :: Maybe Documents } deriving (Show, Read)
data Mailbox = Mailbox { folders :: Folders, mailboxLinks :: [Link] } deriving (Show)

linkWithRel :: String -> [Link] -> Maybe Link
linkWithRel relation = find $ isSuffixOf relation . rel

filename :: Document -> String
filename doc = baseName ++ suffix
    where baseName = subject doc
          filetype = '.' : fileType doc
          suffix
            | filetype == "." = ""
            | filetype `isSuffixOf` baseName = ""
            | otherwise = filetype

lowerCaseFilename :: Document -> String
lowerCaseFilename = map toLower . filename

uploaded :: Document -> Bool
uploaded = ("UPLOADED" ==) . origin



instance FromJSON Link
instance ToJSON Link

instance FromJSON Root where
    parseJSON (Object v) = Root <$>
                            v .: "csrfToken" <*>
                            v .: "primaryAccount" <*>
                            v .: "mailbox" <*>
                            v .: "link"
    parseJSON _          = mzero

instance ToJSON Root where
    toJSON (Root t a m l) = object [
                                    "csrfToken" .= t,
                                    "primaryAccount" .= a,
                                    "mailbox" .= m,
                                    "link" .= l]

instance FromJSON Account where
    parseJSON (Object v) = Account <$>
                            v .: "fullName" <*>
                            v .: "link"
    parseJSON _          = mzero

instance ToJSON Account where
    toJSON (Account name links) = object ["fullName" .= name, "link" .= links]

instance FromJSON Documents where
    parseJSON (Object v) = Documents <$> v .: "document"
    parseJSON _          = mzero

instance ToJSON Documents where
    toJSON (Documents d) = object ["document" .= d]

instance FromJSON Document where
    parseJSON (Object v) = Document <$>
                            v .: "subject" <*>
                            v .: "origin" <*>
                            v .: "fileType" <*>
                            v .: "link"
    parseJSON _          = mzero

instance ToJSON Document where
    toJSON (Document s o ft l) = object [
                                    "subject" .= s,
                                    "origin" .= o,
                                    "fileType" .= ft,
                                    "link" .= l]

instance FromJSON Folders where
    parseJSON (Object v) = Folders <$> v .: "folder"
    parseJSON _          = mzero

instance ToJSON Folders where
    toJSON (Folders f) = object ["folder" .= f]


instance FromJSON Folder where
    parseJSON (Object v) = Folder <$>
                            v .: "name" <*>
                            v .: "icon" <*>
                            v .: "link" <*>
                            v .:? "documents"
    parseJSON _          = mzero

instance ToJSON Folder where
    toJSON (Folder n i l d) = object [
                                    "name" .= n,
                                    "icon" .= i,
                                    "link" .= l,
                                    "documents" .= d]

instance FromJSON Mailbox where
    parseJSON (Object v) = Mailbox <$>
                            v .: "folders" <*>
                            v .: "link"
    parseJSON _          = mzero

instance ToJSON Mailbox where
    toJSON (Mailbox f l) = object ["folders" .= f, "link" .= l]
