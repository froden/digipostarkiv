{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Document where

import Data.Aeson
import GHC.Generics (Generic)
import Link
import Data.List
import Data.Char

data Documents = Documents { document :: [Document] } deriving (Show, Generic)
data Document = Document { subject :: String, creatorName :: String, origin :: String, fileType :: String, link :: [Link] } deriving (Show, Generic)

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

filesNotInDocList :: [FilePath] -> [Document] -> [FilePath]
filesNotInDocList files = deleteFirstsBy equalIgnoreCase files . map filename
	where equalIgnoreCase file doc = (map toLower file) == (map toLower doc)

docsInList :: [FilePath] -> [Document] -> [Document]
docsInList files = filter (\d -> lowerCaseFilename d `elem` lowerFiles)
	where lowerFiles = map (map toLower) files

docsNotInList :: [FilePath] -> [Document] -> [Document]
docsNotInList files = filter (\d -> lowerCaseFilename d `notElem` lowerFiles)
	where lowerFiles = map (map toLower) files

instance FromJSON Documents
instance ToJSON Documents

instance FromJSON Document
instance ToJSON Document