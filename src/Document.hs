{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Document where

import Data.Aeson
import GHC.Generics (Generic)
import Link
import Data.List

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

uploaded :: Document -> Bool
uploaded = ("UPLOADED" ==) . origin

filesNotInDocList :: [FilePath] -> [Document] -> [FilePath]
filesNotInDocList files = (files \\) . map filename

docsInList :: [FilePath] -> [Document] -> [Document]
docsInList files = filter (\d -> filename d `elem` files)

docsNotInList :: [FilePath] -> [Document] -> [Document]
docsNotInList files = filter (\d -> filename d `notElem` files)


instance FromJSON Documents
instance ToJSON Documents

instance FromJSON Document
instance ToJSON Document