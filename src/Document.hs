{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Document where

import Data.Aeson
import GHC.Generics (Generic)
import Link
import Data.Time
import System.Locale
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (liftM)
import Data.List

data Documents = Documents { document :: [Document] } deriving (Show, Generic)
data Document = Document { subject :: String, creatorName :: String, created :: Maybe ZonedTime, origin :: String, fileType :: String, link :: [Link] } deriving (Show)

parseISODateTime :: String -> Maybe ZonedTime
parseISODateTime = parseTime defaultTimeLocale "%FT%X%Q%Z"

filename :: Document -> String
filename doc = baseName ++ suffix
    where baseName = subject doc
          filetype = "." ++ fileType doc
          suffix = if filetype `isSuffixOf` baseName then "" else filetype

uploaded :: Document -> Bool
uploaded = ("UPLOADED" ==) . origin

notDownloaded :: [FilePath] -> [Document] -> [Document]
notDownloaded files = filter (\d -> filename d `notElem` files)

notUploaded :: [FilePath] -> [Document] -> [FilePath]
notUploaded files = (files \\) . map filename

instance FromJSON Documents
instance ToJSON Documents

instance FromJSON Document where
    parseJSON (Object v) = Document <$>
                         (v .: "subject") <*>
                         (v .: "creatorName") <*>
                         liftM parseISODateTime (v .: "created") <*>
                         (v .: "origin") <*>
                         (v .: "fileType") <*>
                         (v .: "link")
    parseJSON _          = empty

instance ToJSON Document where
    toJSON (Document s cn c o f l) =
        object [
            "subject" .= s,
            "creatorName" .= cn,
            "created" .= c,
            "origin" .= o,
            "fileType" .= f,
            "link" .= l
            ]