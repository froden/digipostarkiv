{-# LANGUAGE DeriveGeneric #-}

module Account where

import Data.Aeson
import GHC.Generics (Generic)
import Link

data Account = Account { fullName :: String, link :: [Link] } deriving (Show, Generic)

archiveLink :: (Monad m) => Account -> m Link
archiveLink acc = linkWithRelM "document_archive" $ link acc

instance FromJSON Account
instance ToJSON Account