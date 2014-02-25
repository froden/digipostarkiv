{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Link where

import Data.Aeson
import Data.List
import GHC.Generics (Generic)
import Control.Exception
import Data.Typeable

data Link = Link { rel :: String, uri :: String } deriving (Show, Generic)

instance FromJSON Link
instance ToJSON Link

linkWithRel :: String -> [Link] -> Maybe Link
linkWithRel relation = find $ isSuffixOf relation . rel

data NoLinkException = NoLinkException deriving (Show, Typeable)

instance Exception NoLinkException

linkOrException :: Maybe Link -> Link
linkOrException (Just l) = l
linkOrException Nothing = throw NoLinkException