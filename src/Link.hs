{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, FlexibleContexts #-}

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

linkWithRelM :: (Monad m) => String -> [Link] -> m Link
linkWithRelM relation links = case linkWithRel relation links of
	Nothing -> fail $ "No link found with rel " ++ relation
	Just l -> return l