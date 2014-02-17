{-# LANGUAGE DeriveGeneric #-}

module Link where

import Data.Aeson
import Data.List
import GHC.Generics (Generic)

data Link = Link { rel :: String, uri :: String } deriving (Show, Generic)

instance FromJSON Link
instance ToJSON Link

linkWithRel :: String -> [Link] -> Maybe Link
linkWithRel relation = find (\link -> isSuffixOf relation (rel link))