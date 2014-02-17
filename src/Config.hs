{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Aeson
import GHC.Generics (Generic)
import Control.Exception
import Data.Typeable
import qualified Data.ByteString.Lazy as LBS

data Config = Config { 
    username :: String, 
    password :: String,
    syncDir  :: FilePath } deriving (Show, Generic)

data ConfigParseException = ConfigParseException deriving (Show, Typeable)
instance Exception ConfigParseException

readConfigFile :: FilePath -> IO Config
readConfigFile file = fmap (orThrow . decode) $ LBS.readFile file
    where orThrow Nothing = throw ConfigParseException
          orThrow (Just conf) = conf

instance FromJSON Config
instance ToJSON Config