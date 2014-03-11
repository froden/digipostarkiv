{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, OverloadedStrings #-}

module Config where

import Data.Aeson
import GHC.Generics (Generic)
import Control.Exception
import Data.Typeable
import System.FilePath.Posix
import qualified Data.ByteString.Lazy as LBS

data Config = Config {
 	syncDir  :: FilePath,
    interval :: Maybe Int } deriving (Show, Generic)

defaultConfig :: FilePath -> Config
defaultConfig homeDir = Config (combine homeDir "Digipostarkiv") (Just 10)

data ConfigParseException = ConfigParseException deriving (Show, Typeable)
instance Exception ConfigParseException

readConfigFile :: FilePath -> IO Config
readConfigFile file = fmap (orThrow . decode) $ LBS.readFile file
    where orThrow Nothing = throw ConfigParseException
          orThrow (Just conf) = conf

instance FromJSON Config
instance ToJSON Config
