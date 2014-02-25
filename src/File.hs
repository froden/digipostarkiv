module File where

import System.Directory
import Data.List

existingFiles :: FilePath -> IO [FilePath]
existingFiles = fmap filterSpecialFiles . getDirectoryContents

filterSpecialFiles :: [FilePath] -> [FilePath]
filterSpecialFiles = removeDirs . removeHidden
    where removeDirs = (\\ [".", ".."])
          removeHidden = filter (not . hidden)
          hidden = isPrefixOf "."

readSyncFile :: FilePath -> IO [FilePath]
readSyncFile = fmap lines . readFile