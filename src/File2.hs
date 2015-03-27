{-# LANGUAGE OverloadedStrings #-}

module File2 where

import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath.Posix
import ApiTypes (Folder, Document, folderName, filename, createdTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime

newtype Path = Path {filePath :: FilePath} deriving (Show, Read, Eq, Ord)

data File = File {path :: Path, modifiedTime :: UTCTime} | Dir {path :: Path} deriving (Show, Read, Eq, Ord)

filePathEq :: File -> File -> Bool
filePathEq (File path1 _) (File path2 _) = path1 == path2
filePathEq (Dir path1) (Dir path2) = path1 == path2
filePathEq _ _ = False

fileFromFolderDoc :: Folder -> Document -> File
fileFromFolderDoc folder document = File (folderName folder `combine` filename document) ((zonedTimeToUTC . createdTime) document)

dirFromFolder :: Folder -> File
dirFromFolder folder = Dir $ (addTrailingPathSeparator . folderName) folder

--fileFromFilePath :: FilePath -> File
--fileFromFilePath p = if hasTrailingPathSeparator p then Dir p else File p

data Change = Created  {createdPath  :: Path}
            | Deleted  {deletedPath  :: Path}
            | Modified {modifiedP :: File} deriving (Show)

instance Eq Change where
    (Created file1) == (Created file2) = file1 `filePathEq` file2
    (Deleted file1) == (Deleted file2) = file1 `filePathEq` file2
    (Modified file1) == (Modified file2) = file1 `filePathEq` file2
    _ == _ = False

data RemoteFile = RemoteFile File Folder Document | RemoteDir File Folder deriving (Show, Read)

--data RemoteChange = RemoteCreated File RemoteFile | RemoteDeleted File RemoteFile deriving (Show)
data RemoteChange = RemoteChange Change (Maybe RemoteFile) deriving (Show)

remoteFileFromFolderDoc :: Folder -> Document -> RemoteFile
remoteFileFromFolderDoc folder document = RemoteFile (fileFromFolderDoc folder document) folder document

remoteDirFromFolder :: Folder -> RemoteFile
remoteDirFromFolder folder = RemoteDir (dirFromFolder folder) folder

computeChanges :: Set File -> Set File -> [Change]
computeChanges now previous =
    let
        created = Set.difference now previous
        deleted = Set.difference previous now
    in
        fmap Created ((reverse . Set.toAscList) created) ++ fmap Deleted (Set.toAscList deleted)

computeChangesToApply :: [Change] -> [Change] -> [Change]
computeChangesToApply = (\\)


computeNewStateFromChanges :: Set File -> [Change] -> Set File
computeNewStateFromChanges = foldl applyChange
    where
        applyChange :: Set File -> Change -> Set File
        applyChange resultSet (Created file) = Set.insert file resultSet
        applyChange resultSet (Deleted file) = Set.delete file resultSet

getFileSetFromMap :: Map File RemoteFile -> Set File
getFileSetFromMap = Map.keysSet