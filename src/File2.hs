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
fileFromFolderDoc folder document = File (Path (folderName folder `combine` filename document)) ((zonedTimeToUTC . createdTime) document)

dirFromFolder :: Folder -> File
dirFromFolder = Dir . pathFromFolder

pathFromFolder :: Folder -> Path
pathFromFolder = Path . addTrailingPathSeparator . folderName

isDir :: Path -> Bool
isDir = hasTrailingPathSeparator . filePath

isFile :: Path -> Bool
isFile = not . isDir

--fileFromFilePath :: FilePath -> File
--fileFromFilePath p = if hasTrailingPathSeparator p then Dir p else File p

data Change = Created  {changePath :: Path}
            | Deleted  {changePath :: Path} deriving (Show, Eq)
            -- | Modified {changePath :: Path}

data AppliedChange = AppliedChange Change File

data RemoteFile = RemoteFile File Folder Document | RemoteDir File Folder deriving (Show, Read)

getFile :: RemoteFile -> File
getFile (RemoteFile file _ _) = file
getFile (RemoteDir dir _) = dir

--data RemoteChange = RemoteCreated File RemoteFile | RemoteDeleted File RemoteFile deriving (Show)
data RemoteChange = RemoteChange Change RemoteFile deriving (Show)

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
        fmap Created ((reverse . Set.toAscList . Set.map path) created) ++ fmap Deleted ((Set.toAscList . Set.map path) deleted)

computeChangesToApply :: [Change] -> [Change] -> [Change]
computeChangesToApply = (\\)


computeNewStateFromChanges :: Set File -> [AppliedChange] -> Set File
computeNewStateFromChanges = foldl applyChange
    where
        applyChange :: Set File -> AppliedChange -> Set File
        applyChange resultSet (AppliedChange (Created _) file) = Set.insert file resultSet
        applyChange resultSet (AppliedChange (Deleted _) file) = Set.delete file resultSet

getFileSetFromMap :: Map Path RemoteFile -> Set File
getFileSetFromMap = Set.fromList . map getFile . Map.elems