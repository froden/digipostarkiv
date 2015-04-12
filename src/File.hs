{-# LANGUAGE OverloadedStrings #-}

module File where

import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath.Posix
import ApiTypes (Folder, Document, folderName, filename, createdTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import Data.String.Utils

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

data Change = Created  {changedFile :: File}
            | Deleted  {changedFile :: File} deriving (Show)
            -- | Modified {changePath :: Path}

instance Eq Change where
    (Created file1) == (Created file2) = filePathEq file1 file2
    (Deleted file1) == (Deleted file2) = filePathEq file1 file2
    _ == _ = False

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
        fmap Deleted (Set.toAscList deleted) ++ fmap Created ((reverse . Set.toAscList) created)

computeChangesToApply :: [Change] -> [Change] -> [Change]
computeChangesToApply = (\\)


computeNewState :: Set File -> [Change] -> [Change] -> [Change] -> Set File
computeNewState previousFiles localChanges appliedLocalChanges appliedRemoteChanges =
    computeNewStateFromChanges previousFiles (appliedLocalChanges `union` (localChanges `intersect` appliedRemoteChanges))

findFileByPath :: Set File -> Path -> Maybe File
findFileByPath set p =
    let matchingFiles = Set.filter ((== p) . path) set
    in  if Set.null matchingFiles
        then Nothing
        else Just $ Set.findMin matchingFiles

computeNewStateFromChanges :: Set File -> [Change] -> Set File
computeNewStateFromChanges = foldl applyChange
    where
        applyChange :: Set File -> Change -> Set File
        applyChange resultSet (Created file) = Set.insert file resultSet
        applyChange resultSet (Deleted file) = case findFileByPath resultSet (path file) of
                                                        Just f -> Set.delete f resultSet
                                                        Nothing -> resultSet

getFileSetFromMap :: Map Path RemoteFile -> Set File
getFileSetFromMap = Set.fromList . map getFile . Map.elems

replaceSpecialChars :: [String] -> [String]
replaceSpecialChars = map replaceSpecial
    where
        replaceSpecial =   replace "A\0778" "\0197"
                         . replace "a\0778" "\0229"