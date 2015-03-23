{-# LANGUAGE OverloadedStrings #-}

module File2 where

import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath.Posix
import ApiTypes (Folder, Document, folderName, filename)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

data File = File {path :: FilePath} | Dir {path :: FilePath} deriving (Show, Read, Eq, Ord)

fileFromFolderDoc :: Folder -> Document -> File
fileFromFolderDoc folder document = File $ folderName folder `combine` filename document

dirFromFolder :: Folder -> File
dirFromFolder folder = Dir $ (addTrailingPathSeparator . folderName) folder

fileFromFilePath :: FilePath -> File
fileFromFilePath p = if hasTrailingPathSeparator p then Dir p else File p

data Change = Created File | Deleted File deriving (Show, Eq)

data RemoteFile = RemoteFile File Folder Document | RemoteDir File Folder deriving (Show, Read)

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
        fmap Created ((reverse . Set.toAscList) created) ++ fmap Deleted (Set.toAscList deleted)

computeChangesToApply :: [Change] -> [Change] -> [Change]
computeChangesToApply = (\\)

computeNewStateFromChanges :: Set File -> [Change] -> Set File
computeNewStateFromChanges = foldl applyChange
    where
        applyChange :: Set File -> Change -> Set File
        applyChange resultSet (Created file) = Set.insert file resultSet
        applyChange resultSet (Deleted file) = Set.delete file resultSet

computeNewRemoteStateFromChanges :: Map File RemoteFile -> [RemoteChange] -> Map File RemoteFile
computeNewRemoteStateFromChanges = foldl applyChange
    where
        applyChange :: Map File RemoteFile -> RemoteChange -> Map File RemoteFile
        applyChange resultMap (RemoteChange (Created file) remoteFile) = Map.insert file remoteFile resultMap
        applyChange resultMap (RemoteChange (Deleted file) _) = Map.delete file resultMap

getFileSetFromMap :: Map File RemoteFile -> Set File
getFileSetFromMap = Map.keysSet