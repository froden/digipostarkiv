{-# LANGUAGE OverloadedStrings #-}

module File2 where

import Data.Set
import System.FilePath.Posix
import ApiTypes (Folder, Document, folderName, filename)
import Data.Map (Map)
import qualified Data.Map as Map

data File = File {path :: FilePath} | Dir {path :: FilePath} deriving (Show, Eq, Ord)

fileFromFolderDoc :: Folder -> Document -> File
fileFromFolderDoc folder document = File $ folderName folder `combine` filename document

dirFromFolder :: Folder -> File
dirFromFolder folder = Dir $ (addTrailingPathSeparator . folderName) folder

fileFromFilePath :: FilePath -> File
fileFromFilePath p = if hasTrailingPathSeparator p then Dir p else File p

data Change = Created File | Deleted File deriving (Show, Eq)

data RemoteFile = RemoteFile File Folder Document | RemoteDir File Folder deriving (Show)

remoteFileFromFolderDoc :: Folder -> Document -> RemoteFile
remoteFileFromFolderDoc folder document = RemoteFile (fileFromFolderDoc folder document) folder document

remoteDirFromFolder :: Folder -> RemoteFile
remoteDirFromFolder folder = RemoteDir (dirFromFolder folder) folder

computeChanges :: Set File -> Set File -> [Change]
computeChanges now previous =
    let
        created = difference now previous
        deleted = difference previous now
    in
        fmap Created ((reverse . toAscList) created) ++ fmap Deleted (toAscList deleted)

getFileSetFromMap :: Map File RemoteFile -> Set File
getFileSetFromMap = Map.keysSet