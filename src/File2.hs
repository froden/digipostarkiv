{-# LANGUAGE OverloadedStrings #-}

module File2 where

import Data.Set
import System.FilePath.Posix
import ApiTypes (Folder, Document, folderName, filename)

data File = File {path :: FilePath} | Dir {path :: FilePath} deriving (Show, Eq, Ord)

fileFromRemote :: Folder -> Document -> File
fileFromRemote folder document = File $ folderName folder `combine` filename document

dirFromFolder :: Folder -> File
dirFromFolder folder = Dir $ folderName folder ++ "/"

data Change = Created File | Deleted File deriving (Show, Eq)

data RemoteFile = RemoteFile File Folder Document | RemoteDir File Folder deriving (Show)

remoteFileFromRemote :: Folder -> Document -> RemoteFile
remoteFileFromRemote folder document = RemoteFile (fileFromRemote folder document) folder document

remoteDirFromFolder :: Folder -> RemoteFile
remoteDirFromFolder folder = RemoteDir (dirFromFolder folder) folder

computeChanges :: Set File -> Set File -> [Change]
computeChanges now previous =
    let
        created = difference now previous
        deleted = difference previous now
    in
        fmap Created ((reverse . toAscList) created) ++ fmap Deleted (toAscList deleted)

