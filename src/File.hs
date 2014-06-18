module File where

import System.Directory
import Data.List
import System.FilePath.Posix
import Control.Monad

import Document
import Folder

existingFiles :: FilePath -> IO [FilePath]
existingFiles = fmap filterSpecialFiles . getDirectoryContents

existingFolders :: FilePath -> IO [FilePath]
existingFolders path = existingFiles path >>= filterM doesRelativeDirExists
	where
		doesRelativeDirExists = doesDirectoryExist . combine path

filterSpecialFiles :: [FilePath] -> [FilePath]
filterSpecialFiles = removeDirs . removeHidden
    where removeDirs = (\\ [".", ".."])
          removeHidden = filter (not . hidden)
          hidden = isPrefixOf "."

readSyncFile :: FilePath -> IO [FilePath]
readSyncFile syncfile = doesFileExist syncfile >>= readIfExists
	where 
		readIfExists False = return []
		readIfExists True = (fmap lines . readFile) syncfile


writeSyncFile :: FilePath -> [FilePath] -> IO ()
writeSyncFile syncfile content = writeFile syncfile $ unlines content

syncFile :: FilePath -> FilePath
syncFile syncDir = combine syncDir ".sync"

syncDiff :: [FilePath] -> [FilePath] -> [Document] -> ([Document], [FilePath], [FilePath])
syncDiff lastState localFiles remoteDocs = (docsToDownload, filesToUpload, filesToDelete)
	where 
		docsToDownload = docsNotInList localFiles remoteDocs
		filesToDelete = filesNotInDocList lastState remoteDocs
		filesToUpload = filesNotInDocList localFiles remoteDocs \\ filesToDelete

folderDiff :: [FilePath] -> [FilePath] -> [Folder] -> ([Folder], [FilePath], [FilePath])
folderDiff lastState localFolders remoteFolders = (newRemoteFolders, newLocalFolders, deletedRemoteFolders)
	where
		newRemoteFolders = foldersNotInList localFolders remoteFolders
		deletedRemoteFolders = foldersNotInRemoteList lastState remoteFolders
		newLocalFolders = foldersNotInRemoteList localFolders remoteFolders \\ deletedRemoteFolders

deleteAll :: FilePath -> [FilePath] -> IO ()
deleteAll syncDir = void . mapM (removeFile . combine syncDir)

deleteAllFolders :: FilePath -> [FilePath] -> IO ()
deleteAllFolders syncDir = void . mapM (removeDirectoryRecursive . combine syncDir)

createSyncDir :: FilePath -> IO ()
createSyncDir = createDirectoryIfMissing True

createFolders :: FilePath -> [FilePath] -> IO ()
createFolders syncDir = void . mapM (createDirectoryIfMissing True . combine syncDir)