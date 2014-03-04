module File where

import System.Directory
import Data.List
import System.FilePath.Posix

import Document

existingFiles :: FilePath -> IO [FilePath]
existingFiles = fmap filterSpecialFiles . getDirectoryContents

filterSpecialFiles :: [FilePath] -> [FilePath]
filterSpecialFiles = removeDirs . removeHidden
    where removeDirs = (\\ [".", ".."])
          removeHidden = filter (not . hidden)
          hidden = isPrefixOf "."

readSyncFile :: FilePath -> IO [FilePath]
readSyncFile syncfile = doesFileExist syncfile >>= doRead
	where 
		doRead False = return []
		doRead True = (fmap lines . readFile) syncfile


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

deleteAll :: FilePath -> [FilePath] -> IO [()]
deleteAll syncDir = mapM (removeFile . combine syncDir)

createSyncDir :: FilePath -> IO ()
createSyncDir = createDirectoryIfMissing True