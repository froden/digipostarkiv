module File where

import System.Directory
import Data.List
import System.FilePath.Posix
import Control.Monad
import Control.Applicative
import Data.Char

import qualified ApiTypes as DP

newtype Filename = Filename { name :: String } deriving (Eq, Show)

toLowerCaseFilename :: Filename -> Filename
toLowerCaseFilename = Filename . map toLower . name

class File a where
    filename :: a -> Filename

    lowerCaseFilename :: a -> Filename
    lowerCaseFilename = toLowerCaseFilename . filename

    filenameStr :: a -> String
    filenameStr = name . filename

instance File DP.Document where
    filename = Filename . DP.filename

instance File Filename where
    filename fn = fn

instance File DP.Folder where
    filename = Filename . DP.folderName

fileEqual :: (File a, File b) => a -> b -> Bool
fileEqual f1 f2 = lowerCaseFilename f1 == lowerCaseFilename f2

deleteFileBy :: (File a, File b) => b -> [a] -> [a]
deleteFileBy _ [] = []
deleteFileBy x (y:ys) = if x `fileEqual` y then ys else y : deleteFileBy x ys

diff :: (File a, File b) => [a] -> [b] -> [a]
diff = foldl (flip deleteFileBy)

syncDiff :: (File a, File b) => [a] -> [a] -> [b] -> ([b], [a], [a])
syncDiff lastState localFiles remoteDocs = (docsToDownload, filesToUpload, filesToDelete)
    where
        docsToDownload = diff remoteDocs localFiles
        filesToDelete = diff lastState remoteDocs
        filesToUpload = diff localFiles remoteDocs `diff` filesToDelete

readSyncFile' :: FilePath -> IO [Filename]
readSyncFile' syncfile = doesFileExist syncfile >>= readIfExists
    where
        readIfExists False = return []
        readIfExists True = map Filename . lines <$> readFile syncfile

existingFiles' :: FilePath -> IO [Filename]
existingFiles' = fmap (map Filename) . existingFiles

existingFolders' :: FilePath -> IO [Filename]
existingFolders' = fmap (map Filename) . existingFolders

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

writeSyncFile :: FilePath -> [FilePath] -> IO ()
writeSyncFile syncfile content = writeFile syncfile $ unlines content

syncFile :: FilePath -> FilePath
syncFile syncDir = combine syncDir ".sync"

deleteAll :: FilePath -> [FilePath] -> IO ()
deleteAll syncDir = void . mapM (removeFile . combine syncDir)

deleteAllFolders :: FilePath -> [FilePath] -> IO ()
deleteAllFolders syncDir = void . mapM (removeDirectoryRecursive . combine syncDir)

createSyncDir :: FilePath -> IO ()
createSyncDir = createDirectoryIfMissing True

createFolders :: FilePath -> [FilePath] -> IO ()
createFolders syncDir = void . mapM (createDirectoryIfMissing True . combine syncDir)
