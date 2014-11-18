module File where

<<<<<<< HEAD
import System.Directory
import Data.List
import System.FilePath.Posix
import Control.Monad
import Control.Exception
import Data.Char
import Text.Read
import Data.Maybe

import qualified ApiTypes as DP

data Directory = Directory Filename [Filename] deriving (Read, Show)

dirname :: Directory -> Filename
dirname (Directory n _) = n

dirnames :: [Directory] -> [Filename]
dirnames = map dirname

dirContents :: Directory -> [Filename]
dirContents (Directory _ c) = c

readSyncDirContents :: FilePath -> IO [Directory]
readSyncDirContents path = do
    r <- try (readFile path) :: IO (Either IOException String)
    case r of
        Right content -> return $ fromMaybe [] (readMaybe content)
        Left _ -> return []

writeSyncDirContents :: FilePath -> [Directory] -> IO ()
writeSyncDirContents path dirs = writeFile path (show dirs)

newtype Filename = Filename { name :: String } deriving (Eq, Read, Show)

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

readSyncFile :: FilePath -> IO [Directory]
readSyncFile syncfile = catch readFileIfExists whenNotFound
    where
        readFileIfExists = do
            content <- readFile syncfile
            return $ fromMaybe [] (readMaybe content)
        whenNotFound :: IOException -> IO [Directory]
        whenNotFound _ = return []

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
=======
import System.FilePath ((</>), takeDirectory)

import ApiTypes


type Name = String
data FileTree =
    File Name (Maybe Document) |
    Dir Name [FileTree] (Maybe Folder) deriving (Show, Read)

data FTCtx = FTCtx Name [FileTree] [FileTree] (Maybe Folder) deriving (Show)

type FTZipper = (FileTree, [FTCtx])

instance Eq FileTree where
  (Dir name1 contents1 _) == (Dir name2 contents2 _) = name1 == name2 && contents1 == contents2
  (File name1 _) == (File name2 _) = name1 == name2
  _ == _ = False

ftZipper :: FileTree -> FTZipper
ftZipper ft = (ft, [])

ftUp :: FTZipper -> Maybe FTZipper
ftUp (item, FTCtx name ls rs f:bs) = Just (Dir name (ls ++ [item] ++ rs) f, bs)
ftUp (_, []) = Nothing

ftRight :: FTZipper -> Maybe FTZipper
ftRight (item, FTCtx name ls (r:rs) f:bs) = Just (r, FTCtx name (ls ++ [item]) rs f:bs)
ftRight (_, FTCtx _ _ [] _:_) = Nothing
ftRight (_, []) = Nothing

ftDown :: FTZipper -> Maybe FTZipper
ftDown (Dir name (c:cs) f, bs) = Just (c, FTCtx name [] cs f:bs)
ftDown (Dir _ [] _, _) = Nothing
ftDown (File _ _, _) = error "cannot go down into file"

ftName :: FileTree -> String
ftName (File name _) = name
ftName (Dir name _ _) = name

ftTraverse :: Monad m => (FTZipper -> m FTZipper) -> FTZipper -> m ()
ftTraverse action z@(File{}, _) = do
  nz <- action z
  maybe (return ()) (ftTraverse action) (ftRight nz)
ftTraverse action z@(Dir{}, _) = do
  nz <- action z
  maybe (return ()) (ftTraverse action) (ftDown nz)
  maybe (return ()) (ftTraverse action) (ftRight nz)

treeDiff :: FileTree -> FileTree -> Maybe FileTree
treeDiff ft1@(Dir name1 contents1 folder1) ft2@(Dir name2 contents2 folder2)
    | ft1 == ft2 = Nothing
    | name1 == name2 =
        let contentDiff = contents1 `diff` contents2
        in if null contentDiff
           then Nothing
           else Just $ Dir name1 contentDiff (folder1 `orElse` folder2)
treeDiff ft1 ft2
    | ft1 == ft2 = Nothing
    | otherwise = Just ft1

diff :: [FileTree] -> [FileTree] -> [FileTree]
diff = foldl (flip deleteFrom)
  where
    deleteFrom _ [] = []
    deleteFrom x (y:ys) = case treeDiff y x of
      Nothing -> ys
      Just ft -> ft : deleteFrom x ys

orElse :: Maybe a -> Maybe a -> Maybe a
orElse mx my = case mx of
  Nothing -> my
  Just x -> Just x

fullPath :: FilePath -> FTZipper -> FilePath
fullPath syncDir z@(item, _) = case ftUp z of
  Nothing -> takeDirectory syncDir </> name
  Just parentZipper -> fullPath syncDir parentZipper </> name
  where name = ftName item
>>>>>>> master
