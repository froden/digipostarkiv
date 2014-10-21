{-# LANGUAGE OverloadedStrings #-}

module Sync where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Control.Monad.Error
import Control.Monad.Trans.Resource
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import System.FilePath.Posix
import Data.List
import Data.Maybe
import System.Directory
import Control.Exception
import Text.Read (readMaybe)

import Api
import qualified ApiTypes as DP
import Oauth
import Http (AccessToken)
import Error

type CSRFToken = String
type ApiAction a = ReaderT (Manager, AccessToken, CSRFToken, DP.Mailbox) (ResourceT IO) a

data File' = File' FilePath (Maybe DP.Document)
data Dir' = Dir' FilePath [File'] (Maybe DP.Folder)

type Name = String
data FileTree =
    File Name (Maybe DP.Document) |
    Dir Name [FileTree] (Maybe DP.Folder) deriving (Show, Read)

data FTCtx = FTCtx Name [FileTree] [FileTree] (Maybe DP.Folder) deriving (Show)

type FTZipper = (FileTree, [FTCtx])

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

syncDirName :: String
syncDirName = "Digipostarkiv"

readSyncState :: FilePath -> IO FileTree
readSyncState syncFile = do
    r <- try (readFile syncFile) :: IO (Either IOException String)
    case r of
        Right content -> return $ fromMaybe emptyDir (readMaybe content)
        Left _ -> return emptyDir
    where emptyDir = Dir syncDirName [] Nothing

writeSyncState :: FilePath -> FileTree -> IO ()
writeSyncState syncFile state = writeFile syncFile (show state)

getLocalState :: FilePath -> IO FileTree
getLocalState dirPath = do
    names <- getDirectoryContents dirPath
    let properNames = filter (not . specialFiles) names
    content <- forM properNames $ \name -> do
        let subPath = dirPath </> name
        isDirectory <- doesDirectoryExist subPath
        if isDirectory
            then getLocalState subPath
            else return (File name Nothing)
    return $ Dir (takeFileName dirPath) content Nothing
    where specialFiles f = "." `isPrefixOf` f || f `elem` [".", ".."]

getRemoteState :: ApiAction FileTree
getRemoteState = do
    (_, _, _, mbox) <- ask
    let folders = (DP.folder . DP.folders) mbox
    contents <- mapM downloadFolder folders
    return $ Dir syncDirName contents Nothing
  where
    downloadFolder :: DP.Folder -> ApiAction FileTree
    downloadFolder folder = do
      (manager, aToken, _, _) <- ask
      folderLink <- liftIO $ linkOrException "self" $ DP.folderLinks folder
      fullFolder <- liftResourceT $ getFolder aToken manager folderLink
      let documents = filter DP.uploaded (DP.documentInFolder fullFolder)
      let files = map docToFile documents
      return $ Dir (DP.folderName folder) files (Just folder)
      where
        docToFile doc = File (DP.filename doc) (Just doc)

instance Eq FileTree where
  (Dir name1 contents1 _) == (Dir name2 contents2 _) = name1 == name2 && contents1 == contents2
  (File name1 _) == (File name2 _) = name1 == name2
  _ == _ = False

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

--for now does not consider locally deleted files to ensure
--we dont acidentally delete all files on server
--files deleted locally will be restored from server
newOnServer :: FileTree -> FileTree -> FileTree -> Maybe FileTree
newOnServer local _ remote = remote `treeDiff` local

deletedOnServer :: FileTree -> FileTree -> Maybe FileTree
deletedOnServer previous remote = previous `treeDiff` remote

newLocal :: FileTree -> FileTree -> FileTree -> Maybe FileTree
newLocal local previous remote = do
  new <- local `treeDiff` remote
  case deletedOnServer previous remote of
    Nothing -> return new
    Just d -> new `treeDiff` d



--deletedLocal :: FileTree -> FileTree -> FileTree -> FileTree

ftName :: FileTree -> String
ftName (File name _) = name
ftName (Dir name _ _) = name

fullPath :: FilePath -> FTZipper -> FilePath
fullPath syncDir z@(item, _) = case ftUp z of
  Nothing -> takeDirectory syncDir `combine` name
  Just parentZipper -> fullPath syncDir parentZipper `combine` name
  where name = ftName item

ftTraverse :: Monad m => (FTZipper -> m FTZipper) -> FTZipper -> m ()
ftTraverse action z@(File{}, _) = do
  nz <- action z
  maybe (return ()) (ftTraverse action) (ftRight nz)
ftTraverse action z@(Dir{}, _) = do
  nz <- action z
  maybe (return ()) (ftTraverse action) (ftDown nz)
  maybe (return ()) (ftTraverse action) (ftRight nz)

download :: FilePath -> FTZipper -> ApiAction ()
download syncDir = ftTraverse download'
  where
    download' :: FTZipper -> ApiAction FTZipper
    download' z@(File _ (Just remoteDoc), _) =
      Sync.downloadDocument (fullPath syncDir z) remoteDoc >> return z
    download' (File _ Nothing, _) =
      error "Either file at root node of no remote document"
    download' z@(Dir{}, _) =
      liftIO $ createDirectoryIfMissing True (fullPath syncDir z) >> return z


upload :: FilePath -> FTZipper -> ApiAction ()
upload syncDir = ftTraverse upload'
  where
    upload' :: FTZipper -> ApiAction FTZipper
    upload' z@(File _ _, FTCtx _ _ _ (Just parentFolder):_) =
      uploadDocument parentFolder (fullPath syncDir z) >> return z
    upload' (File _ _, _) = error "Either file at root node or no parent remote folder"
    upload' (Dir name contents folder, ctx) = do
      newFolder <- if name /= syncDirName && isNothing folder
        then liftM Just (createRemoteFolder name)
        else return folder
      return (Dir name contents newFolder, ctx)


deleteLocal :: FilePath -> FTZipper -> IO ()
deleteLocal syncDir = ftTraverse deleteLocal'
  where
    deleteLocal' :: FTZipper -> IO FTZipper
    deleteLocal' z@(File{}, _) = removeFile (fullPath syncDir z) >> return z
    deleteLocal' z@(Dir name [] _, _) = do
      unless (name == syncDirName) $ removeDirectoryRecursive (fullPath syncDir z)
      return z
    deleteLocal' z@(Dir{}, _) = return z


downloadDocument :: FilePath -> DP.Document -> ApiAction ()
downloadDocument localPath remoteDoc = do
  (manager, aToken, _, _) <- ask
  liftResourceT $ Api.downloadDocument aToken manager localPath remoteDoc

createRemoteFolder :: Name -> ApiAction DP.Folder
createRemoteFolder folderName = do
  (manager, aToken, csrfToken, mbox) <- ask
  createFolderLink <- liftIO $ linkOrException "create_folder" $ DP.mailboxLinks mbox
  liftResourceT $ createFolder aToken manager createFolderLink csrfToken folderName

uploadDocument :: DP.Folder -> FilePath -> ApiAction ()
uploadDocument folder localPath = do
    (manager, aToken, csrfToken, _) <- ask
    uploadLink <- liftIO $ linkOrException "upload_document" $ DP.folderLinks folder
    liftResourceT $ uploadFileMultipart aToken manager uploadLink csrfToken localPath

handleTokenRefresh :: (AccessToken -> IO a) -> AccessToken -> IO a
handleTokenRefresh accessFunc token = catch (accessFunc token) handleException
    where
        handleException (StatusCodeException (Status 403 _) hdrs _) = do
            debugLog $ "403 status" ++ show hdrs
            newToken <- refreshAccessToken token --TODO: exceptions?
            storeAccessToken newToken
            accessFunc newToken  --TODO: retry count??
        handleException (StatusCodeException (Status 401 _) hdrs _) = do
            debugLog $ "401 status " ++ show hdrs
            throwIO NotAuthenticated
        handleException e = throwIO $ HttpFailed e


initLocalState :: IO (FilePath, FilePath, FileTree, FileTree)
initLocalState = do
    syncDir <- getOrCreateSyncDir
    let syncFile = getSyncFile syncDir
    previousState <- readSyncState syncFile
    localState <- getLocalState syncDir
    return (syncDir, syncFile, previousState, localState)

checkLocalChange :: IO Bool
checkLocalChange = do
    (_, _, previousState, localState) <- initLocalState
    let added = localState `treeDiff` previousState
    let deleted = previousState `treeDiff` localState
    return $ isJust added || isJust deleted

checkRemoteChange :: IO Bool
checkRemoteChange = loadAccessToken >>= handleTokenRefresh checkRemoteChange'

checkRemoteChange' :: AccessToken -> IO Bool
checkRemoteChange' token = do
    (_, _, previousState, _) <- initLocalState
    runResourceT $ do
      manager <- liftIO $ newManager conduitManagerSettings
      (root, _, mbox) <- getAccount manager token
      remoteState <- runReaderT getRemoteState (manager, token, DP.csrfToken root, mbox)
      let added = remoteState `treeDiff` previousState
      let deleted = previousState `treeDiff` remoteState
      return $ isJust added || isJust deleted

sync :: IO ()
sync = loadAccessToken >>= handleTokenRefresh sync'

--bedre feil-logging
sync' :: AccessToken -> IO ()
sync' token = do
    (syncDir, syncFile, previousState, localState) <- initLocalState
    runResourceT $ do
      manager <- liftIO $ newManager conduitManagerSettings
      (root, _, mbox) <- getAccount manager token
      flip runReaderT (manager, token, DP.csrfToken root, mbox) $ do
        remoteState <- getRemoteState
        let toDownload = newOnServer localState previousState remoteState
        liftIO $ debugLog $ "down " ++ show toDownload
        let toUpload = newLocal localState previousState remoteState
        liftIO $ debugLog $ "up " ++ show toUpload
        let toDelete = deletedOnServer previousState remoteState
        liftIO $ debugLog $ "del " ++ show toDelete
        maybe (return ()) (download syncDir . ftZipper) toDownload
        maybe (return ()) (upload syncDir . ftZipper) toUpload
        liftIO $ maybe (return ()) (deleteLocal syncDir . ftZipper) toDelete
      liftIO $ getLocalState syncDir >>= writeSyncState syncFile

getUserSyncDir :: IO FilePath
getUserSyncDir = do
    homedir <- getHomeDirectory
    return $ combine homedir syncDirName

getOrCreateSyncDir :: IO FilePath
getOrCreateSyncDir = do
    syncDir <- getUserSyncDir
    createDirectoryIfMissing True syncDir
    return syncDir

getSyncFile :: FilePath -> FilePath
getSyncFile syncDir = combine syncDir ".sync"

debugLog :: String -> IO ()
--debugLog = putStrLn
debugLog _ = return ()
