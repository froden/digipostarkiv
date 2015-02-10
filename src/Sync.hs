{-# LANGUAGE OverloadedStrings #-}

module Sync where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Control.Monad.Error
import Control.Monad.Trans.Resource
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Applicative
import System.FilePath.Posix
import Data.List
import Data.Maybe
import System.Directory
import System.IO.Error
import Control.Exception
import Text.Read (readMaybe)

import Api
import qualified ApiTypes as DP
import Oauth
import Http (AccessToken)
import Error
import File

type CSRFToken = String
type ApiAction a = ReaderT (Manager, AccessToken, CSRFToken, DP.Mailbox) (ResourceT IO) a

data SyncState = SyncState FileTree FileTree deriving (Show, Read)

localSyncState :: SyncState -> FileTree
localSyncState (SyncState local _) = local

remoteSyncState :: SyncState -> FileTree
remoteSyncState (SyncState _ remote) = remote

syncDirName :: String
syncDirName = "Digipostarkiv"

readSyncState :: FilePath -> IO SyncState
readSyncState syncFile = do
        r <- try (readFile syncFile) :: IO (Either IOException String)
        case r of
            Right content -> return $ fromMaybe emptyState (readMaybe content)
            Left _ -> return emptyState
    where emptyState = SyncState (Dir syncDirName [] Nothing) (Dir syncDirName [] Nothing)

writeSyncState :: FilePath -> SyncState -> IO ()
writeSyncState syncFile state = writeFile syncFile (show state)

getDirContents :: FilePath -> IO [FilePath]
getDirContents dirPath = do
        names <- getDirectoryContents dirPath
        return $ filter (not . specialFiles) names
    where specialFiles f = "." `isPrefixOf` f || f `elem` [".", ".."]

getLocalState :: FilePath -> IO FileTree
getLocalState dirPath = do
        properNames <- getDirContents dirPath
        content <- forM properNames $ \name -> do
            let subPath = dirPath </> name
            isDirectory <- doesDirectoryExist subPath
            if isDirectory
                then getLocalState subPath
                else return (File name Nothing)
        return $ Dir (takeFileName dirPath) content Nothing

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

newOnServer :: FileTree -> FileTree -> FileTree -> Maybe FileTree
newOnServer local previous remote = newFiles remote previous local

deletedOnServer :: FileTree -> FileTree -> Maybe FileTree
deletedOnServer = deletedFiles

deletedLocal :: FileTree -> FileTree -> Maybe FileTree
deletedLocal = deletedFiles

newLocal :: FileTree -> FileTree -> FileTree -> Maybe FileTree
newLocal = newFiles

newFiles :: FileTree -> FileTree -> FileTree -> Maybe FileTree
newFiles x previous y = do
      new <- x `treeDiff` y
      case deletedFiles previous y of
        Nothing -> return new
        Just d -> new `treeDiff` d

deletedFiles :: FileTree -> FileTree -> Maybe FileTree
deletedFiles previous x = previous `treeDiff` x

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

deleteRemote :: FTZipper -> ApiAction ()
deleteRemote = ftTraverse deleteRemote'
    where
        deleteRemote' :: FTZipper -> ApiAction FTZipper
        deleteRemote' z@(File _ (Just remoteDoc), _) = deleteDoc remoteDoc >> return z
        deleteRemote' z@(File name Nothing, _) = liftIO $ debugLog ("No Digipost Document attached to local File. Cannot delete remote: " ++ name) >> return z
        deleteRemote' z@(Dir _ [] (Just remoteFolder), _) = Sync.deleteFolder remoteFolder >> return z
        deleteRemote' z@(Dir name [] Nothing, _) = liftIO $ debugLog ("No Digipost Folder attached to local Dir. Cannot delete remote: " ++ name) >> return z
        deleteRemote' z@(Dir {}, _) = return z
        --TODO: delete if empty after docs are deleted

deleteLocal :: FilePath -> FTZipper -> IO ()
deleteLocal syncDir = ftTraverseDirLast deleteLocal'
  where
    deleteLocal' :: FTZipper -> IO FTZipper
    deleteLocal' z@(File{}, _) = removeIfExists (fullPath syncDir z) >> return z
    deleteLocal' z@(Dir name _ _, _) = do
        let dirPath = fullPath syncDir z
        isDirectory <- doesDirectoryExist dirPath
        unless (not isDirectory) $ do
            dircont <- getDirContents dirPath
            let nonEmptyDir = (not . null) dircont
            unless (name == syncDirName || nonEmptyDir) $ removeDirectoryRecursive dirPath
        return z

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

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

deleteDoc :: DP.Document -> ApiAction ()
deleteDoc document = do
    (manager, aToken, csrfToken, _) <- ask
    liftResourceT $ deleteDocument aToken manager csrfToken document

deleteFolder :: DP.Folder -> ApiAction ()
deleteFolder folder = do
    (manager, aToken, csrfToken, _) <- ask
    liftResourceT $ Api.deleteFolder aToken manager csrfToken folder

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


initLocalState :: IO (FilePath, FilePath, SyncState, FileTree)
initLocalState = do
    syncDir <- getOrCreateSyncDir
    let syncFile = getSyncFile syncDir
    previousState <- readSyncState syncFile
    localState <- getLocalState syncDir
    return (syncDir, syncFile, previousState, localState)

checkLocalChange :: IO Bool
checkLocalChange = do
    (_, _, previousState, localState) <- initLocalState
    let added = localState `treeDiff` localSyncState previousState
    let deleted = localSyncState previousState `treeDiff` localState
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
      let added = remoteState `treeDiff` remoteSyncState previousState
      let deleted = remoteSyncState previousState `treeDiff` remoteState
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
        let toDownload = newOnServer localState (localSyncState previousState) remoteState
        liftIO $ debugLog $ "down " ++ show toDownload
        let toUpload = newLocal localState (remoteSyncState previousState) remoteState
        liftIO $ debugLog $ "up " ++ show toUpload
        let toDeleteLocal = deletedOnServer (remoteSyncState previousState) remoteState
        liftIO $ debugLog $ "delLocal " ++ show toDeleteLocal
        let toDeleteRemote = (`combineWith` remoteState) <$> deletedLocal (localSyncState previousState) localState
        liftIO $ debugLog $ "delRemote " ++ show toDeleteRemote
        maybe (return ()) (download syncDir . ftZipper) toDownload
        maybe (return ()) (upload syncDir . ftZipper) toUpload
        liftIO $ maybe (return ()) (deleteLocal syncDir . ftZipper) toDeleteLocal
        maybe (return ()) (deleteRemote . ftZipper) toDeleteRemote
        newLocalState <- liftIO $ getLocalState syncDir
        newRemoteState <- getRemoteState
        liftIO $ writeSyncState syncFile (SyncState newLocalState newRemoteState)

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
debugLog = putStrLn
-- debugLog _ = return ()
