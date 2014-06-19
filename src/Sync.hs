{-# LANGUAGE OverloadedStrings #-}

module Sync where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Control.Monad.Error
import Control.Monad.Trans.Resource
import System.FilePath.Posix
import Data.List
import System.Directory
import Control.Exception

import Api
import Root
import qualified Account as A
import qualified Document as D
import qualified File as F
import Oauth
import Http (AccessToken)
import Error
import qualified Folder as F
import qualified Mailbox as M

type CSRFToken = String

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


checkLocalChange :: IO Bool
checkLocalChange = do
    syncDir <- getOrCreateSyncDir
    isLocalFolderChange <- checkLocalChange' F.existingFolders syncDir
    localFolders <- F.existingFolders syncDir
    fileChanges <- mapM (checkLocalChange' F.existingFiles) $ map (combine syncDir) localFolders
    return $ or $ isLocalFolderChange : fileChanges
    

checkLocalChange' :: (FilePath -> IO [FilePath]) -> FilePath -> IO Bool
checkLocalChange' listFunc syncDir = do
    let syncFile = F.syncFile syncDir
    lastState <- F.readSyncFile syncFile
    files <- listFunc syncDir
    return $ lastState /= files    

checkRemoteChange :: IO Bool
checkRemoteChange = loadAccessToken >>= handleTokenRefresh checkRemoteChange'

checkRemoteChange' :: AccessToken -> IO Bool
checkRemoteChange' token = runResourceT $ do
    syncDir <- liftIO getOrCreateSyncDir
    manager <- liftIO $ newManager conduitManagerSettings
    (_, account) <- getAccount manager token
    archiveLink <- liftIO $ linkOrException "document_archive" $ A.link account
    documents <- getDocs manager token archiveLink
    let syncFile = F.syncFile syncDir
    files <- liftIO $ F.existingFiles syncDir
    lastState <- liftIO $ F.readSyncFile syncFile
    let (docsToDownload, newFiles, deletedFiles) = F.syncDiff lastState files documents
    return $ not $ null docsToDownload && null newFiles && null deletedFiles

sync :: IO ()
sync = loadAccessToken >>= handleTokenRefresh sync'

sync' :: AccessToken -> IO ()
sync' token = runResourceT $ do
    syncDir <- liftIO getOrCreateSyncDir
    manager <- liftIO $ newManager conduitManagerSettings
    (root, account) <- getAccount manager token
    -- archiveLink <- liftIO $ linkOrException "document_archive" $ A.link account
    -- documents <- getDocs manager token archiveLink
    let mbox = head $ mailbox root
    let csrf = csrfToken root
    syncFolders manager csrf token mbox syncDir
    let folders = F.folder $ M.folders mbox
    void $ mapM (syncFilesInFolder manager csrf token syncDir) folders

syncFolders :: Manager -> CSRFToken -> AccessToken -> M.Mailbox -> FilePath -> ResourceT IO ()
syncFolders manager csrf token mbox syncDir = do
    let folders = F.folder $ M.folders mbox
    let syncFile = F.syncFile syncDir
    localFolders <- liftIO $ F.existingFolders syncDir
    liftIO $ debugLog (show localFolders)
    lastState <- liftIO $ F.readSyncFile syncFile
    let (newRemoteFolders, newLocalFolders, deletedRemoteFolders) = F.folderDiff lastState localFolders folders
    liftIO $ void $ mapM debugLog [
                            "newFoldersOnServer: [" ++ intercalate ", " (map F.name newRemoteFolders) ++ "]",
                            "newFoldersOnDisk: [" ++ intercalate ", " newLocalFolders ++ "]",
                            "deletedOnServer: [" ++ intercalate ", " deletedRemoteFolders ++ "]" ]
    liftIO $ F.createFolders syncDir $ map F.name newRemoteFolders
    createFolderLink <- liftIO $ linkOrException "create_folder" $ M.link mbox
    void $ mapM (createFolder token manager createFolderLink csrf) newLocalFolders
    liftIO $ F.deleteAllFolders syncDir deletedRemoteFolders
    newState <- liftIO $ F.existingFolders syncDir
    liftIO $ F.writeSyncFile syncFile newState


syncFilesInFolder :: Manager -> CSRFToken -> AccessToken -> FilePath -> F.Folder -> ResourceT IO ()
syncFilesInFolder manager csrf token parent folder = do
    let syncDir = combine parent $ F.name folder
    folderLink <- liftIO $ linkOrException "self" $ F.link folder
    fullFolder <- getFolder token manager folderLink
    let documents = filter D.uploaded $ D.document $ case F.documents fullFolder of
                    Just docs -> docs
                    Nothing -> D.Documents []
    let syncFile = F.syncFile syncDir
    files <- liftIO $ F.existingFiles syncDir
    lastState <- liftIO $ F.readSyncFile syncFile
    let (docsToDownload, newFiles, deletedFiles) = F.syncDiff lastState files documents
    liftIO $ void $ mapM debugLog [
                            "download: [" ++ intercalate ", " (map D.filename docsToDownload) ++ "]",
                            "upload: [" ++ intercalate ", " newFiles ++ "]",
                            "deleted: [" ++ intercalate ", " deletedFiles ++ "]" ]
    downloadAll token manager syncDir docsToDownload
    uploadLink <- liftIO $ linkOrException "upload_document" $ F.link fullFolder
    uploadAll token manager uploadLink csrf (map (combine syncDir) newFiles)
    liftIO $ F.deleteAll syncDir deletedFiles
    newState <- liftIO $ F.existingFiles syncDir
    liftIO $ F.writeSyncFile syncFile newState

getUserSyncDir :: IO FilePath
getUserSyncDir = do
    homedir <- getHomeDirectory
    return $ combine homedir "Digipostarkiv"

getOrCreateSyncDir :: IO FilePath
getOrCreateSyncDir = do
    syncDir <- getUserSyncDir
    F.createSyncDir syncDir
    return syncDir

debugLog :: String -> IO ()
debugLog = putStrLn
--debugLog _ = return ()
