{-# LANGUAGE OverloadedStrings #-}

module Sync where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Control.Monad.Error
import Control.Monad.Trans.Resource
import System.FilePath.Posix
import Data.List
import Data.Maybe
import System.Directory
import Control.Exception

import Api
import qualified ApiTypes as DP
import qualified File as F
import Oauth
import Http (AccessToken)
import Error

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
    isLocalFolderChange <- checkLocalChange' F.existingFolders' syncDir
    localFolders <- F.existingFolders syncDir
    fileChanges <- mapM (checkLocalChange' F.existingFiles' . combine syncDir) localFolders
    return $ or $ isLocalFolderChange : fileChanges
    

checkLocalChange' :: (FilePath -> IO [F.Filename]) -> FilePath -> IO Bool
checkLocalChange' listFunc syncDir = do
    let syncFile = F.syncFile syncDir
    lastState <- F.readSyncFile' syncFile
    files <- listFunc syncDir
    return $ lastState /= files    

checkRemoteChange :: IO Bool
checkRemoteChange = loadAccessToken >>= handleTokenRefresh checkRemoteChange'

--TODO: Fixme folder support
checkRemoteChange' :: AccessToken -> IO Bool
checkRemoteChange' token = runResourceT $ do
    syncDir <- liftIO getOrCreateSyncDir
    manager <- liftIO $ newManager conduitManagerSettings
    (root, _) <- getAccount manager token
    let mbox = head $ DP.mailbox root
    let folders = (DP.folder . DP.folders) mbox
    isRemoteFolderChange <- checkRemoteChange'' (return folders) syncDir
    remoteFileChanges <- mapM (\f -> (checkRemoteChange'' (docsInFolder token manager f) . combine syncDir . F.filenameStr) f) folders
    return $ or $ isRemoteFolderChange : remoteFileChanges
    where
        docsInFolder token' manager folder = do
            folderLink <- liftIO $ linkOrException "self" $ DP.folderLinks folder
            fullFolder <- getFolder token' manager folderLink
            return $ filter DP.uploaded $ DP.document $ fromMaybe (DP.Documents []) (DP.documents fullFolder)

checkRemoteChange'' :: (F.File a) => ResourceT IO [a] -> FilePath -> ResourceT IO Bool
checkRemoteChange'' listFunc syncDir = do
    remoteFiles <- listFunc
    let syncFile = F.syncFile syncDir
    lastState <- liftIO $ F.readSyncFile' syncFile
    return $ not $ null (remoteFiles `F.diff` lastState) && null (lastState `F.diff` remoteFiles)

sync :: IO ()
sync = loadAccessToken >>= handleTokenRefresh sync'

--problem: Hvis man kopierer en katalog får man med .sync-filen og denne vil da ikke bli lastet opp korekt.
sync' :: AccessToken -> IO ()
sync' token = runResourceT $ do
    syncDir <- liftIO getOrCreateSyncDir
    manager <- liftIO $ newManager conduitManagerSettings
    (root, _) <- getAccount manager token
    let mbox = head $ DP.mailbox root
    let csrf = DP.csrfToken root
    syncFolders manager csrf token mbox syncDir
    mailboxLink <- liftIO $ linkOrException "self" $ DP.mailboxLinks mbox
    liftIO $ debugLog $ "getting mailbox: " ++ (show mailboxLink)
    --bug i digipost: får 404 når man henter mailbox med self-link
    --mbox' <- getMailbox token manager mailboxLink
    (root', _) <- getAccount manager token
    let mbox' = head $ DP.mailbox root'
    liftIO $ debugLog "got mailbox"
    let folders = (DP.folder . DP.folders) mbox'
    void $ mapM (syncFilesInFolder manager csrf token syncDir) folders

syncFolders :: Manager -> CSRFToken -> AccessToken -> DP.Mailbox -> FilePath -> ResourceT IO ()
syncFolders manager csrf token mbox syncDir = do
    let folders = DP.folder $ DP.folders mbox
    let syncFile = F.syncFile syncDir
    localFolders <- liftIO $ F.existingFolders' syncDir
    lastState <- liftIO $ F.readSyncFile' syncFile
    let (newRemoteFolders, newLocalFolders, deletedRemoteFolders) = F.syncDiff lastState localFolders folders
    liftIO $ logDiff newRemoteFolders newLocalFolders deletedRemoteFolders
    liftIO $ F.createFolders syncDir $ map DP.folderName newRemoteFolders
    createFolderLink <- liftIO $ linkOrException "create_folder" $ DP.mailboxLinks mbox
    void $ mapM (createFolder token manager createFolderLink csrf . F.name) newLocalFolders
    liftIO $ F.deleteAllFolders syncDir $ map F.name deletedRemoteFolders
    newState <- liftIO $ F.existingFolders syncDir
    liftIO $ F.writeSyncFile syncFile newState


syncFilesInFolder :: Manager -> CSRFToken -> AccessToken -> FilePath -> DP.Folder -> ResourceT IO ()
syncFilesInFolder manager csrf token parent folder = do
    liftIO $ debugLog $ "Syncing: " ++ DP.folderName folder
    let syncDir = combine parent $ DP.folderName folder
    folderLink <- liftIO $ linkOrException "self" $ DP.folderLinks folder
    fullFolder <- getFolder token manager folderLink
    let documents = filter DP.uploaded $ DP.document $ fromMaybe (DP.Documents []) (DP.documents fullFolder)
    let syncFile = F.syncFile syncDir
    files <- liftIO $ F.existingFiles' syncDir
    lastState <- liftIO $ F.readSyncFile' syncFile
    let (docsToDownload, newFiles, deletedFiles) = F.syncDiff lastState files documents
    liftIO $ logDiff docsToDownload newFiles deletedFiles
    downloadAll token manager syncDir docsToDownload
    uploadLink <- liftIO $ linkOrException "upload_document" $ DP.folderLinks fullFolder
    uploadAll token manager uploadLink csrf $ map (combine syncDir . F.name) newFiles
    liftIO $ F.deleteAll syncDir $ map F.name deletedFiles
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

logDiff :: (F.File a, F.File b) => [a] -> [b] -> [b] -> IO ()
logDiff download upload deleted = void $ mapM debugLog [
                            "download: [" ++ intercalate ", " (map F.filenameStr download) ++ "]",
                            "upload: [" ++ intercalate ", " (map F.filenameStr upload) ++ "]",
                            "deleted: [" ++ intercalate ", " (map F.filenameStr deleted) ++ "]" ]

debugLog :: String -> IO ()
debugLog = putStrLn
--debugLog _ = return ()
