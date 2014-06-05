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


guiSync :: Int -> IO ()
guiSync runNumber = do
    syncDir <- getUserSyncDir
    F.createSyncDir syncDir
    hasLocalChanges <- checkLocalChange
    let fullSync = runNumber `mod` 6 == 0
    when (hasLocalChanges || fullSync) sync

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
    let syncFile = F.syncFile syncDir
    lastState <- F.readSyncFile syncFile
    files <- F.existingFiles syncDir
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
    archiveLink <- liftIO $ linkOrException "document_archive" $ A.link account
    documents <- getDocs manager token archiveLink
    let syncFile = F.syncFile syncDir
    files <- liftIO $ F.existingFiles syncDir
    lastState <- liftIO $ F.readSyncFile syncFile
    let (docsToDownload, newFiles, deletedFiles) = F.syncDiff lastState files documents
    liftIO $ void $ mapM debugLog [
                            "download: [" ++ intercalate ", " (map D.filename docsToDownload) ++ "]",
                            "upload: [" ++ intercalate ", " newFiles ++ "]",
                            "deleted: [" ++ intercalate ", " deletedFiles ++ "]" ]
    downloadAll token manager syncDir docsToDownload
    uploadLink <- liftIO $ linkOrException "upload_document" $ A.link account
    uploadAll token manager uploadLink (csrfToken root) (map (combine syncDir) newFiles)
    liftIO $ F.deleteAll syncDir deletedFiles
    newState <- liftIO $ F.existingFiles syncDir
    liftIO $ F.writeSyncFile syncFile newState
    liftIO $ debugLog "Finished"

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
