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
import qualified Config as C
import Oauth
import Http (Session, AccessToken)
import Error


guiSync :: Int -> IO ()
guiSync runNumber = do
    userHome <- getHomeDirectory
    let config = C.defaultConfig userHome
    F.createSyncDir $ C.syncDir config
    hasLocalChanges <- localChange config
    let fullSync = runNumber `mod` 6 == 0
    if hasLocalChanges || fullSync then
        loadAccessToken >>= gsync config
    else
        return ()

gsync :: C.Config -> AccessToken -> IO ()
gsync config token = do
    res <- try (sync config (Right token))
    case res of
        Left (StatusCodeException (Status 403 _) hdrs _) -> do
            liftIO $ debugLog $ "403 status" ++ (show hdrs)
            newToken <- refreshAccessToken token --TODO: exceptions?
            liftIO $ storeAccessToken newToken
            gsync config newToken  --TODO: retry count??
        Left (StatusCodeException (Status 401 _) hdrs _) -> do
            liftIO $ debugLog $ "401 status " ++ (show hdrs)
            throwIO NotAuthenticated
        Left e -> throwIO $ HttpFailed e
        Right _ -> return ()

localChange :: C.Config -> IO Bool
localChange config = do
    let syncDir = C.syncDir config
    let syncFile = F.syncFile syncDir
    files <- F.existingFiles syncDir
    lastState <- F.readSyncFile syncFile
    return $ not $ lastState == files

sync :: C.Config -> Session -> IO ()
sync config session = runResourceT $ do
    manager <- liftIO $ newManager conduitManagerSettings
    (root, account) <- getAccount manager session
    archiveLink <- liftIO $ linkOrException "document_archive" $ A.link account
    documents <- getDocs manager session archiveLink
    let syncDir = C.syncDir config
    let syncFile = F.syncFile syncDir
    files <- liftIO $ F.existingFiles syncDir
    lastState <- liftIO $ F.readSyncFile syncFile
    let (docsToDownload, newFiles, deletedFiles) = F.syncDiff lastState files documents
    liftIO $ void $ mapM debugLog [
                            "download: [" ++ intercalate ", " (map D.filename docsToDownload) ++ "]",
                            "upload: [" ++ intercalate ", " newFiles ++ "]",
                            "deleted: [" ++ intercalate ", " deletedFiles ++ "]" ]
    downloadAll session manager syncDir docsToDownload
    uploadLink <- liftIO $ linkOrException "upload_document" $ A.link account
    uploadAll session manager uploadLink (csrfToken root) (map (combine syncDir) newFiles)
    liftIO $ F.deleteAll syncDir deletedFiles
    newState <- liftIO $ F.existingFiles syncDir
    liftIO $ F.writeSyncFile syncFile newState
    liftIO $ debugLog "Finished"

debugLog :: String -> IO ()
debugLog = putStrLn
--debugLog _ = return ()
