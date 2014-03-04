{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
import Control.Monad.Error
import Control.Monad.Trans.Resource
import System.FilePath.Posix
import Data.List

import Api
import Root
import Link
import qualified Account as A
import qualified Document as D
import qualified File as F
import qualified Config as C

main :: IO ()
main = do
    config <- C.readConfigFile "sync.conf"
    --putStrLn $ show config
    putStrLn $ "Using syncDir: " ++ C.syncDir config
    sync config

sync :: C.Config -> IO()
sync config = runResourceT $ do
    manager <- liftIO $ newManager conduitManagerSettings
    session <- authenticate manager $ authFromConfig config
    (root, account) <- getAccount manager session
    archiveLink <- A.archiveLink account
    documents <- getDocs manager session archiveLink
    let syncDir = C.syncDir config
    let syncFile = F.syncFile syncDir
    files <- liftIO $ F.existingFiles syncDir
    lastState <- liftIO $ F.readSyncFile syncFile
    let (docsToDownload, newFiles, deletedFiles) = F.syncDiff lastState files documents 
    liftIO $ putStrLn $ "download: [" ++ intercalate ", " (map D.filename docsToDownload) ++ "]"
    liftIO $ putStrLn $ "upload: [" ++ intercalate ", " newFiles ++ "]"
    liftIO $ putStrLn $ "deleted: [" ++ intercalate ", " deletedFiles ++ "]"
    downloadAll (Just session) manager syncDir docsToDownload
    let Just uploadLink = linkWithRel "upload_document" $ A.link account
    uploadAll (Just session) manager uploadLink (csrfToken root) (map (combine syncDir) newFiles)
    liftIO $ F.deleteAll syncDir deletedFiles
    newState <- liftIO $ F.existingFiles syncDir
    liftIO $ F.writeSyncFile syncFile newState
    liftIO $ putStrLn "Finished"
