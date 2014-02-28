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
import File
import Config

main :: IO ()
main = do
    config <- readConfigFile "sync.conf"
    --putStrLn $ show config
    putStrLn $ "Using syncDir: " ++ syncDir config
    sync config

sync :: Config -> IO()
sync config = runResourceT $ do
        manager <- liftIO $ newManager conduitManagerSettings
        session <- authenticate manager $ authFromConfig config
        (root, account) <- getAccount manager session
        archiveLink <- A.archiveLink account
        documents <- getDocs manager session archiveLink
        files <- liftIO $ existingFiles (syncDir config)
        let docsToDownload = D.notDownloaded files documents
        liftIO $ putStrLn $ "download: [" ++ intercalate ", " (map D.filename docsToDownload) ++ "]"
        downloadAll (Just session) manager (syncDir config) docsToDownload
        let newFiles = D.notUploaded files documents
        liftIO $ putStrLn $ "upload: [" ++ intercalate ", " newFiles ++ "]"
        --upload newFiles
        let Just uploadLink = linkWithRel "upload_document" $ A.link account
        uploadAll (Just session) manager uploadLink (csrfToken root) (map (combine (syncDir config)) newFiles)
        liftIO $ putStrLn "Finished"
