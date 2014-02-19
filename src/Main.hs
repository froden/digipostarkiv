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
    runResourceT $ do
        manager <- liftIO $ newManager conduitManagerSettings
        authRes <- authenticate manager config
        let cookies = responseCookieJar authRes
        root <- getRoot (Just cookies) manager
        let account = primaryAccount root
        let Just inboxLink = linkWithRel "document_archive" $ A.link account
        allDocuments <- getDocuments (Just cookies) manager inboxLink
        --we only want documents uploaded by the user
        let documents = filter D.uploaded (D.document allDocuments)
        files <- liftIO $ existingFiles (syncDir config)
        --download files from server
        let docsToDownload = D.notDownloaded files documents
        liftIO $ putStrLn $ "download: [" ++ intercalate ", " (map D.filename docsToDownload) ++ "]"
        downloadAll (Just cookies) manager (syncDir config) docsToDownload
        let newFiles = D.notUploaded files documents
        liftIO $ putStrLn $ "upload: [" ++ intercalate ", " newFiles ++ "]"
        --upload newFiles
        let Just uploadLink = linkWithRel "upload_document" $ A.link account
        uploadAll (Just cookies) manager uploadLink (csrfToken root) (map (combine (syncDir config)) newFiles)
        liftIO $ putStrLn "Finished"

