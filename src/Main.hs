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
        session <- auth manager $ credentialsFromConfig config
        (root, account) <- getAccount manager session
        documents <- getDocs manager session $ linkOrException (A.archiveLink account)
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

auth :: Manager -> Auth -> ResourceT IO CookieJar
auth manager credentials = do
    authRes <- authenticate manager credentials
    return $ responseCookieJar authRes

getAccount :: Manager -> CookieJar -> ResourceT IO (Root, A.Account)
getAccount manager cookies = do
    root <- getRoot manager $ Just cookies
    return (root, primaryAccount root)

getDocs :: Manager -> CookieJar -> Link -> ResourceT IO [D.Document]
getDocs manager cookies docsLink = do
    allDocuments <- getDocuments (Just cookies) manager docsLink
    --we only want documents uploaded by the user
    return $ filter D.uploaded (D.document allDocuments)    

credentialsFromConfig :: Config -> Auth
credentialsFromConfig conf = Auth (Config.username conf) (Config.password conf)