{-# LANGUAGE OverloadedStrings #-}

module Sync2 where

import Network.HTTP.Conduit
import Control.Monad.Error
import Control.Monad.Trans.Resource
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath.Posix
import System.Directory
import Data.List

import Api
import qualified ApiTypes as DP
import Http (AccessToken)
import File2
import Oauth
import Sync (handleTokenRefresh, initLocalState)

type CSRFToken = String
type ApiAction a = ReaderT (Manager, AccessToken, CSRFToken, DP.Mailbox) (ResourceT IO) a

syncDirName :: String
syncDirName = "Digipostarkiv"

getRemoteState :: ApiAction (Map File RemoteFile)
getRemoteState = do
    (_, _, _, mbox) <- ask
    let mboxFolders = (DP.folder . DP.folders) mbox
    contents <- mapM getFolderContents mboxFolders
    return $ Map.unions contents
  where
    getFolderContents :: DP.Folder -> ApiAction (Map File RemoteFile)
    getFolderContents folder = do
        (manager, aToken, _, _) <- ask
        folderLink <- liftIO $ linkOrException "self" $ DP.folderLinks folder
        fullFolder <- liftResourceT $ getFolder aToken manager folderLink
        let folderDocuments = filter DP.uploaded (DP.documentInFolder fullFolder)
        let dir = (dirFromFolder folder, remoteDirFromFolder folder)
        let files = map (mapFileToRemoteFile folder) folderDocuments
        return $ Map.fromList (dir : files)
      where
        mapFileToRemoteFile fldr doc = let file = fileFromFolderDoc fldr doc
                                         in (file, RemoteFile file fldr doc)

getDirContents :: FilePath -> IO [FilePath]
getDirContents dirPath = do
        names <- getDirectoryContents dirPath
        return $ filter (not . specialFiles) names
    where specialFiles f = "." `isPrefixOf` f || f `elem` [".", ".."]

getLocalState :: FilePath -> FilePath -> IO (Set File)
getLocalState syncDirPath dirPath = do
        properNames <- getDirContents dirPath
        content <- forM properNames $ \name -> do
            let subPath = dirPath </> name
            isDirectory <- doesDirectoryExist subPath
            if isDirectory
                then getLocalState syncDirPath subPath
                else return $ Set.singleton (File $ makeRelative syncDirPath subPath)
        let contentSet = Set.unions content
        let dir = Dir $ addTrailingPathSeparator (makeRelative syncDirPath dirPath)
        return $ Set.insert dir contentSet

sync :: IO ()
sync = loadAccessToken >>= handleTokenRefresh sync'

sync' :: AccessToken -> IO ()
sync' token = runResourceT $ do
      manager <- liftIO $ newManager conduitManagerSettings
      (root, _, mbox) <- getAccount manager token
      flip runReaderT (manager, token, DP.csrfToken root, mbox) $ do
        remoteState <- getRemoteState
        let remoteFiles = getFileSetFromMap remoteState
        let localFiles = getLocalState
        return ()