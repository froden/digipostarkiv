{-# LANGUAGE OverloadedStrings #-}

module Sync2 where

import Network.HTTP.Conduit
import Control.Monad.Error
import Control.Monad.Trans.Resource
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Map (Map)
import qualified Data.Map as Map

import Api
import qualified ApiTypes as DP
import Http (AccessToken)
import File2
import Oauth
import Sync (handleTokenRefresh, initLocalState)

type CSRFToken = String
type ApiAction a = ReaderT (Manager, AccessToken, CSRFToken, DP.Mailbox) (ResourceT IO) a

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
        mapFileToRemoteFile fldr doc = let file = fileFromRemote fldr doc
                                         in (file, RemoteFile file fldr doc)

sync :: IO (Map File RemoteFile)
sync = loadAccessToken >>= handleTokenRefresh sync'

sync' :: AccessToken -> IO (Map File RemoteFile)
sync' token = runResourceT $ do
      manager <- liftIO $ newManager conduitManagerSettings
      (root, _, mbox) <- getAccount manager token
      runReaderT getRemoteState (manager, token, DP.csrfToken root, mbox)