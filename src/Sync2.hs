{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Sync2 where

import Network.HTTP.Conduit
import Control.Monad.Error
import Control.Monad.Trans.Resource
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath.Posix
import System.Directory
import System.IO.Error
import Data.List
import Data.Maybe
import Control.Exception.Lifted
import Text.Read (readMaybe)
import Data.Either
import Data.Time
import System.Locale

import Api
import qualified ApiTypes as DP
import Http (AccessToken)
import File2
import Oauth

type CSRFToken = String
type ApiAction a = ReaderT (Manager, AccessToken, CSRFToken, DP.Mailbox) (ResourceT IO) a

data SyncState = SyncState {localSyncState :: Set File, remoteSyncState :: Set File} deriving (Show, Read)

syncDirName :: String
syncDirName = "Digipostarkiv"

readSyncState :: FilePath -> IO SyncState
readSyncState syncFile = do
        r <- try (readFile syncFile) :: IO (Either IOException String)
        case r of
            Right content -> return $ fromMaybe emptyState (readMaybe content)
            Left _ -> return emptyState
    where emptyState = SyncState Set.empty Set.empty

writeSyncState :: FilePath -> SyncState -> IO ()
writeSyncState syncFile state = do
    let tempFile = syncFile ++ ".tmp"
    writeFile tempFile (show state)
    renameFile tempFile syncFile

getRemoteState :: ApiAction (Map Path RemoteFile)
getRemoteState = do
    (_, _, _, mbox) <- ask
    let mboxFolders = (DP.folder . DP.folders) mbox
    contents <- mapM getFolderContents mboxFolders
    inboxContents <- getInboxContents
    let allFolders = Map.unions contents
    return $ Map.union inboxContents allFolders

getInboxContents :: ApiAction (Map Path RemoteFile)
getInboxContents = do
    (manager, aToken, _, mbox) <- ask
    inboxLink <- liftIO $ linkOrException "document_inbox" $ DP.mailboxLinks mbox
    documents <- liftResourceT $ getDocuments aToken manager inboxLink
    let inboxDocuments = filter DP.uploaded (DP.document documents)
    let dirPath = Path "./"
    let dir = Dir dirPath
    uploadLink <- liftIO $ linkOrException "upload_document_to_inbox" $ DP.mailboxLinks mbox
    let folder = DP.Folder "" "" [uploadLink { DP.rel = "upload_document" }] (Just $ DP.Documents inboxDocuments)
    let remoteDir = RemoteDir dir folder
    let files = map (mapFileToRemoteFile folder) inboxDocuments
    return $ Map.fromList $ (dirPath, remoteDir) : files

mapFileToRemoteFile :: DP.Folder -> DP.Document -> (Path, RemoteFile)
mapFileToRemoteFile fldr doc = let file = fileFromFolderDoc fldr doc
                               in (File2.path file, RemoteFile file fldr doc)

getFolderContents :: DP.Folder -> ApiAction (Map Path RemoteFile)
getFolderContents folder = do
    (manager, aToken, _, _) <- ask
    folderLink <- liftIO $ linkOrException "self" $ DP.folderLinks folder
    fullFolder <- liftResourceT $ getFolder aToken manager folderLink
    let folderDocuments = filter DP.uploaded (DP.documentInFolder fullFolder)
    let dir = (pathFromFolder folder, remoteDirFromFolder folder)
    let files = map (mapFileToRemoteFile folder) folderDocuments
    return $ Map.fromList (dir : files)


getDirContents :: FilePath -> IO [FilePath]
getDirContents dirPath = do
        names <- getDirectoryContents dirPath
        return $ filter (not . specialFiles) names
    where specialFiles f = "." `isPrefixOf` f || f `elem` [".", ".."]

getLocalState :: FilePath -> IO (Set File)
getLocalState syncDirPath = Set.insert (Dir (Path "./")) <$> findFilesRecursive syncDirPath
    where
        relativeToSyncDir = makeRelative syncDirPath
        findFilesRecursive dirPath = do
            properNames <- getDirContents dirPath
            content <- forM properNames $ \name -> do
                let subPath = dirPath </> name
                isDirectory <- doesDirectoryExist subPath
                modificationTime <- getModificationTime subPath
                if isDirectory
                    then findFilesRecursive subPath
                    else return $ Set.singleton $ File (Path $ relativeToSyncDir subPath) modificationTime
            let contentSet = Set.unions content
            let filePath = addTrailingPathSeparator (relativeToSyncDir dirPath)
            let dir = Dir $ Path filePath
            return $ if filePath == "./" then contentSet else Set.insert dir contentSet

initLocalState :: IO (FilePath, FilePath, SyncState, Set File)
initLocalState = do
    syncDir <- getOrCreateSyncDir
    let syncFile = getSyncFile syncDir
    previousState <- readSyncState syncFile
    localState <- getLocalState syncDir
    return (syncDir, syncFile, previousState, localState)

applyChangesLocal :: FilePath -> Map Path RemoteFile -> [Change] -> ApiAction [Change]
applyChangesLocal syncDir remoteFiles = fmap catMaybes . mapM applyChange
    where
        absoluteTo = combine syncDir
        applyChange :: Change -> ApiAction (Maybe Change)
        applyChange currentChange@(Created file) =
            let
                path = File2.path file
                absoluteTargetFile = absoluteTo (filePath path)
                remoteFile = Map.lookup path remoteFiles
            in
                case remoteFile of
                    Just r -> do
                        res <- try (download r absoluteTargetFile) :: ApiAction (Either ApiException ())
                        if isRight res then do
                            newFileModified <- liftIO $ getModificationTime absoluteTargetFile
                            case r of
                                RemoteFile {} -> return $ Just currentChange {file = File path newFileModified}
                                RemoteDir {} -> return $ Just currentChange {file = Dir path}
                        else
                            return Nothing
                    Nothing -> error $ "file to download not found: " ++ show (File2.path file)
        applyChange (Deleted file) = do
            res <- liftIO (try $ deleteLocal syncDir (File2.path file) :: IO (Either IOException File))
            case res of
                Right file -> return $ Just (Deleted file)
                Left e -> liftIO $ printError e >> return Nothing

applyChangesRemote :: FilePath -> Map Path RemoteFile -> [Change] -> ApiAction [Change]
applyChangesRemote syncDir rState = fmap catMaybes . applyChanges rState
    where
        applyChanges :: Map Path RemoteFile -> [Change] -> ApiAction [Maybe Change]
        applyChanges _ [] = return []
        applyChanges remoteState (headChange:tailChanges) = do
            res <- try (applyChange remoteState headChange) :: ApiAction (Either SomeException (Maybe RemoteChange))
            case res of
                Right remoteChangeMaybe ->
                    case remoteChangeMaybe of
                        --adds newly created folder to remote state in case subsequent upload to that folder
                        Just (RemoteChange (Created (Dir path)) newFolder@(RemoteDir dir _)) -> do
                            let newState = Map.insert path newFolder remoteState
                            appliedChanges <- applyChanges newState tailChanges
                            return $ Just (Created dir) : appliedChanges
                        Just (RemoteChange change remoteFile) -> do
                            appliedChanges <- applyChanges remoteState tailChanges
                            return $ Just change {file = getFile remoteFile} : appliedChanges
                        Nothing -> applyChanges remoteState tailChanges
                Left exception -> liftIO (printError exception) >> applyChanges remoteState tailChanges
            where
                applyChange :: Map Path RemoteFile -> Change -> ApiAction (Maybe RemoteChange)
                applyChange remoteFiles currentChange@(Created (File currentPath@(Path filePath) _)) = do
                    let parentDirPath = addTrailingPathSeparator . takeDirectory $ filePath
                    let absoluteFilePath = combine syncDir filePath
                    let parentDirMaybe = Map.lookup (Path parentDirPath) remoteFiles
                    case parentDirMaybe of
                        Just parentDir -> do
                            upload parentDir absoluteFilePath
                            uploadedDoc@RemoteFile {} <- getUploadedDocument parentDir currentPath
                            -- return empty document because upload does not return document or link
                            return $ Just (RemoteChange currentChange uploadedDoc)
                        Nothing -> error $ "no parent dir: " ++ parentDirPath
                --For now Digipost only support one level of folders
                applyChange remoteFiles currentChange@(Created (Dir path@(Path dirPath))) = do
                    let folderName = takeFileName . takeDirectory $ dirPath
                    (manager, aToken, csrfToken, mbox) <- ask
                    createFolderLink <- liftIO $ linkOrException "create_folder" $ DP.mailboxLinks mbox
                    newFolder <- liftResourceT $ createFolder aToken manager createFolderLink csrfToken folderName
                    return $ Just (RemoteChange currentChange (RemoteDir (Dir path) newFolder))
                applyChange remoteFiles currentChange@(Deleted file) = do
                    let path = File2.path file
                    let remoteFileMaybe = Map.lookup path remoteFiles
                    case remoteFileMaybe of
                        Just remoteFile -> do
                            deleteRemote remoteFile
                            return $ Just (RemoteChange currentChange remoteFile)
                        --assume allready deleted
                        --Nothing -> return $ Just (RemoteChange currentChange Nothing)
                        Nothing -> error $ "remote file not found: " ++ show path

getUploadedDocument :: RemoteFile -> Path -> ApiAction RemoteFile
getUploadedDocument (RemoteDir _ parentFolder) path = do
    contents <- getFolderContents parentFolder
    return $ fromMaybe (error "expected to find uploaded document") (Map.lookup path contents)
getUploadedDocument d f = error $ "parent dir is file or file is dir:\n" ++ show d ++ "\n" ++ show f

upload :: RemoteFile -> FilePath -> ApiAction ()
upload (RemoteDir _ parentFolder) absoluteFilePath = do
    uploadLink <- liftIO $ linkOrException "upload_document" (DP.folderLinks parentFolder)
    (manager, aToken, csrf, _) <- ask
    liftResourceT $ uploadFileMultipart aToken manager uploadLink csrf absoluteFilePath
upload _ _ = error "parent dir cannot be a file"

deleteRemote :: RemoteFile -> ApiAction ()
deleteRemote (RemoteFile _ _ document) = do
    (manager, aToken, csrf, _) <- ask
    liftResourceT $ deleteDocument aToken manager csrf document
deleteRemote (RemoteDir _ folder) = do
    (manager, aToken, csrf, _) <- ask
    liftResourceT $ deleteFolder aToken manager csrf folder

deleteLocal :: FilePath -> Path -> IO File
deleteLocal syncDir p@(Path path) = deleteFileOrDir
    where
        deleteFileOrDir = do
            let targetFile = syncDir </> path
            isDir <- doesDirectoryExist targetFile
            modified <- getModificationTime targetFile
            if isDir then
                removeDirectoryRecursive targetFile
            else
                removeFile targetFile
            return $ File p modified

download :: RemoteFile -> FilePath -> ApiAction ()
download (RemoteFile _ _ document) targetFile = do
    (manager, aToken, _, _) <- ask
    liftResourceT $ downloadDocument aToken manager targetFile document
download (RemoteDir _ _) targetFile = liftIO $ do
    dirExists <- doesDirectoryExist targetFile
    unless dirExists $ createDirectory targetFile

checkLocalChange :: IO Bool
checkLocalChange = do
    (_, _, previousState, localFiles) <- initLocalState
    let previousLocalFiles = localSyncState previousState
    let localChanges = computeChanges localFiles previousLocalFiles
    liftIO $ debugLog ("localChanges " ++ show localChanges)
    return $ not (null localChanges)

checkRemoteChange :: IO Bool
checkRemoteChange = loadAccessToken >>= handleTokenRefresh checkRemoteChange'

checkRemoteChange' :: AccessToken -> IO Bool
checkRemoteChange' token = do
    (_, _, previousState, _) <- initLocalState
    runResourceT $ do
        manager <- liftIO $ newManager conduitManagerSettings
        (root, _, mbox) <- getAccount manager token
        remoteState <- runReaderT getRemoteState (manager, token, DP.csrfToken root, mbox)
        let remoteFiles = getFileSetFromMap remoteState
        let previousRemoteFiles = remoteSyncState previousState
        let remoteChanges = computeChanges remoteFiles previousRemoteFiles
        liftIO $ debugLog ("remoteChange " ++ show remoteChanges)
        return $ not (null remoteChanges)

sync :: IO ()
sync = loadAccessToken >>= handleTokenRefresh sync'

sync' :: AccessToken -> IO ()
sync' token = do
    (syncDir, syncFile, previousState, localFiles) <- initLocalState
    runResourceT $ do
        manager <- liftIO $ newManager conduitManagerSettings
        (root, _, mbox) <- getAccount manager token
        flip runReaderT (manager, token, DP.csrfToken root, mbox) $ do
            remoteState <- getRemoteState
            let remoteFiles = getFileSetFromMap remoteState
            let previousRemoteFiles = remoteSyncState previousState
            let previousLocalFiles = localSyncState previousState
            let localChanges = computeChanges localFiles previousLocalFiles
            let remoteChanges = computeChanges remoteFiles previousRemoteFiles
            --server always win if conflict
            let changesToApplyLocal = remoteChanges
            let changesToApplyRemote = computeChangesToApply localChanges remoteChanges
            liftIO $ debugLog ("locaFiles " ++ show localFiles)

            liftIO $ debugLog ("locaChanges " ++ show localChanges)
            liftIO $ debugLog ("remoteChanges" ++ show remoteChanges)
            liftIO $ debugLog ("changesToApplyLocal " ++ show changesToApplyLocal)
            liftIO $ debugLog ("changesToApplyRemote" ++ show changesToApplyRemote)
            appliedLocalChanges <- applyChangesLocal syncDir remoteState changesToApplyLocal
            appliedRemoteChanges <- applyChangesRemote syncDir remoteState changesToApplyRemote
--             let appliedLocalChanges  = [] :: [Change]
--             let appliedRemoteChanges = [] :: [Change]
            --let appliedChanges = appliedLocalChanges ++ appliedRemoteChanges
            liftIO $ debugLog ("appliedLocalChanges" ++ show appliedLocalChanges)
            liftIO $ debugLog ("appliedRemoteChanges" ++ show appliedRemoteChanges)
            let newLocalState = computeNewStateFromChanges previousLocalFiles (appliedLocalChanges `union` localChanges)
            let newRemoteState = computeNewStateFromChanges previousRemoteFiles (appliedRemoteChanges `union` remoteChanges)
            liftIO $ writeSyncState syncFile (SyncState newLocalState newRemoteState)
            return ()

getUserSyncDir :: IO FilePath
getUserSyncDir = do
    homedir <- getHomeDirectory
    return $ combine homedir syncDirName

getOrCreateSyncDir :: IO FilePath
getOrCreateSyncDir = do
    syncDir <- getUserSyncDir
    createDirectoryIfMissing True syncDir
    return syncDir

getSyncFile :: FilePath -> FilePath
getSyncFile syncDir = combine syncDir ".sync"

debugLog :: String -> IO ()
debugLog = putStrLn
-- debugLog _ = return ()

printError :: Exception a => a -> IO ()
printError e = do
        let dateFormat = iso8601DateFormat (Just "%H:%M:%S")
        timestamp <- formatTime defaultTimeLocale dateFormat <$> getCurrentTime
        let msg = timestamp ++ " " ++ show e
        print msg
        logFile <- fmap (`combine` ".synclog") getUserSyncDir
        appendFile logFile $ msg ++ "\n"