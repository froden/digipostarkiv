{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}

module Sync where

import Network.HTTP.Conduit
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath.Posix
import System.Directory
import Data.List
import Data.Maybe
import Control.Exception.Lifted
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import Data.Typeable
import System.IO.Error
import Control.Concurrent.Async

import Api
import qualified ApiTypes as DP
import Http (AccessToken)
import File
import Oauth
import Db

type CSRFToken = String
type ApiAction a = ReaderT (Manager, AccessToken, CSRFToken, DP.Mailbox) (ResourceT IO) a

syncDirName :: String
syncDirName = "Digipostarkiv"

getRemoteState :: ApiAction (Map Path RemoteFile)
getRemoteState = do
    (mgr, atoken, csrf, mbox) <- ask
    let mboxFolders = (DP.folder . DP.folders) mbox
    contents <- liftIO $ mapConcurrently (\f -> runResourceT $ runReaderT (getFolderContents f) (mgr, atoken, csrf, mbox)) mboxFolders
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
                               in (File.path file, RemoteFile file fldr doc)

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
        names <- replaceSpecialChars <$> getDirectoryContents dirPath
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
                if isDirectory
                    then findFilesRecursive subPath
                    else Set.singleton <$> getLocalFile syncDirPath subPath
            let contentSet = Set.unions content
            let relativeDirPath = addTrailingPathSeparator (relativeToSyncDir dirPath)
            let dir = Dir $ Path relativeDirPath
            return $ if relativeDirPath == "./" then contentSet else Set.insert dir contentSet

getLocalFile :: FilePath -> FilePath -> IO File
getLocalFile basePath fullPath = do
    modificationTime <- getModificationTime fullPath
    let relativePath = makeRelative basePath fullPath
    return $ File (Path relativePath) modificationTime

initLocalState :: IO (FilePath, Set File, Set File, Set File)
initLocalState = do
    syncDir <- getOrCreateSyncDir
    metaDir <- getMetaDir
    (lfs, rfs) <- withDb metaDir $ \conn -> do
        initDatabase conn
        previousLocalFiles <- getLocalFiles conn
        previousRemoteFiles <- getRemoteFiles conn
        return (previousLocalFiles, previousRemoteFiles)
    localState <- getLocalState syncDir
    return (syncDir, lfs, rfs, localState)

applyChangesLocal :: FilePath -> Map Path RemoteFile -> [Change] -> ApiAction [Change]
applyChangesLocal syncDir remoteFiles = fmap catMaybes . mapM applyChange
    where
        applyChange :: Change -> ApiAction (Maybe Change)
        applyChange (Created file) = tryWithLogging $ applyCreatedToLocal syncDir remoteFiles file
        applyChange (Deleted file) = tryWithLogging $ applyDeletedToLocal syncDir file

tryWithLogging :: forall m a. (MonadIO m, MonadBaseControl IO m) => m a -> m (Maybe a)
tryWithLogging action = do
    res <- try action
    case res of
        Right v -> return $ Just v
        Left (SomeException e) -> liftIO $ warningM "Sync.tryWithLogging" (show (typeOf e) ++ ": " ++ show e) >> return Nothing

applyCreatedToLocal :: FilePath -> Map Path RemoteFile -> File -> ApiAction Change
applyCreatedToLocal syncDir remoteFiles file =
    let
        createdPath = File.path file
        absoluteTargetFile = combine syncDir (filePath createdPath)
        remoteFile = Map.lookup createdPath remoteFiles
    in
        case remoteFile of
            Just r -> liftM Created $ download r absoluteTargetFile
            Nothing -> error $ "file to download not found: " ++ show createdPath

applyDeletedToLocal :: (MonadIO m, MonadBaseControl IO m) => FilePath -> File -> m Change
applyDeletedToLocal syncDir file = do
    res <- try $ liftIO $ deleteLocal syncDir (File.path file)
    case res of
        Right deletedFile -> return $ Deleted deletedFile
        Left e | isDoesNotExistError e -> liftIO (warningM "Sync.applyDeletedToLocal" (show e)) >> return (Deleted file)
        Left e -> throw e


applyChangesRemote :: FilePath -> Map Path RemoteFile -> [Change] -> ApiAction [Change]
applyChangesRemote syncDir rState = fmap catMaybes . applyChanges rState
    where
        applyChanges :: Map Path RemoteFile -> [Change] -> ApiAction [Maybe Change]
        applyChanges _ [] = return []
        applyChanges remoteState (headChange:tailChanges) =
                case headChange of
                    Created (Dir createdPath) -> do
                        res <- tryWithLogging $ applyCreatedDirToRemote createdPath
                        let appliedHead = fmap fst res
                        let newFolder = fmap snd res
                        let newState = case newFolder of
                                            Just nf -> Map.insert createdPath nf remoteState
                                            Nothing -> remoteState
                        appliedTail <- applyChanges newState tailChanges
                        return $ appliedHead : appliedTail
                    Created (File createdPath _) -> do
                        appliedHead <- tryWithLogging $ applyCreatedFileToRemote syncDir remoteState createdPath
                        appliedTail <- applyChanges remoteState tailChanges
                        return $ appliedHead : appliedTail
                    Deleted file -> do
                        appliedHead <- tryWithLogging $ applyDeletedToRemote remoteState file
                        appliedTail <- applyChanges remoteState tailChanges
                        return $ appliedHead : appliedTail

applyCreatedFileToRemote :: FilePath -> Map Path RemoteFile -> Path -> ApiAction Change
applyCreatedFileToRemote syncDir remoteFiles (Path relativeFilePath) = do
    let parentDirPath = addTrailingPathSeparator . takeDirectory $ relativeFilePath
    let absoluteFilePath = syncDir </> relativeFilePath
    let parentDirMaybe = Map.lookup (Path parentDirPath) remoteFiles
    case parentDirMaybe of
        Just parentDir -> Created <$> upload parentDir absoluteFilePath
        Nothing -> error $ "no parent dir: " ++ parentDirPath

applyCreatedDirToRemote ::Path -> ApiAction (Change, RemoteFile)
applyCreatedDirToRemote dirPath = do
    --For now Digipost only support one level of folders
    let folderName = takeFileName . takeDirectory . filePath $ dirPath
    (manager, aToken, csrfToken, mbox) <- ask
    createFolderLink <- liftIO $ linkOrException "create_folder" $ DP.mailboxLinks mbox
    newFolder <- liftResourceT $ createFolder aToken manager createFolderLink csrfToken folderName
    let dir = Dir dirPath
    return (Created dir, RemoteDir dir newFolder)

applyDeletedToRemote :: Map Path RemoteFile -> File -> ApiAction Change
applyDeletedToRemote remoteFiles file = do
    let deletedPath = File.path file
    let remoteFileMaybe = Map.lookup deletedPath remoteFiles
    case remoteFileMaybe of
        Just remoteFile -> do
            deleteRemote remoteFile
            return $ Deleted (getFile remoteFile)
        Nothing -> error $ "remote file not found: " ++ show deletedPath

upload :: RemoteFile -> FilePath -> ApiAction File
upload (RemoteDir _ parentFolder) absoluteFilePath = do
    uploadLink <- liftIO $ linkOrException "upload_document" (DP.folderLinks parentFolder)
    (manager, aToken, csrf, _) <- ask
    doc <- liftResourceT $ uploadDocument aToken manager uploadLink csrf absoluteFilePath
    liftIO $ debugM "Sync.upload" ("Uploaded " ++ absoluteFilePath)
    return $ fileFromFolderDoc parentFolder doc
upload _ _ = error "parent dir cannot be a file"

deleteRemote :: RemoteFile -> ApiAction ()
deleteRemote (RemoteFile _ _ document) = do
    (manager, aToken, csrf, _) <- ask
    liftResourceT $ deleteDocument aToken manager csrf document
deleteRemote (RemoteDir _ folder) = do
    (manager, aToken, csrf, _) <- ask
    liftResourceT $ deleteFolder aToken manager csrf folder

deleteLocal :: FilePath -> Path -> IO File
deleteLocal syncDir p@(Path relativePath) = deleteFileOrDir
    where
        deleteFileOrDir = do
            let targetFile = syncDir </> relativePath
            isDirectory <- doesDirectoryExist targetFile
            modified <- getModificationTime targetFile
            if isDirectory then
                removeDirectoryRecursive targetFile >> return (Dir p)
            else
                removeFile targetFile >> return (File p modified)


download :: RemoteFile -> FilePath -> ApiAction File
download (RemoteFile file _ document) targetFile = do
    (manager, aToken, _, _) <- ask
    liftResourceT $ downloadDocument aToken manager targetFile document
    liftIO $ debugM "Sync.download" ("Downloaded " ++ targetFile)
    newFileModified <- liftIO $ getModificationTime targetFile
    return $ File (File.path file) newFileModified
download (RemoteDir dir _) targetFile = liftIO $ do
    dirExists <- doesDirectoryExist targetFile
    unless dirExists $ createDirectory targetFile
    return $ Dir (File.path dir)

checkLocalChange :: IO Bool
checkLocalChange = do
    (_, previousLocalFiles, _, localFiles) <- initLocalState
    let localChanges = computeChanges localFiles previousLocalFiles
    unless (null localChanges) (liftIO $ infoM "Sync.checkLocalChange" ("localChanges:\n" ++ formatList localChanges))
    return $ not (null localChanges)

checkRemoteChange :: IO Bool
checkRemoteChange = handleTokenRefresh checkRemoteChange' <$> loadAccessToken >>= withHttp


withHttp :: (Manager -> ResourceT IO a) -> IO a
withHttp action = runResourceT $ liftIO (newManager tlsManagerSettings) >>= action


checkRemoteChange' :: Manager -> AccessToken -> ResourceT IO Bool
checkRemoteChange' manager token = do
    (_, _, previousRemoteFiles, _) <- liftIO initLocalState
    (root, _, mbox) <- getAccount manager token
    remoteState <- runReaderT getRemoteState (manager, token, DP.csrfToken root, mbox)
    let remoteFiles = getFileSetFromMap remoteState
    let remoteChanges = computeChanges remoteFiles previousRemoteFiles
    unless (null remoteChanges) (liftIO $ infoM "Sync.checkRemoteChange" ("remoteChanges:\n" ++ formatList remoteChanges))
    return $ not (null remoteChanges)

sync :: IO ()
sync = handleTokenRefresh sync' <$> loadAccessToken >>= withHttp

sync' :: Manager -> AccessToken -> ResourceT IO ()
sync' manager token = do
    liftIO $ infoM "Sync.sync" "Start sync"
    (syncDir, previousLocalFiles, previousRemoteFiles, localFiles) <- liftIO initLocalState
    (root, _, mbox) <- getAccount manager token
    flip runReaderT (manager, token, DP.csrfToken root, mbox) $ do
        liftIO $ debugM "Sync.sync" "before getRemoteState"
        remoteState <- getRemoteState
        liftIO $ debugM "Sync.sync" "afterGetRemoteState"
        let remoteFiles = getFileSetFromMap remoteState
        let localChanges = computeChanges localFiles previousLocalFiles
        let remoteChanges = computeChanges remoteFiles previousRemoteFiles
        --server always win if conflict TODO: better handle conflicts
        let changesToApplyLocal = remoteChanges
        let changesToApplyRemote = computeChangesToApply localChanges remoteChanges
        liftIO $ infoM "Sync.sync" ("localChanges:\n" ++ formatList localChanges)
        liftIO $ infoM "Sync.sync" ("remoteChanges:\n" ++ formatList remoteChanges)
        appliedLocalChanges <- applyChangesLocal syncDir remoteState changesToApplyLocal
        appliedRemoteChanges <- applyChangesRemote syncDir remoteState changesToApplyRemote
        liftIO $ debugM "Sync.sync" "finished applying changes"
        let newLocalState = computeNewState previousLocalFiles localChanges appliedLocalChanges appliedRemoteChanges
        let newRemoteState = computeNewState previousRemoteFiles remoteChanges appliedRemoteChanges appliedLocalChanges
        liftIO $ debugM "Sync.sync" "computed new state"
        meta <- liftIO getMetaDir
        liftIO $ withDb meta $ \conn -> do
            initDatabase conn
            replaceLocalFiles newLocalState conn
            replaceRemoteFiles newRemoteState conn
        liftIO $ debugM "Sync.sync" "stored in db"
        return ()

initLogging :: IO ()
initLogging = do
    metaDir <- getMetaDir
    let logFile = combine metaDir "log"
    h <- fileHandler logFile DEBUG >>= \lh -> return $
            setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "" (addHandler h)
    updateGlobalLogger "" (setLevel DEBUG)

getUserSyncDir :: IO FilePath
getUserSyncDir = do
    homedir <- getHomeDirectory
    return $ homedir </> syncDirName

getOrCreateSyncDir :: IO FilePath
getOrCreateSyncDir = do
    syncDir <- getUserSyncDir
    createDirectoryIfMissing True syncDir
    return syncDir

getMetaDir :: IO FilePath
getMetaDir = do
    syncDir <- getOrCreateSyncDir
    let metaDir = syncDir </> ".sync"
    metaIsFile <- doesFileExist metaDir
    when metaIsFile $ removeFile metaDir --to upgrade from older versions
    createDirectoryIfMissing True metaDir
    return metaDir

getSyncFile :: IO FilePath
getSyncFile = (</> "db") <$> getMetaDir

debugLog :: String -> IO ()
debugLog = putStrLn
-- debugLog _ = return ()

formatList :: Show a => [a] -> String
formatList = unlines . map show