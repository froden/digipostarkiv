{-# LANGUAGE OverloadedStrings #-}

module Sync where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Control.Monad.Error
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import System.FilePath.Posix
import Data.List
import Data.Maybe
import System.Directory
import Control.Exception
import Text.Read (readMaybe)

import Api
import qualified ApiTypes as DP
import qualified File as F
import Oauth
import Http (AccessToken)
import Error

type CSRFToken = String
type ApiAction a = ReaderT (Manager, AccessToken, CSRFToken, DP.Mailbox) (ResourceT IO) a

data File' = File' FilePath (Maybe DP.Document)
data Dir' = Dir' FilePath [File'] (Maybe DP.Folder)

type Name = String
data FileTree =
    File Name (Maybe DP.Document) |
    Dir Name [FileTree] (Maybe DP.Folder) deriving (Show, Read)

data FTCtx = FTCtx Name [FileTree] [FileTree] (Maybe DP.Folder) deriving (Show)

type FTZipper = (FileTree, [FTCtx])

ftZipper :: FileTree -> FTZipper
ftZipper ft = (ft, [])

ftUp :: FTZipper -> Maybe FTZipper
ftUp (item, FTCtx name ls rs f:bs) = Just (Dir name (ls ++ [item] ++ rs) f, bs)
ftUp (_, []) = Nothing

ftRight :: FTZipper -> Maybe FTZipper
ftRight (item, FTCtx name ls (r:rs) f:bs) = Just (r, FTCtx name (ls ++ [item]) rs f:bs)
ftRight (_, FTCtx _ _ [] _:_) = Nothing
ftRight (_, []) = error "cannot go rigth from root node"

ftDown :: FTZipper -> Maybe FTZipper
ftDown (Dir name (c:cs) f, bs) = Just (c, FTCtx name [] cs f:bs)
ftDown (Dir _ [] _, _) = Nothing
ftDown (File _ _, _) = error "cannot go down into file"

readSyncState :: FilePath -> IO FileTree
readSyncState syncFile = do
    r <- try (readFile syncFile) :: IO (Either IOException String)
    case r of
        Right content -> return $ fromMaybe emptyDir (readMaybe content)
        Left _ -> return emptyDir
    where emptyDir = Dir "Digipostarkiv" [] Nothing

writeSyncState :: FilePath -> FileTree -> IO ()
writeSyncState syncFile state = writeFile syncFile (show state)

getLocalState :: FilePath -> IO FileTree
getLocalState dirPath = do
    names <- getDirectoryContents dirPath
    let properNames = filter (isPrefixOf ".") $ filter (`notElem` [".", ".."]) names
    content <- forM properNames $ \name -> do
        let subPath = dirPath </> name
        isDirectory <- doesDirectoryExist subPath
        if isDirectory
            then getLocalState subPath
            else return (File name Nothing)
    return $ Dir (takeFileName dirPath) content Nothing

getRemoteState :: ApiAction FileTree
getRemoteState = do
    (_, _, _, mbox) <- ask
    let folders = (DP.folder . DP.folders) mbox
    contents <- mapM downloadFolder folders
    return $ Dir "Digipostarkiv" contents Nothing
  where
    downloadFolder :: DP.Folder -> ApiAction FileTree
    downloadFolder folder = do
      (manager, aToken, _, _) <- ask
      folderLink <- liftIO $ linkOrException "self" $ DP.folderLinks folder
      fullFolder <- liftResourceT $ getFolder aToken manager folderLink
      let documents = filter DP.uploaded (DP.documentInFolder fullFolder)
      let files = map docToFile documents
      return $ Dir (DP.folderName folder) files (Just folder)
      where
        docToFile doc = File (DP.filename doc) (Just doc)

instance Eq FileTree where
  (Dir name1 contents1 _) == (Dir name2 contents2 _) = name1 == name2 && contents1 == contents2
  (File name1 _) == (File name2 _) = name1 == name2
  _ == _ = False

treeDiff :: FileTree -> FileTree -> Maybe FileTree
treeDiff ft1@(Dir name1 contents1 folder1) ft2@(Dir name2 contents2 _)
    | ft1 == ft2 = Nothing
    | name1 == name2 =
        let contentDiff = contents1 `diff` contents2
        in if null contentDiff
           then Nothing
           else Just $ Dir name1 contentDiff folder1
treeDiff ft1 ft2
    | ft1 == ft2 = Nothing
    | otherwise = Just ft1

diff :: [FileTree] -> [FileTree] -> [FileTree]
diff = foldl (flip deleteFrom)
  where
    deleteFrom _ [] = []
    deleteFrom x (y:ys) = case treeDiff y x of
      Nothing -> ys
      Just ft -> ft : deleteFrom x ys


orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
    Just _  -> x
    Nothing -> y
--newOnServer
--addedLocal :: FileTree -> FileTree -> FileTree -> FileTree

--deletedOnServer :: FileTree -> FileTree -> FileTree -> FileTree

--deletedLocal :: FileTree -> FileTree -> FileTree -> FileTree

ftName :: FileTree -> String
ftName (File name _) = name
ftName (Dir name _ _) = name

fullPath :: FTZipper -> FilePath
fullPath z@(item, _) = case ftUp z of
  Nothing -> name
  Just parentZipper -> fullPath parentZipper `combine` name
  where name = ftName item

download :: FTZipper -> ApiAction ()
download z@(File _ (Just remoteDoc), _) = Sync.downloadDocument (fullPath z) remoteDoc
download (File _ Nothing, _) = error "Either file at root node of no remote document"
download z@(Dir{}, _) = do
  liftIO $ createDirectoryIfMissing True (fullPath z)
  maybe (return ()) download (ftDown z)
  maybe (return ()) download (ftRight z)

upload :: FTZipper -> ApiAction ()
upload (File name _, FTCtx _ _ _ (Just parentFolder):_) = uploadDocument parentFolder name
upload (File _ _, _) = error "Either file at root node or no parent remote folder"
upload z@(Dir name _ _, _) = do
  createRemoteFolder name
  maybe (return ()) upload (ftDown z)
  maybe (return ()) upload (ftRight z)

downloadDocument :: FilePath -> DP.Document -> ApiAction ()
downloadDocument localPath remoteDoc = do
  (manager, aToken, _, _) <- ask
  liftResourceT $ Api.downloadDocument aToken manager localPath remoteDoc

createRemoteFolder :: Name -> ApiAction ()
createRemoteFolder folderName = do
  (manager, aToken, csrfToken, mbox) <- ask
  createFolderLink <- liftIO $ linkOrException "create_folder" $ DP.mailboxLinks mbox
  liftResourceT $ createFolder aToken manager createFolderLink csrfToken folderName

uploadDocument :: DP.Folder -> FilePath -> ApiAction ()
uploadDocument folder localPath = do
    (manager, aToken, csrfToken, _) <- ask
    uploadLink <- liftIO $ linkOrException "upload_document" $ DP.folderLinks folder
    liftResourceT $ uploadFileMultipart aToken manager uploadLink csrfToken localPath

--delete :: FileTree -> IO ()
--delete (File localPath _) = deleteFile localPath
--delete (Dir localPath _ _) = deleteDir localPath


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
    isLocalFolderChange <- checkLocalChange' F.existingFolders' syncDir
    localFolders <- F.existingFolders syncDir
    fileChanges <- mapM (checkLocalChange' F.existingFiles' . combine syncDir) localFolders
    return $ or $ isLocalFolderChange : fileChanges


checkLocalChange' :: (FilePath -> IO [F.Filename]) -> FilePath -> IO Bool
checkLocalChange' listFunc syncDir = do
    let syncFile = F.syncFile syncDir
    lastState <- F.readSyncFile' syncFile
    files <- listFunc syncDir
    return $ lastState /= files

checkRemoteChange :: IO Bool
checkRemoteChange = loadAccessToken >>= handleTokenRefresh checkRemoteChange'

--TODO: Fixme folder support
checkRemoteChange' :: AccessToken -> IO Bool
checkRemoteChange' token = runResourceT $ do
    syncDir <- liftIO getOrCreateSyncDir
    manager <- liftIO $ newManager conduitManagerSettings
    (root, _) <- getAccount manager token
    let mbox = head $ DP.mailbox root
    let folders = (DP.folder . DP.folders) mbox
    isRemoteFolderChange <- checkRemoteChange'' (return folders) syncDir
    remoteFileChanges <- mapM (\f -> (checkRemoteChange'' (docsInFolder token manager f) . combine syncDir . F.filenameStr) f) folders
    return $ or $ isRemoteFolderChange : remoteFileChanges
    where
        docsInFolder token' manager folder = do
            folderLink <- liftIO $ linkOrException "self" $ DP.folderLinks folder
            fullFolder <- getFolder token' manager folderLink
            return $ filter DP.uploaded $ DP.document $ fromMaybe (DP.Documents []) (DP.documents fullFolder)

checkRemoteChange'' :: (F.File a) => ResourceT IO [a] -> FilePath -> ResourceT IO Bool
checkRemoteChange'' listFunc syncDir = do
    remoteFiles <- listFunc
    let syncFile = F.syncFile syncDir
    lastState <- liftIO $ F.readSyncFile' syncFile
    return $ not $ null (remoteFiles `F.diff` lastState) && null (lastState `F.diff` remoteFiles)

sync :: IO ()
sync = loadAccessToken >>= handleTokenRefresh sync'

--problem: Hvis man kopierer en katalog får man med .sync-filen og denne vil da ikke bli lastet opp korekt.
sync' :: AccessToken -> IO ()
sync' token = runResourceT $ do
    syncDir <- liftIO getOrCreateSyncDir
    manager <- liftIO $ newManager conduitManagerSettings
    (root, _) <- getAccount manager token
    let mbox = head $ DP.mailbox root
    let csrf = DP.csrfToken root
    syncFolders manager csrf token mbox syncDir
    mailboxLink <- liftIO $ linkOrException "self" $ DP.mailboxLinks mbox
    liftIO $ debugLog $ "getting mailbox: " ++ show mailboxLink
    --bug i digipost: får 404 når man henter mailbox med self-link
    --mbox' <- getMailbox token manager mailboxLink
    (root', _) <- getAccount manager token
    let mbox' = head $ DP.mailbox root'
    liftIO $ debugLog "got mailbox"
    let folders = (DP.folder . DP.folders) mbox'
    void $ mapM (syncFilesInFolder manager csrf token syncDir) folders

syncFolders :: Manager -> CSRFToken -> AccessToken -> DP.Mailbox -> FilePath -> ResourceT IO ()
syncFolders manager csrf token mbox syncDir = do
    let folders = DP.folder $ DP.folders mbox
    let syncFile = F.syncFile syncDir
    localFolders <- liftIO $ F.existingFolders' syncDir
    lastState <- liftIO $ F.readSyncFile' syncFile
    let (newRemoteFolders, newLocalFolders, deletedRemoteFolders) = F.syncDiff lastState localFolders folders
    liftIO $ logDiff newRemoteFolders newLocalFolders deletedRemoteFolders
    liftIO $ F.createFolders syncDir $ map DP.folderName newRemoteFolders
    createFolderLink <- liftIO $ linkOrException "create_folder" $ DP.mailboxLinks mbox
    void $ mapM (createFolder token manager createFolderLink csrf . F.name) newLocalFolders
    liftIO $ F.deleteAllFolders syncDir $ map F.name deletedRemoteFolders
    newState <- liftIO $ F.existingFolders syncDir
    liftIO $ F.writeSyncFile syncFile newState


syncFilesInFolder :: Manager -> CSRFToken -> AccessToken -> FilePath -> DP.Folder -> ResourceT IO ()
syncFilesInFolder manager csrf token parent folder = do
    liftIO $ debugLog $ "Syncing: " ++ DP.folderName folder
    let syncDir = combine parent $ DP.folderName folder
    folderLink <- liftIO $ linkOrException "self" $ DP.folderLinks folder
    fullFolder <- getFolder token manager folderLink
    let documents = filter DP.uploaded $ DP.document $ fromMaybe (DP.Documents []) (DP.documents fullFolder)
    let syncFile = F.syncFile syncDir
    files <- liftIO $ F.existingFiles' syncDir
    lastState <- liftIO $ F.readSyncFile' syncFile
    let (docsToDownload, newFiles, deletedFiles) = F.syncDiff lastState files documents
    liftIO $ logDiff docsToDownload newFiles deletedFiles
    downloadAll token manager syncDir docsToDownload
    uploadLink <- liftIO $ linkOrException "upload_document" $ DP.folderLinks fullFolder
    uploadAll token manager uploadLink csrf $ map (combine syncDir . F.name) newFiles
    liftIO $ F.deleteAll syncDir $ map F.name deletedFiles
    newState <- liftIO $ F.existingFiles syncDir
    liftIO $ F.writeSyncFile syncFile newState

getUserSyncDir :: IO FilePath
getUserSyncDir = do
    homedir <- getHomeDirectory
    return $ combine homedir "Digipostarkiv"

getOrCreateSyncDir :: IO FilePath
getOrCreateSyncDir = do
    syncDir <- getUserSyncDir
    F.createSyncDir syncDir
    return syncDir

logDiff :: (F.File a, F.File b) => [a] -> [b] -> [b] -> IO ()
logDiff toDownload toUpload deleted = void $ mapM debugLog [
                            "download: [" ++ intercalate ", " (map F.filenameStr toDownload) ++ "]",
                            "upload: [" ++ intercalate ", " (map F.filenameStr toUpload) ++ "]",
                            "deleted: [" ++ intercalate ", " (map F.filenameStr deleted) ++ "]" ]

debugLog :: String -> IO ()
debugLog = putStrLn
--debugLog _ = return ()
