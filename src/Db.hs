module Db where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.FilePath
import Control.Exception
import qualified Data.Set as Set

import File

newtype Table = Table {tableName :: String}

localFilesTable :: Table
localFilesTable = Table "local_files"

remoteFilesTable :: Table
remoteFilesTable = Table "remote_files"

initDatabase :: Connection -> IO ()
initDatabase conn = do
    mapM_ createTable [localFilesTable, remoteFilesTable]
    commit conn
    where createTable t = do
             let sql = "create table if not exists " ++ tableName t ++
                       " (id INTEGER PRIMARY KEY, path varchar(255) not null, modified timestamp, dir boolean not null)"
             run conn sql []

withDb :: FilePath -> (Connection -> IO a) -> IO a
withDb metaDir action = do
    conn <- connectSqlite3 (metaDir </> "sync.db")
    finally (action conn) (disconnect conn)

insertFile :: Connection -> Table -> File -> IO ()
insertFile conn t f = do
    let params = case f of
                    File (Path p) modified -> [toSql p, toSql modified, toSql False]
                    Dir (Path p)           -> [toSql p, SqlNull, toSql True]
    _ <- run conn ("insert into " ++ tableName t ++ " (path, modified, dir) values (?, ?, ?)") params
    commit conn


getLocalFiles :: Connection -> IO (Set.Set File)
getLocalFiles = getAllFiles localFilesTable

getRemoteFiles :: Connection -> IO (Set.Set File)
getRemoteFiles = getAllFiles remoteFilesTable

getAllFiles :: Table -> Connection -> IO (Set.Set File)
getAllFiles t conn = do
    res <- quickQuery' conn ("select path, modified, dir from " ++ tableName t) []
    let files = map rowToFile res
    return $ Set.fromList files
    where
        rowToFile :: [SqlValue] -> File
        rowToFile [sqlPath, sqlModified, sqlIsDir] =
            if fromSql sqlIsDir then
                Dir (Path (fromSql sqlPath))
            else
                File (Path (fromSql sqlPath)) (fromSql sqlModified)
        rowToFile x = error $ "Unexpected result: " ++ show x

replaceLocalFiles :: Set.Set File -> Connection -> IO ()
replaceLocalFiles = replaceTable localFilesTable

replaceRemoteFiles :: Set.Set File -> Connection -> IO ()
replaceRemoteFiles = replaceTable remoteFilesTable

replaceTable :: Table -> Set.Set File -> Connection -> IO ()
replaceTable t files conn = do
    _ <- run conn ("delete from " ++ tableName t) []
    mapM_ (insertFile conn t) (Set.toList files)
