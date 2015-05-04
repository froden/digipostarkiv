module Db where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.FilePath
import Control.Exception
import qualified Data.Set as Set

import File

initDatabase :: Connection -> IO ()
initDatabase conn = do
    let createLocalFiles = "create table if not exists local_files (path varchar(255) not null primary key, modified timestamp not null)"
    _ <- run conn createLocalFiles []
    commit conn

withDb :: FilePath -> (Connection -> IO a) -> IO a
withDb metaDir action = do
    conn <- connectSqlite3 (metaDir </> "sync.db")
    finally (action conn) (disconnect conn)

insertFile :: File -> Connection -> IO ()
insertFile f conn = do
    let p = filePath (path f)
    let m = modifiedTime f
    _ <- run conn "insert into local_files (path, modified) values (?, ?)" [toSql p, toSql m]
    commit conn

getLocalFiles :: Connection -> IO (Set.Set File)
getLocalFiles conn = do
    res <- quickQuery' conn "select path, modified from local_files" []
    let files = map rowToFile res
    return $ Set.fromList files
    where
        rowToFile :: [SqlValue] -> File
        rowToFile [sqlPath, sqlModified] = File (Path (fromSql sqlPath)) (fromSql sqlModified)
        rowToFile x = error $ "Unexpected result: " ++ show x

updateLocalFiles :: Set.Set File -> Connection -> IO ()
updateLocalFiles files conn = mapM_ updateFile (Set.toList files)
    where
        updateFile file = do
            let p = filePath (path file)
            let m = modifiedTime file
            _ <- run conn "replace into local_files (path, modified) values (?, ?)" [toSql p, toSql m]
            commit conn
