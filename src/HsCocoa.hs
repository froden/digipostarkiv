{-# LANGUAGE ScopedTypeVariables #-}

module HsCocoa where

import Foreign.C

import Data.ByteString.Char8
import Data.Either
import Control.Exception

import qualified Oauth as O
import Sync
import Http
import Error

foreign export ccall hsAuthUrl :: CString -> IO CString
foreign export ccall hsAccessToken :: CString -> CString -> IO CInt
foreign export ccall hsSync :: IO CInt
foreign export ccall hsLogout :: IO ()
foreign export ccall hsLoggedIn :: IO Bool
foreign export ccall hsLocalChanges :: IO Bool
foreign export ccall hsRemoteChanges :: IO Bool


hsAuthUrl :: CString -> IO CString
hsAuthUrl s = do
	state <- peekCString s
	newCString $ unpack $ O.loginUrl (O.State state)

hsAccessToken :: CString -> CString -> IO CInt
hsAccessToken s c = do
	state <- peekCString s
	code <- peekCString c
	result <- tryAny $ O.accessToken (O.State state) (O.AuthCode code)
	case result of
		Right token -> O.storeAccessToken token >> return 0
		Left NotAuthenticated -> return 1
		Left e -> printError e >> return 99

hsSync :: IO CInt
hsSync = do
	res <- tryAny sync
	case res of
		Right _ -> return 0
		Left NotAuthenticated -> return 1
		Left e -> printError e >> return 99

hsLogout :: IO ()
hsLogout = O.removeAccessToken

hsLoggedIn :: IO Bool
hsLoggedIn = fmap isRight (try O.loadAccessToken :: IO (Either SyncError Http.AccessToken))

hsLocalChanges :: IO Bool
hsLocalChanges = checkLocalChange

hsRemoteChanges :: IO Bool
hsRemoteChanges = checkRemoteChange

tryAny :: IO a -> IO (Either SyncError a)
tryAny action = do
	result <- try action
	case result of
		Right x -> return $ Right x
		Left e -> case fromException e of
			Just (se :: SyncError) -> return $ Left se
			Nothing -> return $ Left (Unhandled e)

printError :: SyncError -> IO ()
printError e = print ("SyncError: " ++ show e)
