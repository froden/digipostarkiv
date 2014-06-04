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
foreign export ccall hsSync :: Int -> IO CInt
foreign export ccall hsLogout :: IO ()
foreign export ccall hsLoggedIn :: IO Bool


hsAuthUrl :: CString -> IO CString
hsAuthUrl s = do
	state <- peekCString s
	newCString $ unpack $ O.loginUrl (O.State state)

hsAccessToken :: CString -> CString -> IO CInt
hsAccessToken s c = do
	state <- peekCString s
	code <- peekCString c
	result <- try $ O.accessToken (O.State state) (O.AuthCode code)
	case result of
		Right token -> O.storeAccessToken token >> return 0
		Left NotAuthenticated -> return 1
		Left _ -> return 99

hsSync :: Int -> IO CInt
hsSync runNumber = do
	result <- try $ guiSync runNumber
	case result of
		Right _ -> return 0
		Left NotAuthenticated -> return 1
		Left (HttpFailed e) -> print e >> return 99

hsLogout :: IO ()
hsLogout = O.removeAccessToken

hsLoggedIn :: IO Bool
hsLoggedIn = fmap isRight (try O.loadAccessToken :: IO (Either SyncError Http.AccessToken))


