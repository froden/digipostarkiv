module HsCocoa where

import Foreign.C

import Data.ByteString.Char8

import qualified Oauth as O
import Main
import Api

foreign export ccall authUrl :: CString -> IO CString
foreign export ccall accessToken :: CString -> CString -> IO CInt
foreign export ccall syncNow :: IO CInt
foreign export ccall logout :: IO ()


authUrl :: CString -> IO CString
authUrl s = do
	state <- peekCString s
	newCString $ unpack $ O.loginUrl (O.State state)

--TODO: Hente accessToken og lagre
accessToken :: CString -> CString -> IO CInt
accessToken s c = do
	state <- peekCString s
	code <- peekCString c
	result <- O.accessToken (O.State state) (O.AuthCode code)
	case result of
		Right token -> O.storeAccessToken token >> return 0
		Left NotAuthenticated -> return 1
		Left _ -> return 99

syncNow :: IO CInt
syncNow = do
	result <- guiSync
	case result of
		Right _ -> return 0
		Left NotAuthenticated -> return 1
		Left (HttpFailed e) -> print e >> return 99

logout :: IO ()
logout = O.removeAccessToken


