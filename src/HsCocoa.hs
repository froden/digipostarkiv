module HsCocoa where

import Foreign.C

import Data.ByteString.Char8

import qualified Oauth as O
import Main
import Api

foreign export ccall hs_authUrl :: CString -> IO CString
foreign export ccall hs_accessToken :: CString -> CString -> IO CInt
foreign export ccall hs_sync :: IO CInt
foreign export ccall hs_logout :: IO ()


hs_authUrl :: CString -> IO CString
hs_authUrl s = do
	state <- peekCString s
	newCString $ unpack $ O.loginUrl (O.State state)

hs_accessToken :: CString -> CString -> IO CInt
hs_accessToken s c = do
	state <- peekCString s
	code <- peekCString c
	result <- O.accessToken (O.State state) (O.AuthCode code)
	case result of
		Right token -> O.storeAccessToken token >> return 0
		Left NotAuthenticated -> return 1
		Left _ -> return 99

hs_sync :: IO CInt
hs_sync = do
	result <- guiSync
	case result of
		Right _ -> return 0
		Left NotAuthenticated -> return 1
		Left (HttpFailed e) -> print e >> return 99

hs_logout :: IO ()
hs_logout = O.removeAccessToken

