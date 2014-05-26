module HsCocoa where

import Foreign.C

import Data.ByteString.Char8
import Data.Maybe

import qualified Oauth as O
import Main
import Api

foreign export ccall hs_authUrl :: CString -> IO CString
foreign export ccall hs_accessToken :: CString -> CString -> IO CInt
foreign export ccall hs_sync :: Int -> IO CInt
foreign export ccall hs_logout :: IO ()
foreign export ccall hs_loggedIn :: IO Bool


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

hs_sync :: Int -> IO CInt
hs_sync runNumber = do
	result <- guiSync runNumber
	case result of
		Right _ -> return 0
		Left NotAuthenticated -> return 1
		Left (HttpFailed e) -> print e >> return 99

hs_logout :: IO ()
hs_logout = O.removeAccessToken

hs_loggedIn :: IO Bool
hs_loggedIn = fmap isJust O.loadAccessToken


