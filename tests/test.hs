import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative

import Sync
import File
import ApiTypes
import FileTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [fileTests]
