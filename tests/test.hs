import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative

import Sync

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]


unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Zipper cannot move up from root node" $
      case ftUp zipper of
        Nothing -> return ()
        Just _ -> assertFailure "Expected nothing"

  , testCase "Zipper can go down and up again" $
      case Just zipper >>= ftDown >>= ftUp of
          Nothing -> assertFailure "Expected zipper"
          Just _ -> return ()

  , testCase "Compute full path" $
      case fullPath <$> ftDown zipper of
          Nothing -> assertFailure "Expected full path"
          Just p -> assertEqual "" "Digipostarkiv/fileA" p
  ]

tree :: FileTree
tree = Dir "Digipostarkiv" [File "fileA" Nothing] Nothing

zipper :: FTZipper
zipper = ftZipper tree
