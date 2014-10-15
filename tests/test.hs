import Test.Tasty
import Test.Tasty.HUnit

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

  -- the following test does not hold
  , testCase "Zipper can go down and up again" $
      case Just zipper >>= ftDown >>= ftUp of
          Nothing -> assertFailure "Expected zipper"
          Just _ -> return ()
  ]

tree :: FileTree
tree = Dir "Digipostarkiv" [File "fileA" Nothing] Nothing

zipper :: FTZipper
zipper = ftZipper tree
