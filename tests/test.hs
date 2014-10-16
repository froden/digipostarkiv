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

  , testCase "Diff of equal trees is Nothing" $
      case treeDiff tree tree of
          Nothing -> return ()
          Just f -> assertFailure $ "Diff was: " ++ (show f)

  , testCase "Diff with super set tree is Nothing" $
      case treeDiff tree (Dir "Digipostarkiv" [File "fileB" Nothing, File "fileA" Nothing] Nothing) of
          Nothing -> return ()
          Just f -> assertFailure $ "Diff was: " ++ (show f)

  , testCase "Diff equal dir missing file" $
      case treeDiff tree (Dir "Digipostarkiv" [] Nothing) of
          Nothing -> assertFailure "Expected diff"
          Just f -> assertEqual "" tree f

  , testCase "Diff equal dir different file" $
      case treeDiff tree (Dir "Digipostarkiv" [File "fileB" Nothing] Nothing) of
          Nothing -> assertFailure "Expected diff"
          Just f -> assertEqual "" tree f

  , testCase "Diff content is empty dir" $
      let tree2 = Dir "Digipostarkiv" [File "fileA" Nothing, Dir "dirA" [] Nothing] Nothing
      in case treeDiff tree2 tree of
          Nothing -> assertFailure "Expected diff"
          Just f -> assertEqual "" (Dir "Digipostarkiv" [Dir "dirA" [] Nothing] Nothing) f

  , testCase "Diff content is new file and equal dir with missing file" $
      let
        tree1 = Dir "Digipostarkiv" [File "fileA" Nothing, Dir "dirA" [] Nothing] Nothing
        tree2 = Dir "Digipostarkiv" [Dir "dirA" [File "fileB" Nothing] Nothing] Nothing
      in case treeDiff tree1 tree2 of
          Nothing -> assertFailure "Expected diff"
          Just f -> assertEqual "" (Dir "Digipostarkiv" [File "fileA" Nothing] Nothing) f

  , testCase "Diff complex trees" $
      let
        tree1 = Dir "Digipostarkiv" [File "fileA" Nothing, Dir "dirA" [File "fileB" Nothing, File "fileC" Nothing] Nothing, Dir "dirB" [File "fileD" Nothing] Nothing] Nothing
        tree2 = Dir "Digipostarkiv" [Dir "dirA" [File "fileB" Nothing, File "fileD" Nothing] Nothing, Dir "dirB" [] Nothing] Nothing
      in case treeDiff tree1 tree2 of
          Nothing -> assertFailure "Expected diff"
          Just f -> assertEqual "" (Dir "Digipostarkiv" [File "fileA" Nothing, Dir "dirA" [File "fileC" Nothing] Nothing, Dir "dirB" [File "fileD" Nothing] Nothing] Nothing) f

  , testCase "compute newOnServer" $
      let
        local = Dir "Digipostarkiv" [Dir "dirA" [File "fileA" Nothing] Nothing] Nothing
        remote = Dir "Digipostarkiv" [Dir "dirA" [File "fileA" Nothing, File "fileB" Nothing] Nothing, Dir "dirB" [] Nothing] Nothing
      in case newOnServer local emptyTree remote of
          Nothing -> assertFailure "Expected diff"
          Just f -> assertEqual "" (Dir "Digipostarkiv" [Dir "dirA" [File "fileB" Nothing] Nothing, Dir "dirB" [] Nothing] Nothing) f

  , testCase "compute deletedOnServer" $
      let
        previous = Dir "Digipostarkiv" [Dir "dirA" [File "fileA" Nothing, File "fileB" Nothing] Nothing, Dir "dirB" [] Nothing] Nothing
        remote = Dir "Digipostarkiv" [Dir "dirA" [File "fileA" Nothing] Nothing] Nothing
      in case deletedOnServer previous remote of
          Nothing -> assertFailure "Expected diff"
          Just f -> assertEqual "" (Dir "Digipostarkiv" [Dir "dirA" [File "fileB" Nothing] Nothing, Dir "dirB" [] Nothing] Nothing) f

  , testCase "compute newLocal" $
      let
        local = Dir "Digipostarkiv" [Dir "dirA" [File "fileA" Nothing, File "fileB" Nothing] Nothing, Dir "dirB" [] Nothing] Nothing
        previous = Dir "Digipostarkiv" [Dir "dirA" [File "fileA" Nothing] Nothing, Dir "dirB" [] Nothing] Nothing
        remote = Dir "Digipostarkiv" [Dir "dirA" [File "fileA" Nothing] Nothing] Nothing
      in case newLocal local previous remote of
          Nothing -> assertFailure "Expected diff"
          Just f -> assertEqual "" (Dir "Digipostarkiv" [Dir "dirA" [File "fileB" Nothing] Nothing] Nothing) f
  ]

emptyTree :: FileTree
emptyTree = Dir "Digipostarkiv" [] Nothing

tree :: FileTree
tree = Dir "Digipostarkiv" [File "fileA" Nothing] Nothing

zipper :: FTZipper
zipper = ftZipper tree