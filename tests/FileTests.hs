module FileTests where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List

import File2

fileTests :: TestTree
fileTests = testGroup "File tests"
  [ testCase "changes should be sorted" $
        let
            localFiles = Set.fromList [Dir "archive/", File "archive/file1"]
            localFilesPrevious = Set.fromList [Dir "money/", File "money/file2"] :: Set File
            remoteFiles = Set.fromList [] :: Set File
            remoteFilesPrevious = Set.fromList [Dir "money/"] :: Set File
            expectedLocalChanges = [Created (Dir "archive/"), Created (File "archive/file1"), Deleted (File "money/file2"), Deleted (Dir "money/")]
            expectedRemoteChanges = [Deleted (Dir "money/")]
            actualLocalChanges = computeChanges localFiles localFilesPrevious
            actualRemoteChanges = computeChanges remoteFiles remoteFilesPrevious
            changesToApplyToRemote = [Created (Dir "archive/"), Created (File "archive/file1"), Deleted (File "money/file2")]
            changesToApplyToLocal = []
        in do
            assertEqual "" expectedLocalChanges actualLocalChanges
            assertEqual "" expectedRemoteChanges actualRemoteChanges
            assertEqual "" changesToApplyToRemote $ actualLocalChanges \\ actualRemoteChanges
            assertEqual "" changesToApplyToLocal $ actualRemoteChanges \\ actualLocalChanges

  ]
