module FileTests where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX

import File

fileTests :: TestTree
fileTests = testGroup "File tests"
  [ testCase "changes should be sorted" $
        let
            timestamp = posixSecondsToUTCTime 10
            localFiles = Set.fromList [Dir (Path "archive/"), File (Path "archive/file1") timestamp]
            localFilesPrevious = Set.fromList [Dir (Path "money/"), File (Path "money/file2") timestamp] :: Set File
            remoteFiles = Set.fromList [] :: Set File
            remoteFilesPrevious = Set.fromList [Dir (Path "money/")] :: Set File
            expectedLocalChanges = [Deleted (File (Path "money/file2") timestamp), Deleted (Dir (Path "money/")), Created (Dir (Path "archive/")), Created (File (Path "archive/file1") timestamp)]
            expectedRemoteChanges = [Deleted (Dir (Path "money/"))]
            actualLocalChanges = computeChanges localFiles localFilesPrevious
            actualRemoteChanges = computeChanges remoteFiles remoteFilesPrevious
            changesToApplyToRemote = [Deleted (File (Path "money/file2") timestamp), Created (Dir (Path "archive/")), Created (File (Path "archive/file1") timestamp)]
            changesToApplyToLocal = []
        in do
            assertEqual "" expectedLocalChanges actualLocalChanges
            assertEqual "" expectedRemoteChanges actualRemoteChanges
            assertEqual "" changesToApplyToRemote $ computeChangesToApply actualLocalChanges actualRemoteChanges
            assertEqual "" changesToApplyToLocal $ computeChangesToApply actualRemoteChanges actualLocalChanges

  , testCase "comput new state" $
        let
            timestamp = posixSecondsToUTCTime 10
            localFiles = Set.fromList [Dir (Path "archive/"), File (Path "archive/file1") timestamp, File (Path "money/file2") timestamp]
            changes = [Created (Dir (Path "archive/")), Created (File (Path "archive/file1") timestamp), Deleted (File (Path "money/file2") timestamp), Deleted (Dir (Path "money/"))]
            expectedNewState = Set.fromList [Dir (Path "archive/"), File (Path "archive/file1") timestamp]
            computedNewState = computeNewStateFromChanges localFiles changes
        in
            assertEqual "" expectedNewState computedNewState
  ]
