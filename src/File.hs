module File where

import System.FilePath ((</>), takeDirectory)

import ApiTypes
import Data.Maybe


type Name = String
data FileTree =
    File Name (Maybe Document) |
    Dir Name [FileTree] (Maybe Folder) deriving (Show, Read)

data FTCtx = FTCtx Name [FileTree] [FileTree] (Maybe Folder) deriving (Show)

type FTZipper = (FileTree, [FTCtx])

instance Eq FileTree where
  (Dir name1 contents1 _) == (Dir name2 contents2 _) = name1 == name2 && contents1 == contents2
  (File name1 _) == (File name2 _) = name1 == name2
  _ == _ = False

ftZipper :: FileTree -> FTZipper
ftZipper ft = (ft, [])

ftUp :: FTZipper -> Maybe FTZipper
ftUp (item, FTCtx name ls rs f:bs) = Just (Dir name (ls ++ [item] ++ rs) f, bs)
ftUp (_, []) = Nothing

ftRight :: FTZipper -> Maybe FTZipper
ftRight (item, FTCtx name ls (r:rs) f:bs) = Just (r, FTCtx name (ls ++ [item]) rs f:bs)
ftRight (_, FTCtx _ _ [] _:_) = Nothing
ftRight (_, []) = Nothing

ftDown :: FTZipper -> Maybe FTZipper
ftDown (Dir name (c:cs) f, bs) = Just (c, FTCtx name [] cs f:bs)
ftDown (Dir _ [] _, _) = Nothing
ftDown (File _ _, _) = error "cannot go down into file"

ftName :: FileTree -> String
ftName (File name _) = name
ftName (Dir name _ _) = name

ftTraverse :: Monad m => (FTZipper -> m FTZipper) -> FTZipper -> m ()
ftTraverse action z@(File{}, _) = do
  nz <- action z
  maybe (return ()) (ftTraverse action) (ftRight nz)
ftTraverse action z@(Dir{}, _) = do
  nz <- action z
  maybe (return ()) (ftTraverse action) (ftDown nz)
  maybe (return ()) (ftTraverse action) (ftRight nz)

treeDiff :: FileTree -> FileTree -> Maybe FileTree
treeDiff ft1@(Dir name1 contents1 folder1) ft2@(Dir name2 contents2 folder2)
    | ft1 == ft2 = Nothing
    | name1 == name2 =
        let contentDiff = contents1 `diff` contents2
        in if null contentDiff
           then Nothing
           else Just $ Dir name1 contentDiff (folder1 `orElse` folder2)
treeDiff ft1 ft2
    | ft1 == ft2 = Nothing
    | otherwise = Just ft1

diff :: [FileTree] -> [FileTree] -> [FileTree]
diff = foldl (flip deleteFrom)
  where
    deleteFrom _ [] = []
    deleteFrom x (y:ys) = case treeDiff y x of
      Nothing -> ys
      Just ft -> ft : deleteFrom x ys

combineWith :: FileTree -> FileTree -> FileTree
combineWith ft1 ft2 = fromMaybe ft1 (comb ft1 ft2)

comb :: FileTree -> FileTree -> Maybe FileTree
comb (Dir name1 contents1 folder1) (Dir name2 contents2 folder2)
    | name1 == name2 =
        let newContents = contents1 `combineWith'` contents2
        in Just $ Dir name1 newContents (folder1 `orElse` folder2)
comb (File name1 doc1) (File name2 doc2)
    | name1 == name2 = Just $ File name1 (doc1 `orElse` doc2)
comb _ _ = Nothing

combineWith' :: [FileTree] -> [FileTree] -> [FileTree]
combineWith' fts1 fts2 = fmap (combineJe fts2) fts1
    where
        combineJe [] x = x
        combineJe (y:ys) x = fromMaybe (combineJe ys x) (comb x y)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse mx my = case mx of
  Nothing -> my
  Just x -> Just x

fullPath :: FilePath -> FTZipper -> FilePath
fullPath syncDir z@(item, _) = case ftUp z of
  Nothing -> takeDirectory syncDir </> name
  Just parentZipper -> fullPath syncDir parentZipper </> name
  where name = ftName item
