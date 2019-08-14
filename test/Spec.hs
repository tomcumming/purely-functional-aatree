import Test.QuickCheck
import Data.Word (Word8)
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import Data.List (sort, nub)

import Lib

foldableToListIsSorted :: [Word8] -> Property
foldableToListIsSorted is = reverse (sort (nub is)) === Foldable.toList tree
  where
    tree = foldr (\x t -> insert t x) Leaf is

noSkippedLevels :: [Word8] -> Bool
noSkippedLevels is = case tree of
  Leaf -> True
  Branch l _ _ _ -> go l tree
  where
    tree = foldr (\x t -> insert t x) Leaf is
    go :: Level -> Tree a -> Bool
    go l t = case t of
      Leaf -> True
      Branch l2 _ _ _ | l2 > l || l < pred l2 -> False
      Branch l2 less _ more -> go l2 less && go l2 more

noLevelRightGrandchildWhenAdding :: [Word8] -> Bool
noLevelRightGrandchildWhenAdding is = check tree
  where
    tree = foldr (\x t -> insert t x) Leaf is
    check :: Tree a -> Bool
    check t = case t of
      Branch l1 _ _ (Branch l2 _ _ (Branch l3 _ _ _)) | l1 == l3 -> False
      Branch _ less _ more -> check less && check more
      Leaf -> True

noLeftHorizontalsWhenAdding :: [Word8] -> Bool
noLeftHorizontalsWhenAdding is = check tree
  where
    tree = foldr (\x t -> insert t x) Leaf is
    check tree = case tree of
      Leaf -> True
      Branch l1 (Branch l2 _ _ _) _ _ | l1 == l2 -> False
      Branch _ less _ more -> check less && check more

treeAfterAddContainsAll :: [Word8] -> Property
treeAfterAddContainsAll is = allInTree === allAdded
  where
    tree = foldr (\x t -> insert t x) Leaf is
    allInTree = Set.fromList (Foldable.toList tree)
    allAdded = Set.fromList is

data Op = Add Word8 | Remove Word8 deriving Show

instance Arbitrary Op where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ if x then Add y else Remove y

treeAfterOpsContainsAll :: [Op] -> Property
treeAfterOpsContainsAll ops = testSet === Set.fromList (Foldable.toList tree)
  where
    tree :: Tree Word8
    tree = foldr
      (\op t -> case op of
        Add x -> insert t x
        Remove x -> delete t x)
      Leaf
      ops
    testSet = foldr
      (\op s -> case op of
        Add x -> Set.insert x s
        Remove x -> Set.delete x s)
      Set.empty
      ops

main :: IO ()
main = do
  putStrLn ""
  putStr "Tree toList is sorted: "
  quickCheck (withMaxSuccess 10000 foldableToListIsSorted)
  putStrLn ""
  putStr "No left horizontals when adding to tree: "
  quickCheck (withMaxSuccess 10000 noLeftHorizontalsWhenAdding)
  putStr "No level right grandchildren when adding to tree: "
  quickCheck (withMaxSuccess 10000 noLevelRightGrandchildWhenAdding)
  putStr "No levels skipped when adding to tree: "
  quickCheck (withMaxSuccess 10000 noSkippedLevels)
  putStr "All inserted items are in tree: "
  quickCheck (withMaxSuccess 10000 treeAfterAddContainsAll)
  putStrLn ""
  quickCheck (withMaxSuccess 10000 treeAfterOpsContainsAll)
