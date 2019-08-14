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

noSkippedLevels :: [Op] -> Bool
noSkippedLevels ops = case tree of
  Leaf -> True
  Branch l _ _ _ -> go l tree
  where
    tree :: Tree Word8
    tree = foldr
      (\op t -> case op of
        Add x -> insert t x
        Remove x -> delete t x)
      Leaf
      ops
    go :: Level -> Tree a -> Bool
    go l t = case t of
      Leaf -> True
      Branch l2 _ _ _ | l2 > l || l < pred l2 -> False
      Branch l2 less _ more -> go l2 less && go l2 more

noLevelRightGrandchild :: [Op] -> Bool
noLevelRightGrandchild ops = check tree
  where
    tree :: Tree Word8
    tree = foldr
      (\op t -> case op of
        Add x -> insert t x
        Remove x -> delete t x)
      Leaf
      ops
    check :: Tree a -> Bool
    check t = case t of
      Branch l1 _ _ (Branch l2 _ _ (Branch l3 _ _ _)) | l1 == l3 -> False
      Branch _ less _ more -> check less && check more
      Leaf -> True

noLeftHorizontals :: [Op] -> Bool
noLeftHorizontals ops = check tree
  where
    tree :: Tree Word8
    tree = foldr
      (\op t -> case op of
        Add x -> insert t x
        Remove x -> delete t x)
      Leaf
      ops
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
  putStr "All inserted items are in tree: "
  quickCheck (withMaxSuccess 10000 treeAfterAddContainsAll)
  putStrLn ""
  putStrLn "Tree matches Set after operations: "
  quickCheck (withMaxSuccess 10000 treeAfterOpsContainsAll)
  putStr "No left horizontals when adding and removing: "
  quickCheck (withMaxSuccess 10000 noLeftHorizontals)

{- Failing
  [Remove 246,Remove 250,Add 189,Add 227,Add 243,Add 246,Add 232,Add 242,Add 250,Add 182,Add 88]
  [Remove 121,Add 107,Remove 134,Add 134,Add 147,Add 119,Add 182,Add 104,Add 121,Add 99,Add 188,Add 117,Add 203,Add 50,Add 86,Add 15,Add 144,Add 173,Add 71,Add 84,Add 10,Add 226,Add 207]
-}

  putStr "No level right grandchildren when adding and removing: "
  quickCheck (withMaxSuccess 10000 noLevelRightGrandchild)

{- Failing
  [Remove 63,Add 119,Add 144,Add 96,Add 126,Add 182,Add 252,Add 151,Add 177,Remove 19,Add 19,Add 63,Add 7,Add 114,Add 91]
-}

  putStr "No levels skipped when adding and removing: "
  quickCheck (withMaxSuccess 10000 noSkippedLevels)


