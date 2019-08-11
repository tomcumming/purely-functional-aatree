import Test.QuickCheck
import Data.Word (Word8)

import Lib

validTree :: Tree a -> Bool
validTree t = levelOrdered t && noLeftHorizontals t && noLevelRightGrandchild t

levelOrdered :: Tree a -> Bool
levelOrdered t = case t of
  Leaf -> True
  Branch l _ _ _ -> go l t
  where
    go :: Level -> Tree a -> Bool
    go l t = case t of
      Leaf -> True
      Branch l2 _ _ _ | l2 > l -> False
      Branch l2 less _ more -> go l2 less && go l2 more

noLevelRightGrandchild :: Tree a -> Bool
noLevelRightGrandchild t = case t of
  Branch l1 _ _ (Branch l2 _ _ (Branch l3 _ _ _)) | l1 == l3 -> False
  Branch _ less _ more -> noLevelRightGrandchild less && noLevelRightGrandchild more
  Leaf -> True

noLeftHorizontals :: Tree a -> Bool
noLeftHorizontals t = case t of
  Branch l1 (Branch l2 _ _ _) _ _ | l1 == l2 -> False
  Branch _ less _ more -> noLeftHorizontals less && noLeftHorizontals more
  Leaf -> True

addingToTreesRemainsValid :: [Word8] -> Bool
addingToTreesRemainsValid adds = go Leaf adds
  where
    go :: Tree Word8 -> [Word8] -> Bool
    go t as = case as of
      [] -> True
      (a:as) -> validTree t2 && go t2 as
        where t2 = insert t a

main :: IO ()
main = quickCheck addingToTreesRemainsValid
