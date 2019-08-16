module Lib (Level, Tree(..), insert, delete, has) where

import Data.Maybe (fromMaybe, catMaybes)

type Level = Int

data Tree a =
    Leaf
  | Branch Level (Tree a) a (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldr f x tree = case tree of
    Leaf -> x
    Branch _ less v more -> foldr f (f v (foldr f x less)) more

skew :: Tree a -> Tree a
skew t = case t of
  Branch l1 (Branch l2 less2 k2 more2) k1 more1 | l1 == l2 ->
    Branch l2 less2 k2 (Branch l1 more2 k1 more1)
  t -> t

split :: Tree a -> Tree a
split t = case t of
  Branch l1 less1 k1 (Branch l2 less2 k2 (Branch l3 less3 k3 more3)) | l1 == l3 ->
    Branch (succ l2) (Branch l1 less1 k1 less2) k2 (Branch l3 less3 k3 more3)
  t -> t

fixLevels :: Tree a -> Tree a
fixLevels t = case t of
  Leaf -> Leaf
  Branch l1 less1 k1 more1
    | tooHigh l1 less1 more1 -> Branch (pred l1) less1 k1 (demote l1 more1)
  t -> t
  where
    levelOf :: Tree a -> Maybe Level
    levelOf tree = case tree of
      Branch l _ _ _ -> Just l
      Leaf -> Nothing

    tooHigh :: Level -> Tree a -> Tree a -> Bool
    tooHigh l less more = pred l > (minimum $ catMaybes [Just l, levelOf less, levelOf more])

    demote l1 t = case t of
      Branch l2 less2 k2 more2 | l2 == l1 -> Branch (pred l2) less2 k2 more2
      t -> t

insert :: Ord a => Tree a -> a -> Tree a
insert t k = case t of
  Leaf -> Branch 1 Leaf k Leaf
  Branch l less k2 more -> case compare k k2 of
    EQ -> t
    LT -> split $ skew $ Branch l (insert less k) k2 more
    GT -> split $ skew $ Branch l less k2 (insert more k)

delete :: Ord a => Tree a -> a -> Tree a
delete t k = fst $ go t False
  where
    go t2 fnd = case t2 of
      Leaf -> (Leaf, Nothing)
      Branch l2 less2 k2 more2 -> case k < k2 of
        True ->
          let (less2', lst) = go less2 fnd
          in go2 fnd lst l2 less2' k2 more2
        False ->
          let fnd' = fnd || k == k2
          in let (more2', lst) = go more2 fnd'
          in go2 fnd' lst l2 less2 k2 more2'

    go2 fnd lst l2 less2 k2 more2 =
      let (tree, s) = go3 fnd lst l2 less2 k2 more2
      in (splitRight $ split $ skewRightRight $ skewRight $ skew $ fixLevels tree, s)

    go3 fnd lst l2 less2 k2 more2
      | True == fnd && lst == Nothing = case (less2, more2) of
        (Leaf, _) -> (more2, Just k2)
        (_, Leaf) -> (less2, Just k2)
        _ -> error "Unreachable"
      | Just lst <- lst, k == k2 = (Branch l2 less2 lst more2, Just lst)
      | otherwise = (Branch l2 less2 k2 more2, lst)

    skewRight :: Tree a -> Tree a
    skewRight t = case t of
      Leaf -> Leaf
      Branch l less k more -> Branch l less k (skew more)

    skewRightRight :: Tree a -> Tree a
    skewRightRight t = case t of
      Leaf -> Leaf
      Branch l less k more -> case more of
        Leaf -> Branch l less k more
        Branch l2 less2 k2 more2 -> Branch l less k (Branch l2 less2 k2 (skew more2))

    splitRight :: Tree a -> Tree a
    splitRight t = case t of
      Leaf -> Leaf
      Branch l less k more -> Branch l less k (split more)

has :: Ord a => Tree a -> a -> Bool
has t k = case t of
  Leaf -> False
  Branch _ less k2 more -> case compare k k2 of
    EQ -> True
    LT -> has less k
    GT -> has more k
