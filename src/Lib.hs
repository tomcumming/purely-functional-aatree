module Lib (Level, Tree(..), insert, delete, has) where

import Data.Maybe (fromMaybe)

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

insert :: Ord a => Tree a -> a -> Tree a
insert t k = case t of
  Leaf -> Branch 1 Leaf k Leaf
  Branch l less k2 more -> case compare k k2 of
    EQ -> t
    LT -> split $ skew $ Branch l (insert less k) k2 more
    GT -> split $ skew $ Branch l less k2 (insert more k)

delete :: Ord a => Tree a -> a -> Tree a
delete t k = case t of
  Leaf -> Leaf
  Branch l2 less2 k2 more2 -> fst $ go l2 less2 k2 more2
  where
    isLast less2 k2 more2 = case k < k2 of
      True | Leaf <- less2 -> True
      False | Leaf <- more2 -> True
      _ -> False
    go l2 less2 k2 more2 = case isLast less2 k2 more2 of
      True -> (Leaf, Just k2)
      False -> case k < k2 of
        True | Branch l3 less3 k3 more3 <- less2 ->
          let (less2, s) = go l3 less3 k3 more3
          in
            ( Branch l2 less2 (if k == k2 then fromMaybe k2 s else k2) more2
            , s
            )
        False | Branch l3 less3 k3 more3 <- more2 ->
          let (more2, s) = go l3 less3 k3 more3
          in
            ( Branch l2 less2 (if k == k2 then fromMaybe k2 s else k2) more2
            , s
            )
        _ -> error "Unreachable"

has :: Ord a => Tree a -> a -> Bool
has t k = case t of
  Leaf -> False
  Branch _ less k2 more -> case compare k k2 of
    EQ -> True
    LT -> has less k
    GT -> has more k
