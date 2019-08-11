module Lib (Level, Tree(..), insert, has) where

type Level = Int

data Tree a =
    Leaf
  | Branch Level (Tree a) a (Tree a)
  deriving (Show)

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

has :: Ord a => Tree a -> a -> Bool
has t k = case t of
  Leaf -> False
  Branch _ less k2 more -> case compare k k2 of
    EQ -> True
    LT -> has less k
    GT -> has more k
