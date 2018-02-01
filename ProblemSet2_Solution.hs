{-# LANGUAGE GADTSyntax #-}
import Proof


-- Mohammad Ali
-- Homework 2

-- Exercise 1

data Tree a where
  Empty :: Tree a
  Branch  :: a -> Tree a -> Tree a -> Tree a
  deriving Show

-- Exercise 2
incrementTree :: Tree Integer -> Tree Integer
incrementTree  Empty = Empty
incrementTree  (Branch n l r) = Branch (n+1) (incrementTree l) (incrementTree r)

-- Exercise 3
treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Branch n l r) = 1 + treeSize l + treeSize r

-- Exercise 4
bstInsert :: Integer -> Tree Integer -> Tree Integer
bstInsert x Empty = Branch x Empty Empty
bstInsert x (Branch n l r)
                        |x == n = Branch n l r
                        |x > n  = Branch n l (bstInsert x r)
                        |x < n  = Branch n (bstInsert x l) r


-- Exercise 5
isBST :: Tree Integer -> Bool
isBST Empty = True
isBST (Branch n Empty Empty) = True
isBST (Branch n left@(Branch m1 l r) right@(Branch m2 l2 r2))
                        |n > m1 && n < m2 = isBST left && isBST right
                        |otherwise = False
isBST (Branch n Empty right@(Branch m l r))
                        |n < m = isBST right
                        |otherwise = False
isBST (Branch n left@(Branch m l r) Empty)
                        |n < m = isBST left
                        |otherwise = False
