module BinarySearchTree where

import qualified Data.List
-- You might want to use some externals. Better use qualified import
-- so there won't be any clash
-- for example instead of "sort" (from Data.List) use "Data.List.sort"

-- !! DO NOT CHANGE BSTree data type and type signatures of functions

-- | Binary search tree as described at wikipedia:
--  https://en.wikipedia.org/wiki/Binary_search_tree
data BSTree a = Node a (BSTree a) (BSTree a)
              | Nil
              deriving (Show, Read, Eq)

value :: BSTree a -> a
value Nil = error "Nil does not have a value"
value (Node x _ _) = x

left :: BSTree a -> BSTree a
left Nil = Nil
left (Node _ l _) = l

right :: BSTree a -> BSTree a
right Nil = Nil
right (Node _ _ r) = r

-- | Check whether is @BSTree@ valid (i.e., does not violate any rule)
-- TODO: implement validity check
inOrder :: BSTree a -> [a]
inOrder Nil = [] 
inOrder (Node x l r) = inOrder l ++ [x] ++ inOrder r

isAscending :: (Ord a) => [a] -> Bool
isAscending [] = True
isAscending [x] = True
isAscending (x:y:xs) = x <= y && isAscending (y:xs)

isValid :: Ord a => BSTree a -> Bool
isValid = isAscending . inOrder

-- | Check whether is @BSTree@ is leaf
-- TODO: implement leaf check
isLeaf :: Ord a => BSTree a -> Bool
isLeaf (Node a l r) = l == Nil && r == Nil
isLeaf Nil = False

-- | Count all nodes in @BSTree@
-- TODO: implement counting all nodes of the tree
size :: BSTree a -> Integer
size (Node _ l r) = 1 + size l + size r
size Nil = 0

-- | Height of @BSTree@ (height of @Nil@ is 0)
-- TODO: implement finding out height of the tree
height :: BSTree a -> Integer
height (Node _ l r) = max (1 + height l) (1 + height r)
height Nil = 0

-- | Minimal height in the @BSTree@ (height of @Nil@ is 0)
-- TODO: implement finding out minimal depth of the tree
minHeight :: BSTree a -> Integer
minHeight (Node _ l r) = min (1 + minHeight l) (1 + minHeight r)
minHeight Nil = 0

-- | Check if given element is in the @BSTree@
-- TODO: implement finding out if element is in the tree
contains :: Ord a => BSTree a -> a -> Bool
contains (Node x l r) a | a < x = contains l a
                        | a > x = contains r a
                        | True  = True 
contains Nil a = False

-- | Create new tree with given element inserted
-- TODO: implement insertion to the tree
insert :: Ord a => BSTree a -> a -> BSTree a
insert (Node x l r) a | x == a = Node x l r
                      | a < x  = Node x (insert l a) r
                      | True   = Node x l (insert r a)
insert Nil a = Node a Nil Nil



-- | Create new tree with given element deleted (min element in the right subtree strategy)
-- TODO: implement deletion from the tree
findMin (Node a l r) | l == Nil = a
                     | True     = findMin l

delete :: Ord a => BSTree a -> a -> BSTree a
delete (Node x l r) a | a < x = Node x (delete l a) r
                      | a > x = Node x l (delete r a)
                      | True  = delete' (Node x l r) 
delete Nil _ = Nil

-- no subtree -> just remove
delete' (Node _ Nil Nil) = Nil
--- one subtree -> replace by subtree
delete' (Node _ l Nil) = l
delete' (Node _ Nil r) = r
--- both subtrees -> 
delete' (Node _ l r) = (Node x l (delete r x)) where x = findMin r


-- | Convert @BSTree@ to list (will be in isAscending order if tree is valid)
-- TODO: implement conversion from tree to list
toList :: BSTree a -> [a]
toList = inOrder

-- | Build new @BSTree@ from arbitrary list with use of median (left if even)
-- TODO: implement conversion from list to tree, use median (hint: sort)
-- median :: Ord a => Fractional a => [a] -> Maybe a
-- median = medianFromSorted . Data.List.sort

fromList :: Ord a => [a] -> BSTree a
fromList = buildTree . Data.List.nub . Data.List.sort

split :: [a] -> ([a], [a])
split a = splitAt (((length a) + 1) `div` 2) a

extractMiddle :: ([a],[a]) -> ([a], [a], a)
extractMiddle (a, b) = (init a, b, last a)

buildTree [] = Nil
buildTree sorted = Node x (buildTree a1 ) (buildTree a2) where (a1, a2, x) = (extractMiddle . split) sorted