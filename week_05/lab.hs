
data BinTree t = Empty | Root t (BinTree t) (BinTree t)
                 deriving (Eq, Ord, Show)

{-
 -            5
 -           / \
 -          1   7
 -           \
 -           3
 -
 - -}
myTree = Root 5 (Root 1 (Empty) (Root 3 Empty Empty)) (Root 7 Empty Empty)
leaf x = Root x Empty Empty
myTreel = Root 5 (Root 1 (Empty) (leaf 3)) (leaf 7)


-- (a)
addNode :: Ord a => a -> BinTree a -> BinTree a
addNode a Empty = leaf a
addNode a (Root value left right)
    |   a > value = Root value (left) (addNode a right)
    |   otherwise = Root value (addNode a left) (right)


-- (b)
maketree :: Ord a => [a] -> BinTree a
maketree (x:[]) = addNode x Empty
maketree (x:xs) = addNode x (maketree xs)


-- (c)
inorder :: BinTree a -> [a]
inorder Empty = []
inorder (Root value left right) = (inorder left) ++ [value] ++ (inorder right)


-- (c)
mpsort :: Ord a => [a] -> [a]
mpsort [] = []
mpsort a = inorder (maketree a)


-- High Order Sort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let small = quicksort [a | a <- xs, a <= x]
        big   = quicksort [a | a <- xs, a > x]
    in small ++ [x] ++ big


hosort :: (a -> a -> Bool) -> [a] -> [a]
hosort f [] = []
hosort f (x:xs) =
    let small = hosort f [a | a <- xs, f a x]
        big   = hosort f [a | a <- xs, not(f a x)]
    in small ++ [x] ++ big
