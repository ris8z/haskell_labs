import qualified Data.Map as Map
import Data.List (intercalate)

data Tree t =
    Empty
  | Root1 t (Tree t) (Tree t)
  | Root2 t t (Tree t) (Tree t) (Tree t)
  deriving (Eq, Ord, Show)


--add(X, T) returns the 2-3 Tree from adding X to the 2-3 Tree T.
add :: (Eq a, Ord a, Show a) => a -> Tree a -> Tree a
add a Empty = Root1 a Empty Empty --base case n1 
add a (Root1 value _ _)  --base case n2
  | a > value = Root2 value a Empty Empty Empty
  | otherwise = Root2 a value Empty Empty Empty
add a (Root2 x1 x2 left mid right) --rec call
  | a <= x1           = Root2 x1 x2 (add a left) mid right
  | x1 < a && a <= x2 = Root2 x1 x2 left (add a mid) right
  | otherwise         = Root2 x1 x2 left mid (add a right)


-- member(X, T) returns true if X is in the 2-3 Tree T.
member :: (Eq a, Ord a, Show a) => a -> Tree a -> Bool
member a Empty = False
member a (Root1 value _ _)
  | a == value = True
  | otherwise = False
member a (Root2 x1 x2 left mid right)
  | a == x1 = True
  | a == x2 = True
  | a < x1 = member a left
  | a < x2 = member a mid
  | otherwise = member a right


-- height(T) returns the height of T.
height :: (Eq a, Ord a, Show a) => Tree a -> Int
height Empty = 0
height (Root1 {}) = 1
height (Root2 _ _ left mid right) = 1 + max (height left) (max (height mid) (height right))



mytree = add 8 (add 7 (add 2 (add 4 (add 5 (add 3 Empty)))))

-- make a list of tuple (node, row)
-- the number of max row is going to be the height of tree
-- for each row you do a list comprension to get the node e get that in a string and then add \n
-- just print it with putStrLn
