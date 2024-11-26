import Data.List (intercalate)



data Tree t =
    Empty
  | Root1 t (Tree t) (Tree t)
  | Root2 t t (Tree t) (Tree t) (Tree t)
  deriving (Eq, Ord, Show)


--add(X, T) returns the 2-3 Tree from adding X to the 2-3 Tree T.
add :: (Ord a) => a -> Tree a -> Tree a
add a Empty = Root1 a Empty Empty 
add a (Root1 value _ _)
  | a > value = Root2 value a Empty Empty Empty
  | otherwise = Root2 a value Empty Empty Empty
add a (Root2 x1 x2 left mid right)
  | a <= x1           = Root2 x1 x2 (add a left) mid right
  | x1 < a && a <= x2 = Root2 x1 x2 left (add a mid) right
  | otherwise         = Root2 x1 x2 left mid (add a right)


-- member(X, T) returns true if X is in the 2-3 Tree T.
member :: (Eq a, Ord a) => a -> Tree a -> Bool
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
height :: Tree a -> Int
height Empty = 0
height (Root1 {}) = 1
height (Root2 _ _ left mid right) = 
  1 + max (height left) (max (height mid) (height right))


toList :: (Show a) => Tree a -> Int -> Int -> [(String, Int, Int)]
toList Empty lvl pos = [("nil", lvl, pos)]
toList (Root1 x left right) lvl pos = 
  let
    newLvl = lvl + 1 
    newPos = pos * 3
  in
    [(show x, lvl, pos)]
    ++ toList left newLvl newPos
    ++ toList right newLvl (newPos + 2)
toList (Root2 x1 x2 left mid right) lvl pos = 
  let
    newLvl = lvl + 1 
    newPos = pos * 3
  in
    [(show x1 ++ "," ++ show x2, lvl, pos)] 
    ++ toList left newLvl newPos 
    ++ toList mid newLvl (newPos + 1)
    ++ toList right newLvl (newPos + 2)


isSmaller :: (String, Int, Int) -> (String, Int, Int) -> Bool
isSmaller (_, lvl1, pos1) (_, lvl2, pos2)
  | lvl1 == lvl2 = pos1 < pos2
  | otherwise    = lvl1 < lvl2


sort :: (a -> a -> Bool) -> [a] -> [a]
sort f [] = []
sort f (x:xs) =
    let 
      small = sort f [a | a <- xs, f a x]
      big   = sort f [a | a <- xs, not(f a x)]
    in
      small ++ [x] ++ big


prettyPrintHelper :: (Show a) => Tree a -> String
prettyPrintHelper tree =
  let
    allNodes = sort isSmaller (toList tree 0 0) 
    maxLevel = maximum [lvl | (_, lvl, _) <- allNodes] 
    maxWidth = 3 ^ maxLevel * 3
    formatLevel :: Int -> String
    formatLevel lvl = 
      let
        nodesAtLevel = [(val, pos) | (val, l, pos) <- allNodes, l == lvl]
        line = replicate maxWidth ' '  
        placeNodes :: [(String,Int)] -> String -> String
        placeNodes [] line = line
        placeNodes ((val, pos):tail) line =
          let
            space = maxWidth `div` (3 ^ lvl) 
            idx = pos * space + space `div` 2
            newLine = take idx line ++ val ++ drop (idx + length val) line
          in
            placeNodes tail newLine
      in 
        placeNodes nodesAtLevel line
  in
    unlines $ intercalate [""] [[formatLevel lvl] | lvl <- [0..maxLevel]]


--prettyPrint(T) is always true and displays the 2-3 Tree T
prettyPrint :: (Show a) => Tree a -> IO ()
prettyPrint a = putStrLn (prettyPrintHelper a)


--mytree = Root2 3 10 Empty (Root1 6 Empty Empty) (Root2 15 20 Empty (Root1 18 Empty Empty) Empty)
mytree = add 18 (add 20 (add 15 (add 6 (add 10 (add 3 Empty)))))
