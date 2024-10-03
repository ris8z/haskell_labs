import Data.List (union)
{-]]
    Functional Programming Lab 4: More Recursion and Lists
    Aim
    The aim of this week's exercise is to continue to learn how to use recursion with recursive
    data structures such as lists.
--}


-- Append two lists
myAppend :: [a] -> [a] -> [a]
myAppend [] a = a
myAppend (x:xs) a = x : myAppend xs a


-- Extract the first element of a list, which must be non-empty.
myHead :: [a] -> a
myHead [] = error "Empty List"
myHead (x:_) = x


-- Extract the last element of a list, which must be finite and non-empty.
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (_:xs) = myLast(xs)


-- Extract the elements after the head of a list, which must be non-empty.
myTail :: [a] -> [a]
myTail [] = error "Empty List"
myTail (_:xs) = xs


-- Return all the elements of a list except the last one. The list must be non-empty.
myInit :: [a] -> [a]
myInit [] = error "Emtyp List"
myInit [x] = []
myInit (x:xs) = x : myInit xs


-- Returns the length of a finite list as an Int.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs


-- Returns the elements of xs in reverse order. xs must be finite.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]


-- Concatenate a list of lists.
myConcat :: [[a]] -> [a]
myConcat [] = []
--myConcat (x:xs) = x ++ myConcat xs
--maybe using the ++ is cheat here (:0)
--maybe we can reuse myAppend
myConcat (x:xs) = myAppend x (myConcat xs) --easy :)


-- Computes the sum of a finite list of numbers.
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum(xs)


-- Computes the product of a finite list of numbers.
myProduct :: Num a => [a] -> a
myProduct [] = 0
myProduct [x] = x
myProduct (x:xs) = x * myProduct xs


-- Returns the maximum value from a list, which must be non-empty, finite, and of an ordered type
myMaximum :: Ord a => [a] -> a
myMaximum [] = error "Empty List"
myMaximum [x] = x
myMaximum (x:y:xs) = if x > y then myMaximum(x:xs) else myMaximum(y:xs)


-- Returns the minimum value from a list, which must be non-empty, finite, and of an ordered type
myMinimum :: Ord a => [a] -> a
myMinimum [] = error "Empty List"
myMinimum [x] = x
myMinimum (x:y:xs) = if x < y then myMinimum(x:xs) else myMinimum(y:xs)


-- the list membership predicate.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = if a == x then True else myElem a xs


-- Removes the first occurrence of x from its list argument.
-- 1 [2,3,1,2,5] -> [2,3,2,5]
myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete a (x:xs) = if a == x then xs else x : myDelete a xs


-- Returns the list union of the two lists. Duplicates, and elements of the first list, are removed from the the
-- Second list, but if the first list contains duplicates, so will the result. . For example,
-- myUnion [1,3,5,1] [2,2,3,4] == [1,3,5,1,2,4]

-- a u b = a + (b - a inntersecato b)
-- let K = (b - a inter b), all the alement that b has but a does not
-- we need just to append to a K
myUnion :: Eq a => [a] -> [a] -> [a]
myUnion lst [] = lst 
myUnion lst (x:xs) = if myElem x lst
                        then myUnion lst xs
                        else myUnion (myAppend lst [x]) xs


-- Returns the list intersection of two lists. For example,
-- myIntersect [1,2,3,4] [2,4,6,8] == [2,4]
-- If the first list contains duplicates, so will the result.
-- myIntersect [1,2,2,3,4] [6,4,4,2] == [2,2,4]
-- we are just building a new list saying is this element part of the other one yes add it no skip it, while that list finsh
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] lst = []
myIntersect (x:xs) lst = if myElem x lst
                            then x : myIntersect xs lst
                            else myIntersect xs lst

-- myIntersect a b = [x | x <- a, myElem x b]    this is cheating :(
