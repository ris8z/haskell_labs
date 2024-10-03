{- 
    Write a function isPalindrome :: [a] -> Bool that takes a list and checks if
    it is a palindrome. For example:

    > isPalindrome [1,7,5,7,1]
    True
    > isPalindrome “madam”
    True
 - -}

{-
 -  Eq a => means that a must be an object that is equalable (comparable) like interface in java
 -  (head:tail) is patter matching that give u for exmplae [1,2,3] ->  head = 1  tail = [2,3]
 -  head xs (gives u the first element of a list)
 -  last xs (gives u the  last element of a list)
 -  tail xs (gives u the tail of a list)
 -  init xs (gives u the list without the last element)
 -
 - -}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
--isPalindrome (head:tail) = if head /= last tail then False else isPalindrome(init tail)
isPalindrome xs = if head xs /= last xs then False else isPalindrome(init (tail xs))



{-
 -  Write a function shortest :: [[a]] -> [a] that takes a list of lists and returns
    the shortest list in the list (and returns [] if the list of lists is empty). For example:
    > shortest [[1,2,3],[1,2],[1,2,3,4,5],[4,3,2,1]]
    [1,2]
 -
 - -}

shortest :: [[a]] -> [a]
shortest [x] = x
shortest (x:y:[]) = if length x < length y 
                        then x 
                        else y
shortest (x:y:xs) = if length x < length y
                        then shortest (x:xs)
                        else shortest (y:xs)


{-
    3. Adding Two Polynomials
    A polynomial in a single variable can be represented rather simply by a list of its
    coefficients. For example:
    [1,7,5,2] represents 2x3 + 5x2 + 7x + 1
    [42,2,1] represents x2 + 2x + 42
    [-3,0,0,0,1] represents x4 – 3
    [0,-2,0,4] represents 4x3 - 2x
    Notice how the list index for each element corresponds to the exponent of the term.

    Define a type synonym Poly for this representation. Two polynomials can be summed
    by adding the coefficients of corresponding terms. For example, the sum of 2x3 + x2 + 1
    and 3x4 + 4x2 – 7 is 3x4 + 2x3 + 5x2 – 6.
    Define a Haskell function sumPoly :: Poly -> Poly -> Poly that sums two
    polynomials that are represented as above. Take care with the case of polynomials with
    different degrees. For example:
    > sumPoly [1,7,5,2] [42,2,1]
    [43,9,6,2]
    > sumPoly [-3,0,0,0,1] [1,7,5,2]
    [-2,7,5,2,1]
    > sumPoly [0,-2,0,4] [1,7,5,2]
    [1,5,5,6]
 - -}

-- [1, 2, 3]
-- [1, 2]
--
--You can not have pattern matching and if in the same function body

sumPoly :: [Int] -> [Int] -> [Int]
sumPoly a [] = a
sumPoly [] b = b
sumPoly (x:xs) (y:ys) = x + y : sumPoly xs ys

{-
    4. Evaluating a Polynomial

    Define a Haskell function evalPoly :: Int -> [Int] -> Int which, given a
    polynomial and a value for x, will calculate the value of the polynomial for that value of
    x. For example:
    > evalPoly 3 [1,7,5,2]
    121
    > evalPoly (-2) [0,-2,0,4]
    -28
    > evalPoly 4 (sumPoly [0,-2,0,4] [1,7,5,2])
    485
    There are many ways to do this, but an identity that you may find helpful is the
    following:
    anxn + … + a2x2 + a1x + a0 = a0 + x(a1 + x(a2 + x(… an) …) )
    that's another remainder to study series in math
 - -}

-- tanks for the hint big david sincler

-- 2 [1]

evalPoly :: Int -> [Int] -> Int
evalPoly x [] = 0
evalPoly x (a:ax) = a + x * (evalPoly x ax)
