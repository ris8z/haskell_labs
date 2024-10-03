--Haskell functions.
--should have a type declaration and a body.


diff :: Int -> Int -> Int 
diff x y = abs (x - y)

--the brackets here are important bc
--without them the opertaion would be abs(x) - y


{- 
 -The area of a triangle with sides a, b, c is given by the formula: 
 -
 - √(s(s - a)(s - b)(s - c))
 -
 - where s = (a + b + c) / 2
 -
 -The type of triangleArea should be Float -> Float -> Float -> Float
  

  The behaviour of triangleArea should be as follows:
  > triangleArea 3 4 5
  6.0
  > triangleArea 1 2 2.5
  0.949918
  > triangleArea 1 1 (sqrt 2)
  0.5
--}
tArea :: Float -> Float -> Float -> Float 
tArea a b c =
    let s = (a + b + c) / 2
    in sqrt(s * (s - a) * (s - b) * (s - c))



{- 
    Design a Haskell function isSum that takes three integer arguments and tests whether
    one of them is the sum of the other two. 
    The behaviour of isSum should be as follows:
    > isSum 1 2 3
    True
    > isSum 4 9 5
    True
    > isSum 12 5 7
    True
    > isSum 23 23 23
    False
    You should start by declaring the type of isSum in your script.
 - -}

isSum :: Int -> Int -> Int -> Bool
isSum a b c = (a == b + c) || (b == a + c) || (c == a + b)


{-
    Add to your function definition some checks to handle such invalid data and report an
    error if appropriate. You can use the built-in error function for this purpose; it is called
    with a string argument which is the error message. For example:
    error “Not a triangle!”
-}

--3 number can form a trinagle only if each number is smaller than the sum of the other two
--

--check if 3 can form a triangle
checkT :: Float -> Float-> Float -> Bool
checkT a b c = (a < b + c) && (b < a + c) && (c < a + b)


--calculate the are of a triangle with error checking
tcArea :: Float -> Float -> Float -> Float 
tcArea a b c =
    if checkT a b c 
        then tArea a b c 
        else error "Not a triangle!"




