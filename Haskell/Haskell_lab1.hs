main :: IO()
main = do
    print 10

square :: Int -> Int
square n = n*n

average :: Float -> Float -> Float
average x y = (x+y)/2

{-
Exercise 1: Write a function mymin, which takes 2 arguments and returns the smaller one.
Exercise 2: Write a function isInside x a b, which checks if x is in the closed interval [a,b]
Exercise 3: Write a function myfunc, which finds the arithmetic mean of the square of 2 given numbers.
Exercise 4: Write a function myfib n, whick finds the n-th number of the Fibonacci sequence.
Write an iterative solution to the previous exercise.
(Note: The sequence is indexed from 0)
Exercise 5: Write a function mygcd a b, which finds the greatest common divisor of a and b.
Exercise 6: Write a function mymaxdivisor x, which finds the greatest divisor d of the whole number x > 1, for which d < x.
Exercise 7: Write a function oddNumbersSum a b, which finds the sum of the odds number in the closed interval a b
Exercise 8: Write a function isPrime, which checks if a given integers is prime.
Exercise 9: Write a function cntPalindormes, which counts the palindromes in the closed interval a b, where a
and b are whole non-negative numbers and a < b
-}

-- Exercise 1
mymin :: Int -> Int -> Int
mymin x y = if x < y then x else y

-- Exercise 2
isInside :: Double -> Double -> Double -> Bool
isInside x a b = a <= x && x <= b

-- Exercise 3
myfunc :: Double -> Double -> Double
myfunc a b = (a * a + b * b)/2

-- Exercise 3'
myfunc' :: Double -> Double -> Double
myfunc' a b = average (square a) (square b)
    where
            average a b = (a+b)/2
            square a = a*a

-- Exercise 4
myFib :: Int -> Int
myFib n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = myFib (n - 1) + myFib (n - 2)

-- Exercise 4'
myFib' :: Int -> Int
myFib' 0 = 1
myFib' 1 = 1
myFib' n = myFib' (n-1) + myFib' (n-2)

-- Exercise 4''
myFibIter :: Integer -> Integer
myFibIter n = helper 0 0 1
    where
        helper i next current
            | i > n     = next
            | otherwise = helper (i + 1) (current + next) next

-- Exercise 5
mygcd :: Int -> Int -> Int
mygcd a b = if b == 0 then a else mygcd b (mod a b)

-- -- Exercise 6
mymaxdivisor :: Int -> Int
mymaxdivisor x = help 1 1 
    where
        help res iter 
            | iter > (div x 2) = res
            | (rem x iter == 0 ) = help iter (iter + 1)
            | otherwise = help res (iter + 1)

-- Exercise 7
oddNumbersSum :: Int -> Int -> Int
oddNumbersSum a b = helper1 0 a
    where
        helper1 res iter
            | iter > b = res
            | odd iter = helper1 (iter + res) (iter + 1)
            | otherwise = helper1 res (iter + 1)

-- Exercise 8
isPrime :: Int -> Bool
isPrime x = if countDivisors x 1 0 == 2 then True else False
    where 
        countDivisors a it counter
            |it > a = counter
            |(rem a it == 0) = countDivisors a (it + 1) (counter + 1)
            |otherwise = countDivisors a (it + 1) counter


-- Exercise 9
isPalindrome :: String -> Bool
isPalindrome w
 | nChars <= 1           = nChars == 1
 | nChars == 2           = firstElem == lastElem
 | firstElem /= lastElem = False
 | firstElem == lastElem = isPalindrome (take (nChars - 2) (tail w))
 where firstElem = head w
       lastElem  = last w
       nChars    = length w

--- isPalindrome takes string so if we type in the 
-- terminal isPalindrome (show 123321) it returns True
--- show converts number to string

-- for the next function i make it strictly for integers
cntPalindromes :: Int -> Int -> Int
cntPalindromes a b = helper3 a 0
    where
        helper3 iter counter
            |iter > b = counter
            |isPalindrome (show iter) = helper3 (iter + 1) (counter + 1)
            |otherwise = helper3 (iter + 1) counter




{-
// first try doesnt work :D
countPalindromes :: Int -> String -> Int
countPalindromes a b = helper2 [a] 0
    where 
        helper2 iter counter
            |iter > b = counter
            |isPalindrome iter = helper2 (iter + 1) (counter + 1)
            |otherwise = helper2 (iter + 1) counter
            where
                isPalindrome w
                    | nChars <= 1           = nChars == 1
                    | nChars == 2           = firstElem == lastElem
                    | firstElem /= lastElem = False
                    | firstElem == lastElem = isPalindrome (take (nChars - 2) (tail w))
                        where 
                            firstElem = head w
                            lastElem  = last w
                            nChars    = length w

-}


--линейно итеративен процес намира броя на естествените делители на едно естествено число.

cntDivisors :: Int -> Int
cntDivisors n = helper4 (quot n 2) 1
    where
        helper4 iterr counter
            |iterr == 0 = counter
            |rem n iterr == 0 = helper4 (iterr - 1) (counter + 1)
            |otherwise = helper4 (iterr - 1) counter