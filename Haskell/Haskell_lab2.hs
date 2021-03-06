{-
Exercise 1: Define the following functions:
incrementAllBy :: [Int] -> Int -> [Int], which takes a list and a number and adds the number to all the elements of the list
multiplyAllBy :: [Int] -> Int -> [Int], which takes a list and a number and mupltiplies the number to all the elements of the list
filterSmallerThan, which takes a list and a number and removes all the elements of the list which are smaller than the given number
Exercies 2: Define a function isAscending :: Integer -> Bool, which checks if the digits are in ascending order
The function takes a number, but works with a list of its digits.
Exercise 3: Let as  = [a1, a2, ... , ak] and bs = [b1, b2, ..., bk] are non-empty lists with equal number of integers.
Define a predicate isImage :: [Int] -> [Int] -> Bool, which returns True only when exists a number x: ai = x + bi, for every i = 1,...,k
Exercise 4: Define a function chunksOf :: Int -> [a] -> [[a]], which divides the input list into sublists with length the given number
Exercise 5: Define a function divisors :: Integer -> [Integer], which generates a list of all divisors of a given number
-}

main :: IO()
main = do
    print 1
    print [1, 2, 3]
    print v1
    print (null v1)
    print v2
    print (head v2)
    print (tail v2)
    print (1:[])
    print [v2]
    print (1:[2,3])
    print ([0] ++ [2, 3])
    print ([1,2,3] ++ [4])
    print v3

-- списък v1 от числа
v1 :: [Int]
v1 = []

v2 :: [[Int]]
v2 = [ [1, 2, 3], [4, 5], []]

sum1 :: [Int] -> Int
sum1 xs = 
    if null xs then 0 else head xs + sum1 (tail xs)

sum2 :: [Int] -> Int
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

sum3 :: [Double] -> Double
sum3 []     = 0
sum3 (x:xs) = x + sum3 xs

-- в типа Eq - тип за равенство (с == != и т.н) Ord - .. 
sum4 :: (Num t, Eq t, Ord t) => [t] -> t
sum4 []         = 0
sum4 (x:xs)   = x + sum4 xs

v3 :: (Int, Double, Int)
v3 = (1, 1.2, 3)

v4 :: [(Int, Double)]
v4 = [(1, 1.2), (2, 2.4)]

-- Exercise 1
incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy xs a = if null xs then [] else a + (head xs) : incrementAllBy (tail xs) a

multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs a = if null xs then [] else a * (head xs) : multiplyAllBy (tail xs) a

filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs s = if null xs then [] else if (head xs) < s 
   then (head xs) : filterSmallerThan (tail xs) s else filterSmallerThan (tail xs) s

-- Exercise 2
isAscending' :: [Int] -> Bool
isAscending' xs 
    | null xs || null (tail xs) = True
    | head xs >= head (tail xs) = False
    | otherwise = isAscending' (tail xs)

--преобразува число в списък от числа
numToList :: Int -> [Int]
numToList n  = if n < 10 then [n] else (n `mod` 10) : numToList (n `div` 10)

isAscending :: Int -> Bool
isAscending n = isAscending' (reverse (numToList n))

-- Exercise 3

isImage :: [Int] -> [Int] -> Bool
isImage as bs = if null (tail as)  then True
    else if ((head as) - (head bs)) == ( (head (tail as)) - (head (tail bs)) )
        then isImage (tail as) (tail bs) else False


-- Exercise 4

chunksOf :: Num a => Int -> [a] -> [[a]]
chunksOf n xs 
    | null xs = []
    | otherwise = (take n xs) : chunksOf n (drop n xs)

-- Exercise 5
divisors :: Integer -> [Integer]
divisors x = helper x 1
    where
        helper x it
            | it == x + 1            = []
            | mod x it == 0         = it : helper x (it + 1)
            | otherwise             = helper x (it + 1)
        