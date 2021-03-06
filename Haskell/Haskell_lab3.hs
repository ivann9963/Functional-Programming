main :: IO()
main = do
    print 10
{-
Task 3: Define a predicate isTriangular :: [[Int]] -> Bool, which takes a square numeric matrix represented as a list of lists 
and checks whether it is upper triangular, i.e. whether all elements below its main diagonal are zero.
Task 4. Define a function primesInRange :: Integer -> Integer -> [Integer], which constructs a list of the prime numbers in the interval [a, b]
Task 5. Define a function prodSumDiv :: [Integer] -> Integer -> Integer, which finds the product of natural numbers in given list, the sum of the divisors of which is a multiple of k.
Task 6. Define a function isSorted :: [Int] -> Bool, which checks if a list is sorted in ascending order
Task 7. Define a function merge :: [Int] -> [Int] -> [Int], which receives two sorted lists and combines them so that the result is also sorted
Task 8. Define a function insert :: Int -> [Int] -> [Int], which adds an item to a sorted list so that the resulting list is also sorted
Task 9. Implement a function insertionSort :: [Int] -> [Int], which implements sorting by inserting into a list

-}
-- Task 3
allElementsEq :: [Int] -> Int -> Bool
allElementsEq xs a 
    | null xs = True
    | head xs == a = allElementsEq (tail xs) a
    | otherwise = False

isTriangular :: [[Int]] -> Bool
isTriangular xs = helper7 (tail xs) 1
    where
        helper7 lst n
            | null lst = True
            | allElementsEq (take n (head lst)) 0 = helper7 (tail lst) (n + 1)
            | otherwise = False
-- Task 4
primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = helper a b []
    where
        helper a b xs
            | a > b = xs
            | cntDivisors a == 2 = a : helper (a + 1) b xs
            | otherwise = helper (a + 1) b xs
                where
                    cntDivisors n = helper3 (quot n 2) 1
                        where
                             helper3 iterr counter
                                |iterr == 0 = counter
                                |rem n iterr == 0 = helper3 (iterr - 1) (counter + 1)
                                |otherwise = helper3 (iterr - 1) counter
                        
-- Task 5
prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv xs k = helper xs k 1
    where
        helper xs k res 
            | null xs = res
            | (sumDiv (head xs)) `mod` k == 0 = helper (tail xs) k (res * (head xs))
            | otherwise = helper (tail xs) k res
            where
                sumDiv n = helper4 n 0
                 where
                    helper4 iterr sum
                        |iterr == 0 = sum
                        |rem n iterr == 0 = helper4 (iterr - 1) (sum + iterr)
                        |otherwise = helper4 (iterr - 1) sum 

divisors :: Integer -> [Integer]
divisors x = helper x 1
    where
        helper x it
            | it == x + 1            = []
            | mod x it == 0         = it : helper x (it + 1)
            | otherwise             = helper x (it + 1)
        
-- Task 6
isSorted :: [Int] -> Bool
isSorted xs
    | null xs = True
    | isSortedH1 (head xs) xs = isSorted (tail xs)
    | otherwise = False
        where
            isSortedH1 el lst 
                | null lst = True
                | el <= (head lst) = isSortedH1 el (tail lst)
                | otherwise = False
-- Task 7
merge :: [Int] -> [Int] -> [Int]
merge xs ys = helper0 xs ys []
    where 
        helper0 lst1 lst2 res 
            | null lst1 = (res ++ lst2)
            | null lst2 = (res ++ lst1)
            | (head lst1) < (head lst2) = helper0 (tail lst1) lst2 (res ++ [head lst1])
            | (head lst2) < (head lst1) = helper0 lst1 (tail lst2) (res ++ [head lst2])
            | (head lst1) == (head lst2) = helper0 (tail lst1) (tail lst2) (res ++ [head lst1])
            | otherwise = res 
            
-- Task 8
insert :: Int -> [Int] -> [Int]
insert a xs = helper5 a xs []
    where
        helper5 el lst res
            | null lst = res ++ [el]
            | head lst < el = helper5 el (tail lst) (res ++ [head lst])
            | head lst > el = res ++ [el] ++ lst
            | head lst == el = res ++ [el] ++ (tail lst)

-- Task 9 
insertionSort :: [Int] -> [Int]
insertionSort xs = helper6 xs (head (tail xs)) [(head xs)]
    where
        helper6 lst el res
            | null lst = res
            | el <= (head lst) = helper6 (tail lst) (head (tail lst)) (insert el res)
            | el > (head lst) = helper6 (tail lst) (head (tail lst)) res

{-
Homework task. Define a function encode :: String -> String, compresses a string the following way:
encode "Haskell" -- -> "Haskell"
encode "aaabccdefff" -- -> "3abccde3f"
encode "aaaaaaaaaaaabbb" -- -> "12a3b"
encode "aabbb" -- -> "aa3b"

-}

encode :: String -> String
encode str = helper13 str 1
    where
        helper13 str counter
            | null (tail str) && counter == 1                       = [head str]
            | null (tail str) && counter == 2                       = [head str] ++ [head str]
            | null (tail str)                                       = show counter ++ [head str]
            | (head str) /= (head (tail str)) && counter == 1       = [head str] ++ helper13 (tail str) 1
            | (head str) /= (head (tail str)) && counter == 2       = [head str] ++ [head str] ++ helper13 (tail (tail str)) 1
            | (head str) /= (head (tail str)) && counter > 2        = show counter ++ [head str] ++ helper13 (tail str) 1
            | otherwise                                             = helper13 (tail str) (counter + 1)            

