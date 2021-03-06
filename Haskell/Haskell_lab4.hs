import Data.List
{-
AUTOMATICALLY TRANSLATED, proceed with caution :D
Task 1. To define type Shape with 3 constructors:
Circle, which has 1 argument - radius
Rectangle, which has 2 arguments - width and height
Triangle that has 3 arguments - 3 sides
Let Shape be an instance of the Show class and define the show method for it.
Task 2. To define Shape:
a) function perimeter :: Shape -> Double, which finds the perimeter of the figure
b) area :: Shape -> Double function, which finds the face of the figure
c) predicate isRound :: Shape -> Bool, which checks if a figure is round
Task 3. Define a sumArea function that accepts a list of figures and returns the sum of the faces of the figures in the list.
To define another function biggestShape, which finds the figure with the largest face.
Task 4. To define type Point, which sets a point in the plane and a point in space.
Let it be an instance of the class Eq and for it be defined equality of points of the same dimension.
Task 5. To define a distance function for working with the type, which finds the distance between two (compatible) points.
If the points are of different dimensions (ie they have a different number of coordinates) the function returns an error message.
Task 6. Define a getClosestPoint function that accepts a list of points and another point p.
As a result, the function returns the point on the list that is closest to the point p.
Task 7. To define a recursive algebraic type of binary tree (BTree) and the following functions:
a) a size function that finds the number of elements of a binary tree
b) the height function, which finds the height of a binary tree
c) the sumTree function, which finds the sum of the elements of a binary tree
d) the sumLeaves function, which finds the sum of the elements on the leaves of a binary tree
e) an inorder function that traverses a binary tree in the Left-Root-Right row

Task - Define a function getLevel :: Int -> BTree -> [Int], which finds the elements on the k-th level of a binary tree
Task - Define a function average :: BTree -> [Int], which calculates the arithmetic mean of all elements of a tree
Task - Define a function mirrorTree :: BTree -> BTree, which converts a tree into its "mirror tree"
-}


-- Some new theory

data People = Person Name Age
type Name = String
type Age = Int

-- Constructors with different number of arguments
--data Shape = Circle Float | Rectangle Float Float



--isRound :: Shape -> Bool
--isRound (Circle _)  =True
--isRound (Rectangle _ _) = False

--area :: Shape -> Float
--area (Circle r) = pi * r * r
--area (Rectangle h w) = h * w

data Season = Spring | Summer | Autumn | Winter
            deriving (Eq, Ord, Enum, Show, Read)

-- Recursive type
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr



data Shape = Circle Double | 
                Rectangle Double Double |
                 Triangle Double Double Double
                deriving Show

--instance Show Shape where
--   show (Triangle a b c) = show (a, b, c)
--   show (Rectangle a b) = "Pravoagalnik sas strani " ++ show a ++ "i" ++ show b

-- print (Rectangle 3 4)

perimeter :: Shape -> Double
perimeter (Circle r) = pi * r * 2
perimeter (Rectangle a b) = 2 * ( a + b )
perimeter (Triangle a b c) = a + b + c

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b
area (Triangle a b c) = sqrt ( p * (p - a) * (p - b) * (p - c))
                    where p = (a + b + c) / 2

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False
isRound (Triangle _ _ _) = False

sumArea :: [Shape] -> Double
sumArea xs = sum (map area xs)

biggestShape :: [Shape] -> Double
biggestShape shapes = maximum (map area shapes)

data Point = Point3D Double Double Double |
                Point2D Double Double
                deriving (Eq, Show)

distance :: Point -> Point -> Double
distance (Point2D a b) (Point2D c d) = sqrt((a-c)**2 + (b-d)**2)
distance (Point3D a b c) (Point3D x y z) = sqrt((a-x)**2 + (b-y)**2 + (c-z)**2)
distance _ _ = error "Error types"


-- returns the smallest distance between a list of points and a given point
getClosestPoint2 :: [Point] -> Point -> Double
getClosestPoint2 points p = minimum [distance i p|i <- points]


getClosestPoint :: [Point] -> Point -> Point
getClosestPoint points p = foldl1 (\ p1 p2 -> if distance p p1 <= distance p p2 then p1 else p2) points

{- Task -  Haskell reverseOrdSuff :: Int -> Int
reverseOrdSuff 37563 → 36
reverseOrdSuff 32763 → 367
reverseOrdSuff 32567 → 7
reverseOrdSuff 32666 → 6
-}

rev :: Int -> Int
rev k = helper k 0
  where
    helper n res
      | n <= 0 = res
      | otherwise = helper (quot n 10) (res*10 + (rem n 10))

-- converts int to list
digs 0 = []
digs x = digs (div x 10) ++ [mod x 10]    

reverseOrdSuff :: Int -> Int
reverseOrdSuff k = helper2 k 0
  where 
    helper2 n result
      | n <= 0 = result
      | rem n 10 < rem (quot n 10) 10 = helper2 (quot n 10) (result * 10 + rem n 10) 
      | otherwise = result * 10 + rem n 10

{-
Task - Define a function on Haskell sumUnique :: [[Int]] -> Int which from list of lists of whole numbers finds the sum of
the numbers, which are unique for the list in which they belong to
Examples:
sumUnique [[1,2,3,2],[-4,-4],[5]] → 9 (= 1+3+5)
sumUnique [[2,2,2],[3,3,3],[4,4,4]] → 0
sumUnique [[1,2,3],[4,5,6],[7,8,9]] → 45
-}

uniqueSum :: [Int] -> Int
uniqueSum xs = hl (group (sort xs)) 0
  where
    hl lst res
      | null lst                    = res
      | length (head lst) == 1      = hl (tail lst) (res + (head (head lst)))
      | otherwise                   = hl (tail lst) res
      
sumUnique :: [[Int]] -> Int
sumUnique xs
  | null xs = 0
  | otherwise = uniqueSum (head xs) + sumUnique (tail xs)

type Vector1 = (Double, String, Int)

v1 :: (Double, String, Int)
v1 = (1.2, "abc", 5)

v2 :: Vector1
v2 = (1.2, "abc", 5)

type Product = (String, Int)
type StoreAvailability = [Product]

p1 :: Product
p1 = ("Nivea", 12)

f3 :: Product -> String
f3 (x, _ ) = x

f4 :: Product -> Int
f4 (_, x) = x


--("Nivea", 12), ("Avon", 8), ("Dove", 1), ("Garnier", 5), ("Loreal", 32)

allPrices :: StoreAvailability -> [Int]
allPrices products 
  | null products = []
  | otherwise = [f4 (head products)] ++ (allPrices (tail products))

averagePrice :: StoreAvailability -> Int
averagePrice products = sum (allPrices products) `div` length (allPrices products)


type Pointt = (Double, Double, Double)

pt3 :: Pointt
pt3 = (2.0, 3.0, 4.0)

pt4 :: Pointt
pt4 = (5.0, 2.5, 3.0)

x :: Pointt -> Double
x (x, _ , _) = x

y :: Pointt -> Double
y (_, y , _) = y

z :: Pointt -> Double
z (_, _ , z) = z

-- print (x pt1) -> 2.0
 
distance2 :: Pointt -> Pointt -> Double
distance2 pt1 pt2 = ((x pt1) - (x pt2)) * ((x pt1) - (x pt2)) + ((y pt1) - (y pt2)) * ((y pt1) - (y pt2)) + ((z pt1) - (z pt2)) * ((z pt1) - (z pt2))


data BTree = Empty | Node Int BTree BTree 
                deriving Show

t1 :: BTree                                             --      5
t1 = Node 5 (Node 2 Empty                              --      / \
                                  (Node 3 Empty Empty))  --    2   6
                     (Node 6 Empty Empty)                --    \
                                                       --       3

t2 :: BTree                                                             --    5
t2 = Node 5 (Node 3 Empty Empty)                      --   / \
                     (Node 4 (Node 5 Empty Empty)      --  3   4
                                   (Node 7 Empty Empty))     --      /  \
                                                                             --     5    7

size :: BTree -> Int
size Empty = 0
size (Node _ lt rt) = 1 + size lt + size rt

height :: BTree -> Int
height Empty = 0
height (Node _ lt rt) = 1 + max (height lt) (height rt)

sumTree :: BTree -> Int
sumTree Empty = 0
sumTree (Node a lt rt) = a + sumTree lt + sumTree rt

sumLeaves :: BTree -> Int
sumLeaves Empty = 0
sumLeaves (Node a Empty Empty) = a
sumLeaves (Node _ lt rt) = sumLeaves lt + sumLeaves rt

inorder :: BTree -> [Int]
inorder Empty = []
inorder (Node el lt rt) = inorder lt ++ [el] ++ inorder rt

getLevel :: Int -> BTree -> [Int]
getLevel _ Empty = []
getLevel 0 (Node v _ _) = [v]
getLevel k (Node _ lt rt) = getLevel (k-1) lt ++ getLevel (k-1) rt

average :: BTree -> Double
average Empty = error "Empty tree"
average bt = fromIntegral (sumTree bt) / fromIntegral (size bt)

mirrorTree :: BTree -> BTree
mirrorTree Empty = Empty
mirrorTree (Node a lt rt) = Node a (mirrorTree lt) (mirrorTree rt)


data Btree = NullT | Node (Float,Float) Btree Btree