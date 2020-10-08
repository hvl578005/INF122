import qualified Data.Map as Map

-- Chapter 6 Recursive Functions Exercises

-- 6.1

-- The function would respond with an error because we have base case fac 0 = 1. This is so the function stops and doesnt go on
-- for ever.

fac :: (Num p, Ord p) => p -> p
fac 0 = 1
fac n = if n < 0 then 1 else n * fac (n-1)

-- 6.2

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 6.3

multip :: Int -> Int -> Int
multip m 0 = 0
multip m n = m + (m * (n-1))

expon :: Int -> Int -> Int
expon m 0 = m 
expon m n = if n < 0 then 1 else m ^ n

-- 6.4

euclid :: Int -> Int -> Int
euclid m 0 = 0
euclid 0 n = 0
euclid m n      | m == n    = m
                | m > n     = euclid (m-n) n
                | otherwise = euclid m (n-m)

{- 6.5

length [1,2,3]
length 1 + [2,3]
length 1 + 1 + [3]
length 1 + 1 + 1 
length = 3

drop 3 [1,2,3,4,5]
drop 3 ([1:[2,3,4,5]])  = drop (3-1) [2,3,4,5,6]
drop 2 (2:[3,4,5])      = drop (2-1) [3,4,5]
drop 1 (3:[4,5])        = drop (1-0) [4,5]
drop 0 [4:5]
                        = [4,5]

init [1,2,3] 
init (1:[2,3])  = 1 : init [2,3]
init (2:[3])    = 1 2 : init [3]
init ([3])      = 1 2 
                = init [1,2]

-}

-- 6.6

-- a

and' :: [Bool] -> Bool
and' [] = False
and' (x:xs) | x && not (null xs) = and' xs
            | x && null xs = True
            | otherwise = False

-- b

concat' :: [[a]] -> [a]
concat' [] = []
concat' xss = foldr (++) [] xss

-- c

replicate' :: Int -> a -> [a]
replicate' 1 m = [m]
replicate' n m = m : replicate' (n-1) m

-- d

nth :: [a] -> Int -> a 
nth (x:xs) n    | n == 0        = x
                | otherwise     = nth xs (n-1)

-- e

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (b:bs)  | a == b        = True
                | otherwise     = elem' a bs

-- 6.7

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x > y     = y : merge (x:xs) ys
                    | x < y     = x : merge xs (y:ys)
                    | x == y    = x : merge xs (y:ys)

-- 6.8

halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs    | even (length xs)  = (splitAt((length xs) `div` 2) xs)
            | otherwise         = (splitAt(((length xs) `div` 2) + 1) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge(msort start) (msort end)
            where (start, end) = halve xs

--6.9

-- a

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- b

take' :: (Eq t, Num t) => t -> [a] -> [a]
take' _ [] = []
take' n (x:xs)  | n == 0        = []
                | otherwise     = x : take' (n-1) xs

-- c

last' :: Num a => [a] -> a
last' [] = 0
last' (x:xs)    | length xs == 0   = x
                | otherwise        = last' xs