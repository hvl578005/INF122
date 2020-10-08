
-- 5.7 Exercises

-- Assignment 1

sumsq :: Int
sumsq = sum [x^2 | x <- [1..100]]

-- Assignment 2

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x,y) | x <- [0..x], y <- [0..y]]

-- Assignment 3

square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y ]

-- Assignment 4

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n] ]

-- Assignment 5

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], z^2 == x^2+y^2]

-- Assignment 6

factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (factors x)]

-- Assignment 7

listcom :: [(Int, Int)]
listcom = concat [[(x,y) | x <- [1,2], y <- [3,4]]]














