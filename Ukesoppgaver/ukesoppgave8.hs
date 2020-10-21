import Data.List

-- Week 8

-- Assignment 1

-- a) by recursion and patterns

alrec :: [Bool] -> Bool 
alrec [] = True
alrec (x:xs)    | x         = alrec xs 
                | not x     = False

-- alternative solution

al1 :: [Bool] -> Bool
al1 [] = True 
al1 (False:_) = False 
al1 (_:ls) = al1 ls 

-- b) by built-in function

albuilt :: [Bool] -> Bool 
albuilt [] = True -- ikke nødvendig da all er definert slik at hvis listen er tom så true
albuilt xs = all (==True) xs

-- c) by using foldl 

alfoldl :: [Bool] -> Bool 
alfoldl [] = True 
alfoldl xs = foldl (&&) True xs

-- d) by using foldr

alfoldr :: [Bool] -> Bool 
alfoldr [] = True 
alfoldr xs = foldr (&&) True xs

-- Assignment 2 

ala :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool 
ala _ b [] = b
ala a b xs = foldl a b xs

-- alternative solution

ala2 :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool 
ala2 func b ls   = foldl func (head ls) (tail ls)  

-- Assignment 3

stjerne :: Char -> Int -> [[Char]]
stjerne c n = [intersperse ' ' (replicate i c) | i <- [1 .. n]]

trekant :: Int -> IO () 
trekant s = putStr $ unlines $ stjerne '*' s 

-- alternative solution

trekant1 :: Int -> IO () 
trekant1 n = do 
    putStr $ trekantHelper n

trekantHelper :: Int -> String 
trekantHelper n 
                | n < 1 = [] 
                | otherwise = trekantHelper (n-1) ++ replicate n '*' ++ "\n"

-- Assignment 4

julestjerne :: Char -> Char -> Int -> [[Char]]
julestjerne c v n = [replicate k v ++ intersperse ' ' (replicate i c) ++ replicate k v | i <- [1 .. n], k <- [n-i]]

juletre :: Int -> IO () 
juletre s = putStr $ unlines $ julestjerne '*' ' ' s

-- alternative solution

juletre1 :: Int -> IO () 
juletre1 n = do 
    putStr $ juletreHjelper n 0

juletreHjelper :: Int -> Int -> String 
juletreHjelper n m 
                    | n < 1 = [] 
                    | otherwise = juletreHjelper (n-1) (m+1) ++ replicate m ' ' ++ concat (replicate n "* ") ++ "\n"