
-- Assignment A

-- 4.5

sant :: Bool -> Bool -> Bool
sant b c = if b == True && c == True then True else False

-- 4.7

multlambda :: Int -> Int -> Int -> Int
multlambda = \x -> (\y -> (\z -> x * y * z ))

-- 4.8

luhnDouble :: Int -> Int
luhnDouble a =  if a + a > 9 then ( a + a - 9)
                else a + a 

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d =  if ((luhnDouble a) + b + (luhnDouble c) + d ) `mod` 10 == 0 then True else False

-- Assignment B

-- 5.6

factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (factors x)]

-- 5.7

listcom :: [(Int, Int)]
listcom = concat [[(x,y) | x <- [1,2], y <- [3,4]]]

-- 5.9

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct os es = sum [o*e | (o,e) <- zip os es]

-- Assignment C

rem1 :: Eq a => [a] -> a -> [a]
rem1 [] o = []
rem1 (l:ls) o   | o == l        = ls
                | otherwise     = l : rem1 ls o    

-- Four different ways of rem1

-- Conditional expresssion

rem2 :: Eq a => [a] -> a -> [a]
rem2 [] _ = []
rem2 (x:xs) el =    if x == el then xs else x : rem2 xs el

-- Guarded expression
                        
rem3 :: Eq a => [a] -> a -> [a]
rem3 xs el
            | xs == [] = []
            | head xs == el = tail xs
            | otherwise = head xs : rem3 (tail xs) el

-- List comprehension

rem4 :: Eq a => [a] -> a -> [a]
rem4 xs el = [ x | (x,n) <- zip xs [0..], x /= el || n > length (takeWhile (/= el) xs)]

-- Using where-keyword

rem5 :: Eq a => [a] -> a -> [a]
rem5 xs el = beforeEl ++ (if afterEl == [] then [] else (tail afterEl))
                where 
                    beforeEl = takeWhile (/= el) xs
                    afterEl = dropWhile (/= el) xs

-- Assignment D

diff :: Eq a => [a] -> [a] -> [a]
diff as [] = as
diff (a:as) (b:bs)  | [a' | a' <- (a:as), a'==b] == [] = diff (a:as) bs
                    | otherwise = diff (rem1 (a:as) b) bs

-- Løsning i gruppetime

diff1 :: Eq a => [a] -> [a] -> [a]
diff1 xs [] = xs
diff1 xs (e:els) = diff (rem1 xs e) els 

-- Nyttige fuksjoner : foldl and foldr

diff2 :: Eq a => [a] -> [a] -> [a]
diff2 xs els = foldl rem1 xs els

diff3 :: Eq a => [a] -> [a] -> [a]
diff3 xs els = foldr (\x y -> rem1 y x) xs els