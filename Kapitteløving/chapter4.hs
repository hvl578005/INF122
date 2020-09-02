
-- 4.8 Exercises

-- Assignment 1

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Assignment 2

-- a

third :: [a] -> a 
third xs = head (tail (tail xs ))

-- b

third2 :: [a] -> a
third2 xs = xs !! 2

-- c

third3 :: [a] -> a
third3 (_:_:x:_) = x

-- Assignment 3

-- a

safetail :: [a] -> [a]
safetail xs = if null xs then xs else tail xs

-- b

safetail2 :: [a] -> [a]
safetail2 xs    | null xs       = xs
                | otherwise     = tail xs

-- c

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs

-- Assignment 4

{-(||) :: Bool -> Bool -> Bool
False || False = False
True || True = True
_ || True = True
b || b = b -}

-- Assignment 5

sant :: Bool -> Bool -> Bool
sant b c = if b == True && c == True then True else False

-- Assignment 6

sant2 :: Bool -> Bool -> Bool
sant2 b c = if b == True then c else False

-- Assignment 7

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

multlambda :: Int -> Int -> Int -> Int
multlambda = \x -> (\y -> (\z -> x * y * z ))

-- Assignment 8



