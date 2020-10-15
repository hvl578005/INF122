
-- Chapter 7

-- Assignnment 1

listcomp :: (t -> a) -> (t -> Bool) -> [t] -> [a]
listcomp f p xs = [f x | x <- xs, p x]

listcomp2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
listcomp2 f p xs = map f (filter p xs)

-- Assignment 2

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p 

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                 = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = [] 





































