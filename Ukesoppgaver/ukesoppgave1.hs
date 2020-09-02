
-- assignment 1.4. 

qsortrev [] = []
qsortrev (n:ns) = qsortrev larger ++ [n] ++ qsortrev smaller
    where
        smaller = [a | a <- ns, a <= n]
        larger = [b | b <- ns, b > n]

-- assignment 1.5
-- it will remove numbers that are equal to n so it will shorten the list by removing duplicates from the list

qsort [] = []
qsort (n:ns) = qsort smaller ++ [n] ++ qsort larger
    where
        smaller = [a | a <- ns, a < n]
        larger = [b | b <- ns, b > n]

-- assignment 2.4

-- another possible solution is head (reverse a). it will first reverse the list then return the head. a is the list i defined in terminal.
-- head (reverse a)




-- assignment 2.5

-- 'a' is the list i defined in terminal
-- reverse (tail (reverse a))
-- reverse ( drop 1 ( reverse a))

--assignment C1

plu :: [Int] -> Int -> [Int]
plu [] n = []
plu [x] n = [x+n]
plu (x:xs) n = (x+n) : plu xs n

-- assignment C2

pali :: (Eq a) => [a] -> Bool
pali xs = xs == reverse xs
