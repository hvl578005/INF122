import Data.Char

-- Ukesoppgaver 7

-- Assignnment A

-- 7.1

listcomp :: (t -> a) -> (t -> Bool) -> [t] -> [a]
listcomp f p xs = [f x | x <- xs, p x]

listcomp2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
listcomp2 f p xs = map f (filter p xs)

-- 7.4

dec2int :: [Int] -> Int
dec2int = foldl addDigit 0
    where addDigit num x = 10 * num + x

-- 7.5

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \n m -> f (n, m)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(n,m) -> f n m

-- 7.9

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = settSammen (map f (splitList1 xs)) (map g (splitList2 xs))

settSammen [][] = []
settSammen xs [] = xs
settSammen [] ys = ys
settSammen (x:xs) (y:ys) = x:y:settSammen xs ys

splitList1 [] = []
splitList1 xs = head xs : splitList1 (drop 2 xs)

splitList2 [] = []
splitList2 xs   | length xs == 1    = []
                | otherwise         = xs!!1 : splitList2 (drop 2 xs)

-- en annen lettere metode

altMap2 :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap2 func1 func2 ls = map func (zip ls [0..])
                    where func = \(elem, idx) = if idx `mod` 2 == 0
                                                    then func1 elem 
                                                    else func2 elem

-- en annen metode der en pairs the list

altMap3 :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap3 func1 func2 ls = concat $ map func (pairing ls) 
                            where 
                                func [x,y] = [func1 x, func2 y]
                                func [x] = [func1 x]

                                pairing (x:y:ls) = [x,y]:pairing ls
                                pairing (x:ls) = [x]:[]
                                pairing [] = []

-- 8.5

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a 
folde f g (Val v)   = f v
folde f g (Add w v) = g (folde f g w) (folde f g v)

-- 8.6

eval :: Expr -> Int
eval x = folde (+2) (+) x

-- en annen metode?????

eval1 :: Expr -> Int
folde fEval gEval e

fEval :: Int -> Int
fEval = \n -> n  -- finnes en funkson i haskell id som gjør nøyaktig det samme
gEval :: Int -> Int -> Int 
gEval = \n1 n2 -> n1 + n2

-- en til metode

eval2 :: Expr -> Int 
eval2 folde id (+) e

size :: Expr -> Int
size x = folde (const 1) (+) x

-- DEL B

infiks' :: Expr -> String 
infiks' (Val n) = show n 
infiks' (Add x y) = "(" ++ (infiks' x) ++ " + " ++ (infiks' y) ++ ")"

