
-- Assignment B) 3.3

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
-- kan også bruke 
-- double :: Int -> Int
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)


-- Assignment C

-- False :: Bool

-- 5 + 8 :: Num a => a 

-- (+) 2 :: Num a => a -> a 

-- (+2) :: Num a => a -> a 

-- (2+) :: Num a => a -> a 

-- (["foo", "bar"], 'a') :: ([String], char)
-- dette er også rett =  :: ([[Char]], char)

-- [(True, []), (False, [['a']])] :: [(Bool, [[Char]])]

-- \xy-> y!!x :: Int -> [a] -> a
-- når det er !! vet vi vi tar ut noe fra en liste, og det må være en Int vi tar ut. da ender vi opp med et tall

-- [take,drop,\xy-> (y!!x)] :: har ikke type

-- [take,drop,\xy-> [y!!x]] :: [Int -> [a] -> [a]]


-- Assignment D

foo1 :: a -> b -> (a,b)
foo1 x y = (x, y)

foo2 :: a -> b -> (a,b)
foo2 x = \y -> (x, y)

foo3 :: a -> b -> (a,b)
foo3 = \x y -> (x, y) 

foo4 :: a -> b -> (a,b)
foo4 = \x -> \y -> (x, y) 

foo5 :: b -> a -> (a,b)
foo5 = \x -> \y -> (y,x) 

foo6 :: a -> b -> (a,b)
foo6 = \y -> \x -> (y,x)

-- alle funksjonene bortsett fra foo5 er ekvivalente. dette kan man se da de har samme type, og de får samme output når man skriver inn
-- de samme parameterene

-- Assignment E

f1 :: a -> (a,a)
f1 x = (x,x)

f2 :: (a,b) -> a
f2 (x,y) = x

f3 :: (a,b) -> b
f3 (x,y) = y

f4 :: a -> b -> a 
f4 x y = x

f5 :: a -> b -> b
f5 x y = y

-- Assignment F

f :: Int -> Int -> Int
f x y = x+y

g :: (Int, Int) -> Int
g (x,y) = x+y