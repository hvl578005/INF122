
-- Week 37 Exercises

-- A

-- rekursiv metode

fjernrec :: String -> Char -> String
fjernrec [] _ = []
fjernrec (a:as) b   | a == b    = fjernrec as b
                    | a /= b    = a : fjernrec as b

-- list comprehension

fjernlist :: String -> Char -> String
fjernlist [] _ = []
fjernlist as b = [ a | a <- as, a /= b]

-- B

-- list comprehension (fikk ikke til rekursiv)

tegnposlist :: Char -> String -> [Int]
tegnposlist _ [] = []
tegnposlist n xs = [y | y <- [0..length xs-1], xs !! y == n]

-- C

intToList :: Int -> [Int]
intToList 0 = []
intToList x = intToList (x `div` 10) ++ [x `mod` 10]

-- D a)

settSammen :: [String] -> String
settSammen [] = ""
settSammen (x:xs) = foldl (\a b -> a ++ " " ++ b ) x xs

-- D b)

delStrengen :: String -> [String]
delStrengen [] = []
delStrengen (x:xs) 
        | x == ' '  = delStrengen (xs)
        | otherwise = vent (x:xs) : delStrengen (drop (length (vent (x:xs))) xs)

vent :: String -> String
vent [] = []
vent (x:xs)
        | x == ' '  = []
        | otherwise = [x] ++ vent xs

-- D c)

gdelStrengen :: String -> String -> [String]
gdelStrengen [] _ = []
gdelStrengen (x:xs) (ns)
        | elem x ns = gdelStrengen xs ns
        | otherwise = ventTil (x:xs) (ns) : gdelStrengen (drop (length (ventTil (x:xs) (ns))) xs ) (ns)

ventTil :: String -> String -> String
ventTil [] n = []
ventTil (x:xs) (ns)
        | elem x ns = []
        | otherwise = [x] ++ ventTil xs ns