
-- Kathrine Hermansen group 1 Oblig 1

import Data.Char

data Ast = Word String | Num Int | Mult Ast Ast | Plus Ast Ast | Minus Ast Ast deriving (Eq, Show)

tokenize :: String -> [String]
tokenize [] = []
tokenize ('(':xs) = "*": tokenize xs
tokenize (')':xs) = "+": tokenize xs
tokenize (',':xs) = "-": tokenize xs
tokenize (' ':xs) = tokenize xs
tokenize (x:xs) 
    | isDigit x = takeWhile isDigit (x:xs) : tokenize (dropWhile isDigit xs)
    | isAlpha x = takeWhile isAlpha (x:xs) : tokenize (dropWhile isAlpha xs)

parseExpr :: [String] -> (Ast, [String])
parseExpr (s) = let (a,z) = parseT(s) in 
    if null z then (a,z)
    else if head z == "-" then
        let (c, rest) = parseExpr (tail(z)) in (Minus a c, rest)
    else (a,z) 

parseT :: [String] -> (Ast, [String])
parseT (s) = let (a,z) = parseG(s) in 
    if null z then (a,z)
    else if head(z) == "+" then
        let (c,rest) = parseT (tail(z)) in (Plus a c, rest)
        else (a,z) 

parseG :: [String] -> (Ast, [String])
parseG (s) = let (a,z) = parseF(s) in 
    if null z then (a,z)
    else if head(z) == "*" then
        let (c,rest) = parseG (tail(z)) in (Mult a c, rest)
        else (a,z) 

parseF ("(":s) = let (a, ")" :b) = parseExpr(s) in (a,b)
parseF (x:xs) = if onlyDigits x then (Num(read x :: Int),xs)
                else (Word x, xs)

onlyDigits xs = takeWhile isDigit xs == xs
onlyAlpha xs = takeWhile isAlpha xs == xs

parse :: String -> Ast 
parse str = fst(parseExpr (tokenize str))

helper :: Ast -> String -> String
helper (Word w) xs  = xs ++ "Word " ++ show w ++ "\n"
helper (Num n) xs   = xs ++ "Num " ++ show n ++ "\n"
helper (Minus a b) xs = xs ++ "Minus " ++  "\n" ++ helper a (xs ++ "   ") ++ helper b (xs ++ "   ")
helper (Plus a b) xs  = xs ++ "Plus " ++ "\n" ++ helper a (xs ++ "   ") ++ helper b (xs ++ "   ")
helper (Mult a b) xs = xs ++ "Mult " ++ "\n" ++ helper a (xs ++ "   ") ++ helper b (xs ++ "   ")

viss :: Ast -> String 
viss ast = helper ast ""

vis:: Ast-> IO()
vis ast = putStr (viss ast)

eval :: Ast -> String
eval str = evalhelp str 

evalhelp :: Ast -> String
evalhelp (Word x)       = x
evalhelp (Num x)        = show x
evalhelp (Mult x y)     | onlyDigits (evalhelp x) && onlyDigits (evalhelp y) = show ((read (evalhelp x) :: Int) * (read (evalhelp y) :: Int))
                        | onlyDigits (evalhelp x) && onlyAlpha (evalhelp y) = concat (replicate (read (evalhelp x) :: Int) (evalhelp y))
                        | otherwise = error "feil"

evalhelp (Minus x y)    | onlyDigits (evalhelp x) && onlyDigits (evalhelp y) = show ((read (evalhelp x) :: Int) - (read (evalhelp y) :: Int))
                        | (onlyDigits (evalhelp x) && null (evalhelp x)) && onlyAlpha (evalhelp y) = diff (evalhelp x) (evalhelp y)
                        | onlyAlpha (evalhelp x) && onlyAlpha (evalhelp y) = diff (evalhelp x) (evalhelp y)
                        | otherwise = error "feil"

evalhelp (Plus x y)     | onlyDigits (evalhelp x) && onlyDigits (evalhelp y) = show ((read (evalhelp x) :: Int) + (read (evalhelp y) :: Int))
                        | otherwise = evalhelp x ++ evalhelp y


rem1 :: Eq a => [a] -> a -> [a]
rem1 [] o = []
rem1 (l:ls) o   | o == l        = ls
                | otherwise     = l : rem1 ls o   

diff :: Eq a => [a] -> [a] -> [a]
diff as [] = as
diff (a:as) (b:bs)  | null ([a' | a' <- a : as, a' == b]) = diff (a:as) bs
                    | otherwise = diff (rem1 (a:as) b) bs

