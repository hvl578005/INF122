import Data.Char

-- Assignment 1

data Ast = V Int | N String | P Ast Ast | M Ast Ast deriving (Show)

eval :: Ast -> Int
eval (V x)      = x
eval (P x y)    = (eval x) + (eval y) 
eval (M x y)    = (eval x) * (eval y)

-- Assignment 2

inn :: Ast -> String
inn (V x)      = show x
inn (P x y)    = "(" ++ (inn x) ++ " + " ++ (inn y) ++ ")"
inn (M x y)    = "(" ++ (inn x) ++ " * " ++  (inn y) ++ ")"

-- Assignment 3.1

tokenize :: String -> [String]
tokenize [] = []
tokenize ('*':xs) = "*": tokenize xs
tokenize ('+':xs) = "+": tokenize xs
tokenize (' ':xs) = tokenize xs
tokenize (x:xs) 
    | isDigit x = (takeWhile isDigit (x:xs)) : tokenize (dropWhile isDigit xs)
    | isLetter x = (takeWhile isLetter (x:xs)) : tokenize (dropWhile isLetter xs)

-- Assignment 3.2

parseE :: [String] -> (Ast, [String])

parseE ("+":xs) = let   (e1, r1) = parseE xs ; 
                        (e2, r2) = parseE r1 in (P e1 e2, r2)
parseE ("*":xs) = let   (e1, r1) = parseE xs ; 
                        (e2, r2) = parseE r1 in (M e1 e2, r2)
parseE (x:xs) =  if onlyDigits x then (V (read x :: Int), xs)
                else (N x, xs)

onlyDigits xs = takeWhile isDigit xs == xs

parse xs = fst(parseE (tokenize xs))

-- fikk ikke til 3.1 men var kanskje inne på noe???




