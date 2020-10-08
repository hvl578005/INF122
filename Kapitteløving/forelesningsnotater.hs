import Data.Char

-- Forelesningsnotater 17. September

tokenize [] t s = []
tokenize (x:xs) t s     
    | elem x t  = [x] : tokenize xs t s
    | elem x s = tokenize xs t s 
    | otherwise = (takeWhile (notin (t++s)) (x:xs)) : tokenize (dropWhile (notin (t++s)) (x:xs)) t s 

notin xs = \x -> not(elem x xs)

tokens xs = tokenize xs "*+" " "

data Ast = A Ast Ast | M Ast Ast | V Int deriving (Show)

parseE :: [String] -> (Ast, [String])

parseE ("+":xs) = let   (e1, r1) = parseE xs ; 
                        (e2, r2) = parseE r1 in (A e1 e2, r2)

parseE ("*":xs) = let   (e1, r1) = parseE xs ; 
                        (e2, r2) = parseE r1 in (M e1 e2, r2)

parseE (x:xs) =  if (onlyDigits x) then (V (read x), xs)
                else error ("Syntaksfeil ved " ++ x)

onlyDigits xs = all isDigit xs

parse xs = fst(parseE (tokens xs))


