{-
INF122, H2020, oppgaver for uke 8
-}
module Uke8 where

a = [True, False, True]
b = [False, True, False]
c = [True, True, True]
d = []

-- Oppgave 1
al_a :: [Bool] -> Bool
al_a [] = True
al_a [x] = x
al_a (x:xs) | x == True = al_a xs
            | otherwise = False

al_b :: [Bool] -> Bool
al_b = and

al_c :: [Bool] -> Bool
al_c = foldl (&&) True

al_d :: [Bool] -> Bool
al_d = foldr (&&) True


-- Oppgave 2
ala :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool 
ala o b = foldl o b


-- Oppgave 3
trekant :: Int -> IO ()
trekant n = if n > 0
              then let createString x str | x == 1 = str
                                          | otherwise = createString (x-1) str ++ "\n" ++ (concat $ replicate x str)
                   in putStr (createString n "*" ++ "\n")
              else return ()


-- Oppgave 4
juletre :: Int -> IO ()
juletre n = if n > 0
              then let create x str | x == 1 = (concat $ replicate n " ") ++ str
                                    | otherwise = create (x-1) str ++ ("\n" ++ (concat $ replicate ((n-x)+1) " ")) ++ (concat $ replicate x str)
                   in putStr (create n "* " ++ "\n")
              else return ()