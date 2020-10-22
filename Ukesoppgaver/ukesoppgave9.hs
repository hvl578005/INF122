import Data.List

-- Week 9

-- Assignment A

brett :: Int -> IO () 
brett n = do 
    putStr $ unlines $ bretthjelper n 

bretthjelper n = if (n > 0 && n < 100) 
    then [i ++ intersperse ' ' (replicate i '*')  | i <- [n..1]]
        else return 0

--bretthelp2 n = 
     --   | otherwise = trekantHelper (n-1) ++ replicate n '*' ++ "\n"


{-

A. Programmer en aksjon brett :: Int -> IO (), 
som viser en N x N matrise, der 0<N<100 er inputtallet.
 Øverste rad i matrisen skal vise kolonnenummer, første 
 (eller siste) kolonne skal vise radnummer, mens hver posision 
 inne i matrisen skal ha et punktum, f.eks., brett 3 skal gi 


1 2  3                          1 2  3
1 .  .  .             eller       .  .  .  1
2 .  .  .                           .  .  .  2
3 .  .  .                           .  .  .  3

Tallene i øverste rad må ha minst en blank imellom, mens alle punktene 
i matrisen skal ha like avstander til sine naboer. Du må altså sørge for å
justere avstander, spesielt, når brettet har mer enn 9 rader/kolonner.-}