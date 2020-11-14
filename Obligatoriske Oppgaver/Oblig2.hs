{-# LANGUAGE LambdaCase #-}

import Data.Char
import Data.List
import System.IO
import Control.Monad
import System.Directory

-- Kathrine Hermansen, Group 1

-- glider: 10 (s 0 0, b 2 2) 2 2 3 3 3 4 2 5

type Pos = (Int, Int)
type Board = [Pos]

main :: IO ()   
main = do 
            clr 
            goto (0, 0)
            putStr "Welcome to Conways game of life. Start game by writing \nc <n> - create and show a new empty board of size n \nr <name> - read the rules and board board from file"
            mainfunc
            
mainfunc :: IO ()
mainfunc = do 
            goto(0,5)
            line <- getLine
            if ((head (line)) == 'c') then do       -- create new board
                                            goto (0,4)
                                            putStr "\ESC[0J"
                                            let ls = words (tail line)
                                            if (all (all isDigit) [head ls])
                                                then do 
                                                        if (nrule (read (head ls)))
                                                            then c (read (head ls))
                                                            else errorR "n must be a digit bigger than 0 and smaller than 100"
                                                else errorR "n must be a digit bigger than 0 and smaller than 100"

            else if ((head (line)) == 'r') then do      -- reading from file
                                            goto (0,4)
                                            putStr "\ESC[0J"
                                            let b = doesFileExist (tail (tail line))    -- check if file exists
                                            b >>= \case
                                                True -> do
                                                            xs <- readFile (tail (tail line))
                                                            loadfile xs
                                                False ->  errorR "file does not exist"

            else if (line == "quit") then return () -- quit

            else errorR "unknown command"           -- unknown command

tokenize :: String -> [String]                      -- using tokenizer to parse file
tokenize [] = []
tokenize ('(':xs) = "(": tokenize xs
tokenize (')':xs) = ")": tokenize xs
tokenize (',':xs) = ",": tokenize xs
tokenize (' ':xs) = tokenize xs
tokenize ('^':xs) = "^": tokenize xs
tokenize ('?':xs) = "?": tokenize xs
tokenize (x:xs) 
    | isDigit x = takeWhile isDigit (x:xs) : tokenize (dropWhile isDigit xs)
    | isAlpha x = takeWhile isAlpha (x:xs) : tokenize (dropWhile isAlpha xs)
    | otherwise = tokenize xs

errorR str = do 
                goto (0,4)
                putStr "\ESC[0J"
                putStr str
                mainfunc

nrule :: Int -> Bool
nrule n = if n > 0 && n < 100 then True else False -- checking if n is betweeen 1 and 99 
                                            
c :: Int -> IO ()           -- creating board
c n = do
        clr
        writeTops n
        writeRows 1 n 
        printInstructions n 
        gameoflife [2,3] [3,3] n []

clr :: IO ()            -- emptying screen
clr =  do 
    putStr "\ESC[2J"  

ve :: Int 
ve = 3 

writeTops :: (Num a, Enum a, Ord a, Show a) => a -> IO ()
writeTops nR = writeAt (ve + 1, 0)                                                
                ( ( concat [ if i > 9 then show i ++ " " else " " ++ show i ++ " " | i <- [1..nR]]) ++ "\n")

writeAt :: Pos -> String -> IO ()
writeAt (x,y) xs = do   
    putStr("\ESC7")                        
    goto(x,y) 
    putStr xs
    putStr("\ESC7") 

goto :: Pos -> IO ()                                     
goto(x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeRows :: Int -> Int -> IO ()
writeRows i n = if i == (n + 1) then return ()                    
                else do writeRow i n                    
                        writeRows (i+1) n               

writeRow :: Int -> Int -> IO ()
writeRow i nR = do                                                 
    writeAt (if i > 9 then ve-2 else ve-1, 1+i) (show i)  
    putStrLn (concat (replicate nR "  ."))

printInstructions :: Int -> IO () 
printInstructions n = do 
    goto (0, (n+2))                          
    putStr "Welcome to Conways Game of Life! \n"

printError :: String -> Int -> IO () 
printError s n = do 
    goto (0, (n+3))            
    putStr "\ESC[0J"       
    putStr s 

gameoflife :: [Int] -> [Int] -> Int -> Board -> IO ()
gameoflife s b n cells = do 
    showcells cells
    goto (0, (n+5))                             --move cursor
    putStr "[\ESC[0J"                           -- erase previous error message
    command <- getLine
    let cs = words command
    case cs of                                  -- check commando
        ("quit":[]) -> return ()                -- quit 
        ("n":xs) -> do                          -- add new cells on board
                        printError "" n
                        if ((allnumbers xs) && (notOTB xs n))
                            then do 
                                    showcells (stringtotuples xs)
                                    let nCells = newcells n xs cells 
                                    gameoflife s b n nCells
                            else if (not (allnumbers xs))
                                then do
                                        printError "Coordinates must be all numbers, and the numbers must be tuples." n
                                        gameoflife s b n cells
                                else do
                                        printError "Out of bounds" n
                                        gameoflife s b n cells
        ("e":xs) -> do                          -- erase cells on board
                        printError "" n
                        if ((allnumbers xs) && (notOTB xs n))
                            then do 
                                    showcellse (stringtotuples xs)
                                    let delcell = dCells n (stringtotuples xs) cells
                                    gameoflife s b n delcell
                            else if (not (allnumbers xs))
                                then do
                                        printError "Coordinates must be all numbers, and the numbers must be tuples." n
                                        gameoflife s b n cells
                                else do
                                        printError "Out of bounds" n
                                        gameoflife s b n cells
        ("?":[]) -> do 
                        rules n s b
                        gameoflife s b n cells 

        ("b":x:y:[]) -> do 
                            printError "" n
                            if (and (map (all isDigit) [x,y]))
                                then gameoflife s [read x..read y] n cells
                                else do  
                                            printError "Must be all numbers and not more or less than two." n
                                            gameoflife s b n cells
                                        
        ("s":x:y:[]) -> do
                            printError "" n
                            if (and (map (all isDigit) [x,y]))
                                then gameoflife [read x..read y] b n cells 
                                else do 
                                            printError "Must be all numbers and not more or less than two." n
                                            gameoflife s b n cells

        ("w":[]) -> do 
                        printError "" n
                        w n cells 
                        gameoflife s b n cells 

        ("l":xs) -> do 
                        printError "" n 
                        if ((isnumbers xs) && (head (stringtolist xs) >= 1) && (length xs == 1))
                            then do
                                    let newx = head (stringtolist xs)
                                    life newx newx s b n cells 
                            else do 
                                    printError "X must be one integer & larger or equal to 1" n
                                    gameoflife s b n cells

        [] -> do 
                printError "" n
                let nextBoard = nextgen cells s b n 
                if (nextBoard == cells)
                    then do 
                            printError "Stable configuration is reached." n 
                            gameoflife s b n cells 
                    else do 
                            showcellse cells
                            showcells nextBoard
                            gameoflife s b n nextBoard

        _ -> do                                 
                printError "unknown command" n  
                gameoflife s b n cells    

-- livemode for the game   
life :: Int -> Int -> [Int] -> [Int] -> Int -> Board -> IO ()
life x originalx s b n cells | x >=1 = do 
                                let nextCells = nextgen cells s b n 
                                if nextCells == cells 
                                    then do 
                                            printError "Stable configuration is reached." n 
                                            gameoflife s b n cells
                                    else do
                                            showcellse cells 
                                            showcells nextCells
                                            let xx = x - 1     
                                            wait 5000000
                                            life xx originalx s b n nextCells
                             | otherwise = do 
                                    printError ((show originalx) ++ " generations is played.") n 
                                    gameoflife s b n cells

--waiting function to use in livemode
wait :: Int -> IO () 
wait n = sequence_ [return () | _ <- [1..n]]

-- map list of string from xs from gameloop to list of int used in s and b
stringtolist :: [String] -> [Int]
stringtolist [] = []
stringtolist xs = map read xs

-- check if all are numbers
isnumbers [] = True
isnumbers (x:xs)    | and (map (all isDigit) [x]) = isnumbers xs
                    | otherwise = False

-- adding new cells to board 
newcells :: Int -> [String] -> Board -> Board
newcells _ [] cells = cells 
newcells _ (_:[]) cells = cells 
newcells n (x:y:xs) cells 
                            | and (map (all isDigit) [x,y]) = if (elem (read x, read y)) cells then newcells n xs cells 
                                else 
                                    let (nx, ny) = (read x, read y) in
                                        if (nx <= n && nx > 0 && ny <= n && ny > 0)
                                            then newcells n xs (cells ++ [(nx,ny)])
                                            else newcells n xs cells 
                            | otherwise = newcells n xs cells

-- deleting living cells from board
dCells :: Int -> Board -> Board -> Board
dCells _ [] cells = cells 
dCells n (x:xs) cells   | elem x cells = dCells n xs (filter (/=x) cells )
                        | otherwise = dCells n xs cells

-- method to make list of string to tuples             
stringtotuples :: [String] -> [(Int, Int)]
stringtotuples [] = []
stringtotuples (x:y:xs) = [((read x :: Int), (read y ::Int))] ++ stringtotuples xs

-- checking if coordinates written in board is not out of bounds
notOTB [] n = True
notOTB (_:[]) n = True
notOTB (x:y:ls) n   | and (map (all isDigit) [x,y]) = 
                        let (nx, ny) = (read x, read y) in
                            if (nx > n || nx < 1 || ny > n || ny < 1)
                                then False 
                                else notOTB ls n
                    | otherwise = False

-- checking if coordinates written in board is all numbers and pairs
allnumbers [] = True
allnumbers (_:[]) = False 
allnumbers (x:y:ls) | and (map (all isDigit) [x,y]) = allnumbers ls 
                    | otherwise = False
                         
-- show living cells on board                           
showcells :: Board -> IO () 
showcells b = sequence_ [writeInGrid p "O" | p <- b]

-- delete living cells on board
showcellse :: Board -> IO () 
showcellse b = sequence_ [writeInGride p "." | p <- b]

--- used to write living cells on board
writeInGrid (x,y) xs = do   
    printError "" 50  -- remove error
    putStr("\ESC7") 
    putStr ("\ESC[" ++ show (x+1) ++ ";" ++ show ((y+4) + ((y-1) * 2)) ++ "H")      -- move cursor
    putStr "O"
    putStr("\ESC8")  

-- used to delete living cells on board
writeInGride (x,y) xs = do   
    printError "" 50  -- remove error
    putStr("\ESC7") 
    putStr ("\ESC[" ++ show (x+1) ++ ";" ++ show ((y+4) + ((y-1) * 2)) ++ "H")      -- move cursor
    putStr "."
    putStr("\ESC8") 

-- show the rules of s and b
rules :: Int -> [Int] -> [Int] -> IO ()
rules n ls lb = do 
    goto (0, (n+3))            -- move cursors 
    putStr "\ESC[0J"        -- erase previous error message 
    putStr $ "An empty cell with at least " ++ (show (head lb)) ++ " and a maximum of " ++ (show (last lb)) ++ " neighbours becomes alive. \n"
    putStr $ "A living cell with at least " ++ (show (head ls)) ++ " and a maximum of " ++ (show (last ls)) ++ " living neighbours survives."

-- printing out all living cells who are currently on board
w :: Int -> Board -> IO ()
w n cells = do 
                goto (0, (n+3))            -- move cursors 
                putStr "\ESC[0J"        -- erase previous error message 
                putStr $ "All living cells currently on board: " ++ show cells

-- determining if a cell is alive on board
isAlive :: Board -> Pos -> Bool 
isAlive b p = elem p b

-- determining if a cell is dead on board
isEmpty :: Board -> Pos -> Bool 
isEmpty b p = not (isAlive b p)

--showing all neighbours to a living cell. corner cells have 3 neighbours, wall cells have 5, others has 8
neighbs :: Int -> Pos -> [Pos]
neighbs str (x, y)
                    | (y == 1 && x == 1)                        = [(x, y + 1), (x + 1, y + 1), (x + 1, y)]
                    | (y == 1 && x == str)                      = [(x - 1, y), (x - 1, y + 1), (x, y + 1)]
                    | (y == str && x == 1)                      = [(x, y - 1), (x + 1, y - 1), (x + 1, y)]
                    | (y == str && x == str)                    = [(x - 1, y - 1), (x - 1, y), (x, y - 1)]
                    | (x == 1)                                  = [(x, y + 1), (x, y - 1), (x + 1, y + 1), (x + 1, y - 1), (x + 1, y)]
                    | (y == 1)                                  = [(x - 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1), (x + 1, y)]
                    | (y == str)                                = [(x - 1, y - 1), (x - 1, y), (x, y - 1), (x + 1, y - 1), (x + 1, y)]
                    | (x == str)                                = [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y + 1), (x, y - 1)]
                    | (x > 1 && x < str && y > 1 && y < str)    = [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y + 1), 
                                                                (x, y - 1), (x + 1, y + 1), (x + 1, y - 1), (x + 1, y)]
                    | otherwise = []

-- living neighbours on board
liveneighbours :: Int -> Board -> Pos -> Int
liveneighbours str b = length . filter (isAlive b) . (neighbs (str))

-- surviving cells
survivors :: Int ->Board -> [Int] -> [Pos]
survivors str b s = [p | p <- b, elem (liveneighbours str b p) s]

-- births from empty cells
births :: Board -> [Int] -> Int -> [Pos]
births brett b str  = [p | p <- rmdumps(concat(map (neighbs (str)) brett)),
                        fst  p<= str,
                        snd p <= str,
                        isEmpty brett p, 
                        elem (liveneighbours (str+1) brett p) b] 

-- method to remove duplicates
rmdumps :: Eq a => [a] -> [a]
rmdumps [] = [] 
rmdumps (x:xs) = x : rmdumps (filter (/= x) xs)

-- living cells for next generation
nextgen :: Board -> [Int] -> [Int] -> Int -> Board
nextgen brett s b str = survivors str brett s ++ births brett b str

-- metodene nedenfor er brukt til å parse innholdet i filen på. 
-- gjort litt tungvint vil jeg si hehe

loadfile xs = do 
                let rfile = tokenize xs
                loadfile1 rfile

loadfile1 (x:xs) | (and (map (all isDigit) [x])) && (nrule (read x)) = rParse1 (read x) [] [] xs     
                 | otherwise = do 
                                goto (0,4)
                                putStr "\ESC[0J"
                                putStr "Size of grid must be a number bigger than 0 and smaller than 100."
                                mainfunc

rParse1 n s b [] = errorR "A fault has occured. Try again: r n (s s1 s2, b b1 b1) t1 t2 .. tk"
rParse1 n s b (x:xs)    | x == "("  = rParse2 n s b xs 
                        | otherwise = errorR "Remember parantheses. Try again"

rParse2 n s b (x:xs)    | x == "s" = rParseS n s b xs 
                        | x == "b" = rParseB n s b xs
                        | otherwise = errorR "A fault has occured. Try again: r n (s s1 s2, b b1 b1) t1 t2 .. tk"

-- s way
rParseS n s b (x:y:xs)  | and (map (all isDigit) [x,y]) = cParseS n [read x..read y] b xs 
                        | otherwise = errorR "A fault has occured. Try again: r n (s s1 s2, b b1 b1) t1 t2 .. tk"

cParseS n s b (x:xs)    | x == "," = rParseS1 n s b xs
                        | otherwise = errorR "Remember comma between s and b."

rParseS1 n s b (x:xs)   | x == "b" = rParseS2 n s b xs 
                        | otherwise = errorR "A fault has occured. Try again: r n (s s1 s2, b b1 b1) t1 t2 .. tk"

rParseS2 n s b (x:y:xs) | and (map (all isDigit) [x,y]) = rParse3 n s [read x..read y] xs 
                        | otherwise = errorR "A fault has occured. Try again: r n (s s1 s2, b b1 b1) t1 t2 .. tk"

-- b way
rParseB n s b (x:y:xs)  | and (map (all isDigit) [x,y]) = cParseB n s [read x..read y] xs 
                        | otherwise = errorR "A fault has occured. Try again: r n (s s1 s2, b b1 b1) t1 t2 .. tk"

cParseB n s b (x:xs)    | x == "," = rParseB1 n s b xs 
                        | otherwise = errorR "Remember comma between s and b."

rParseB1 n s b (x:xs)   | x == "s" = rParseB2 n s b xs 
                        | otherwise = errorR "A fault has occured. Try again: r n (s s1 s2, b b1 b1) t1 t2 .. tk"

rParseB2 n s b (x:y:xs) | and (map (all isDigit) [x,y]) = rParse3 n [read x..read y] b xs 
                        | otherwise = errorR "A fault has occured. Try again: r n (s s1 s2, b b1 b1) t1 t2 .. tk"

-- func 

rParse3 n s b (x:xs)    | x == ")" = do
                                        if ((allnumbers xs) && (notOTB xs n))
                                            then parsing n s b (stringtotuples xs)
                                            else errorR "Coordinates must be tuples."
                        | otherwise = errorR "Remember to close with parantheses."

parsing n s b cells = do 
                        clr 
                        writeTops n
                        writeRows 1 n 
                        printInstructions n 
                        gameoflife s b n cells