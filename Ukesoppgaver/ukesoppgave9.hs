import Data.Char
import System.IO

-- Week 9

-- Assignment A & B

n x y = do
    putStr("\ESC7") 
    putStr ("\ESC[" ++ show (x+1) ++ ";" ++ show ((y+4) + ((y-1) * 2)) ++ "H")
    putStr ("X")
    putStr("\ESC8") 

d x y =  do

    putStr("\ESC7") 
    putStr ("\ESC[" ++ show (x+1) ++ ";" ++ show ((y+4) + ((y-1) * 2)) ++ "H")
    putStr (".")
    putStr("\ESC8") 

q = do 
    putStr "\ESC[2J"  
    putStr "\ESC[0;0H"

brett :: Int -> IO () 
brett nR = if nR > 0 && nR < 100 then
        do 
    clr
    writeTops nR 
    writeRows 1 nR 
        else feil 

feil :: IO ()
feil = putStr "Feil input. Tall må være mellom 0 og 100."

clr :: IO ()
clr =  do 
    putStr "\ESC[2J"    

writeTops nR = writeAt (ve + 1, 0)                                                
                ( ( concat [ if i > 9 then show i ++ " " else " " ++ show i ++ " " | i <- [1..nR]]) ++ "\n")

ve :: Int 
ve = 3 

writeAt (x,y) xs = do   
    putStr("\ESC7")                        
    goto(x,y) 
    putStr xs
    putStr("\ESC7")     

goto :: (Int, Int) -> IO ()                                     
goto(x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


writeRows i n = if i == (n + 1) then return ()                    
                else do writeRow i n                    
                        writeRows (i+1) n               

writeRow i nR = do                                                 
    writeAt (if i > 9 then ve-2 else ve-1, 1+i) (show i)  
    putStrLn (concat (replicate nR "  ."))



-- FORELESNINGS NOTATER brett1

brett1 :: Int -> IO () 
brett1 n 
        | n <= 0 = error "Must be bigger than 0" -- check for valid n 
        | n >= 100 = error "Must be smaller than 100"
        | otherwise = do
            putStr "\ESC[2J"                                                    -- clearing page 
            goto 0 0                                                            -- moving cursor to top left 
            putStr $ spaces 2                                                   -- space for row numbers 
            mapM_ (putStr . (\nb -> spaces (3 - l nb) ++ (show nb)) [1..n]  -- printing col numbers
            putStrLn ""                                                         -- going to next line to start printing rows
            mapM_ (putStr . (brettRow n)) [1..n]                                -- printing the rows   
            printInstructions n
            xGame n       

-- funksjon for å printe ut brettet 
l :: Int -> Int 
l = length . show 

spaces :: Int -> String 
spaces n = replicate n ' '

brettRow :: Int -> Int -> String 
brettRow cols row = spaces (2 - l row)                      -- spaces before one-digit numbers
                    ++ (show row)                           -- the row number
                    ++ concat ["  ." | _ <- [1..cols]]      -- the dots with two spaces in front
                    ++ "\n"

xGame :: Int -> IO ()                           -- game loop
xGame n = do                
    goto 0 (n+4)                                -- input line below instructions and error messages
    putStr "[\ESC[0J"                           -- clear previous input 
    command <- getLine                          -- get commando
    let cs = words                              -- split on spaces 
    case cs of                                  -- check commando
        ("q":[]) -> return ()                   -- quit 
        ("n":x:y:[]) -> do                      -- add X
            change x y n "X"
            xGame n 
        ("d":x:y:[]) -> do                      -- remove X
            change x y n "."
            xGame n      
        _ -> do                                 -- unknown command
            printError "unknown command" n  
            xGame n 

change :: String -> String -> Int -> String -> IO ()                    -- put X or . in grid 
change x y n str                                                        --  check that coordinates are numbers 
                | and (map (all isDigit) [x,y]) = 
                    let (nx, ny) = (read x, read y) in
                        if (nx > n || nx < 1 || ny > n || ny < 1)       -- check bounds
                            then printError "Out of bounds"
                            else printInGrid str nx ny n 
                | otherwise = printError "coordinates must be numbers"

printInGrid :: String -> Int -> Int -> Int -> IO ()         -- print X or . in grid 
printInGrid s x y n = do 
    printError "" n                                         -- remove error 
    goto ((x*3) + 2) (y+1)                                  -- move cursor
    putStr s 

printInstructions :: Int -> IO () 
printInstructions n = do 
    goto 0 (n+2)                                -- move cursor
    putStr "Enter a command: n <x> <y>.."       -- put instructions 



printError :: String -> Int -> IO () 
printError s n = do 
    goto 0 (n+3)            -- move cursos 
    putStr "\ESC[0J"        -- erase preious error message 
    putStr s 
