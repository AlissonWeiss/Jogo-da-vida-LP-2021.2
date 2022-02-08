
-- TYPES
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
type Matrix = [[String]]
data Result = Result { m :: Matrix, it :: Int }
-- END TYPES

-- EXTRACTOR
getNumberOfIterations :: Result -> Int
getNumberOfIterations (Result _ it) = it

getResult :: Result -> Matrix
getResult (Result m _) = m
-- END EXTRACTOR

-- GAME LOGIC
asd :: [String] -> [String]
asd [] = []
asd ("v" : "m" : "v" : xs) = "v" : "v" : "v" : asd xs
asd (x : "v" : xs) = "m" : asd xs
asd (x : "z" : xs) = "q" : asd xs
asd (x : xs) = x : asd xs

test :: Matrix -> Matrix
test [] = []
test (x : xs) = [asd x] ++ xs



gamelogic :: Matrix -> Matrix
gamelogic matrix = test matrix
-- END GAME LOGIC

-- qwerty :: [String] -> String
-- qwerty ["v_"] = "v"
-- qwerty x = "W"


-- perfomActions :: Matrix -> Matrix
-- perfomActions [] = []
-- perfomActions (x : xs) = [qwerty x] : perfomActions xs
-- COMPARE
compareMatrix :: Matrix -> Matrix -> Bool
compareMatrix m1 m2 = m1 == m2
-- END COMPARE

-- GAME LOOP
gameloop :: Int -> Int -> Matrix -> Matrix -> Result
gameloop begin end inputMatrix outputMatrix
    | begin == end = Result outputMatrix begin
    | begin < end = do
        if compareMatrix inputMatrix outputMatrix then
            Result outputMatrix begin
        else
            gameloop (begin + 1) end inputMatrix $ gamelogic inputMatrix 
    | otherwise = Result outputMatrix begin

-- END GAME LOOP

-- PRINT MATRIX
matrix2str :: [String] -> String
matrix2str [] = ""
matrix2str (x : xs) = x ++ "|" ++ matrix2str xs
-- END PRINT MATRIX


-- GAME INSTANCE
-- m m v z 
-- z v m m
-- z v m m
main :: IO ()
main = do    
    let matrix = [["v", "m", "v", "z"],["z", "v", "m", "m"],["z", "v", "m", "m"]]

    let n = 5

    let original = map matrix2str matrix
    putStr "\nMatriz original:\n"
    mapM_ putStrLn original

    -- print diagonal
    -- let x = zipWith (!!) matrix [0..]
    -- mapM_ putStr x

    let finalMatrix = gameloop 1 n matrix [[]]

    putStr "Numero de iteracoes: "
    print $ getNumberOfIterations finalMatrix

    putStr "\nResultado do jogo:\n"
    
    let body = map matrix2str $ getResult finalMatrix
    mapM_ putStrLn body
