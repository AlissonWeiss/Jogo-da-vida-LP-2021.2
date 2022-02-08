
-- TYPES
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
type Matrix = [String]
data Matrix2 = Matrix2 { content :: Matrix, contentData :: Matrix };
data Result = Result { m :: Matrix, it :: Int }
-- END TYPES

-- EXTRACTOR
getNumberOfIterations :: Result -> Int
getNumberOfIterations (Result _ it) = it

getResult :: Result -> Matrix
getResult (Result m _) = m
-- END EXTRACTOR

-- GAME LOGIC

gamelogic :: Matrix -> Matrix
gamelogic [] = []
gamelogic ("m" : "v" : "v" : "v" : xs) = "v" : "v" : "v" : "v" : xs
gamelogic x = x

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
matrix2str :: [String] -> Int -> Int -> String
matrix2str [] _ _ = ""
matrix2str (x : xs) index col = do
    if index == col then
        x ++ "|\n" ++ matrix2str xs 1 col
    else    
        x ++ "|" ++ matrix2str xs (index + 1) col
-- END PRINT MATRIX

-- GAME INSTANCE
-- m v z z
-- v v m m
-- z v m m
main :: IO ()
main = do    
    let matrix = ["m", "v", "z", "z", "v", "v", "m", "m", "z", "v", "m", "m"]

    let n = 5

    let original = matrix2str matrix 1 4
    putStr "\nJogo Inicial:\n"
    putStr original

    let finalMatrix = gameloop 1 n matrix []

    putStr "\nNumero de iteracoes: "
    print $ getNumberOfIterations finalMatrix

    putStr "\nResultado do jogo:\n"
    
    let final = matrix2str (getResult finalMatrix) 1 4
    putStr final
