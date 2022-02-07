-- m m v z 
-- z v m m
-- z v m m


type Matrix = [[String]]
data Result = Result { m :: Matrix, it :: Int }

-- EXTRACTOR
getNumberOfIterations :: Result -> Int
getNumberOfIterations (Result _ it) = it

getResult :: Result -> Matrix
getResult (Result m _) = m
-- END -- EXTRACTOR

-- GAME LOGIC
compareMatrix :: Matrix -> Matrix -> Bool
compareMatrix m1 m2 = m1 == m2
-- END GAME LOGIC

-- GAME LOOP
gameloop :: Matrix -> Matrix -> Int -> Int -> Result
gameloop inputMatrix outputMatrix begin end
    | begin == end = Result outputMatrix begin
    | begin < end = do
        if compareMatrix outputMatrix inputMatrix then
            Result outputMatrix begin
        else
            gameloop outputMatrix inputMatrix (begin + 1) end
    | otherwise = Result outputMatrix begin

-- END GAME LOOP

-- PRINT MATRIX
matrix2strHelper :: String -> String
matrix2strHelper x = x

matrix2str :: [String] -> String
matrix2str [] = ""
matrix2str (x : xs) = matrix2strHelper x ++ "|" ++ matrix2str xs
-- END PRINT MATRIX

main :: IO ()
main = do    
    let matrix = [["m", "m", "v", "z"],["z", "v", "m", "m"],["z", "v", "m", "m"]]
    -- let outputMatrix = Matrix ['a', 'a']
    let n = 5

    let finalMatrix = gameloop matrix matrix 1 n

    putStr "Numero de iteracoes: "
    print $ getNumberOfIterations finalMatrix

    putStr "\nResultado do jogo:\n"
    let body = map matrix2str $ getResult finalMatrix
    mapM_ putStrLn body
