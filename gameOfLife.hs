-- TYPES
type Matrix = [String]
data Result = Result { m :: Matrix, it :: Int }
-- END TYPES

-- EXTRACTOR
getNumberOfIterations :: Result -> Int
getNumberOfIterations (Result _ it) = it

getResult :: Result -> Matrix
getResult (Result m _) = m
-- END EXTRACTOR

-- CONVERTER

strToInt :: String -> Int
strToInt = read

intToStr :: Int -> String
intToStr = show

-- END CONVERTER

-- FILE FUNCTIONS

readInputFile :: FilePath -> IO(Int, Int, Int, Matrix)
readInputFile file = fmap(readLine . words) (readFile file)

--readLine :: [String] -> (Int, Int, Int, [String])
readLine :: [String] -> (Int, Int, Int, Matrix)
readLine (iteracao : linhas : colunas : matriz) =
    (i, l, c, m)
    where
        i = strToInt iteracao
        l = strToInt linhas
        c = strToInt colunas
        m = matriz

-- END FILE FUNCTIONS

-- GAME LOGIC

gamelogic :: Matrix -> Matrix
gamelogic [] = []
gamelogic ("m" : "v" : "v" : "v" : xs) = "v" : "v" : "v" : "v" : xs
gamelogic x = x

-- END GAME LOGIC

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
main :: IO ()
main = do

    -- Inicio teste 1
    putStrLn "\nTeste 1.\n"
    (iteracao_t1, linhas_t1, colunas_t1, matriz_t1) <- readInputFile "teste_1.txt" -- Lê os dados do arquivo 
    putStrLn ("Iterações: " ++ intToStr iteracao_t1)
    putStrLn ("Linhas: " ++ intToStr linhas_t1)
    putStrLn ("Colunas: " ++ intToStr colunas_t1)
    let matriz_t1_str = matrix2str matriz_t1 1 colunas_t1
    putStrLn ("\nMatriz inicial\n" ++ matriz_t1_str)

    let gameResult_t1 = gameloop 1 iteracao_t1 matriz_t1 []
    let iteracoes_gastas_t1 = getNumberOfIterations gameResult_t1
    putStrLn ("Número de iterações gastas: " ++ intToStr iteracoes_gastas_t1)

    putStr "\nResultado do jogo:\n"

    let final_matriz_t1 = matrix2str (getResult gameResult_t1) 1 colunas_t1
    putStr final_matriz_t1

    putStrLn "\nFim Teste 1.\n"
    -- Fim teste 1
