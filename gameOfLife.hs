{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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

readLine :: [String] -> (Int, Int, Int, Matrix)
readLine (iteracao : linhas : colunas : matriz) =
    (i, l, c, m)
    where
        i = strToInt iteracao
        l = strToInt linhas
        c = strToInt colunas
        m = matriz

-- END FILE FUNCTIONS

checksum :: (Num a1, Num a2, Num a3) => Matrix -> Int -> Int -> [[Int]] -> (a2, a1, a3) -> (a2, a1, a3)
checksum inputMatrix linhas colunas [] (a,b,c) = (a,b,c)
checksum inputMatrix linhas colunas neighbors (a,b,c)
    | i < 0 || j < 0 || i >= linhas || j >= colunas = checksum inputMatrix linhas colunas body (a, b, c)
    | inputMatrix!!index == "m" = checksum inputMatrix linhas colunas body (a, b + 1, c)
    | inputMatrix!!index == "v" = checksum inputMatrix linhas colunas body (a + 1, b, c)
    | inputMatrix!!index == "z" = checksum inputMatrix linhas colunas body (a, b, c + 1)
    where body = tail neighbors
          i = head (head neighbors)
          j = head neighbors !! 1
          index = i * colunas + j

neighbors :: Int -> Int -> [[Int]]
neighbors i j = [[i-1,j+1], [i,j+1], [i+1,j+1], [i-1,j], [i+1,j], [i-1,j-1], [i, j-1], [i+1,j-1]]

collection :: (Num a1, Num a2, Num a3) => Matrix -> Int -> Int -> Int -> (a2, a1, a3)
collection inputMatriz lines column index = do
    checksum inputMatriz lines column (neighbors (div index column) (mod index column)) (0,0,0)

-- TODO ^

-- utils:
index :: Int -> Matrix -> [Char]
index i matrix = head(drop i matrix)
alives :: (a, b, c) -> a
alives (alive, _, _) = alive
deads :: (a, b, c) -> b
deads (_, dead, _) = dead
zombies :: (a, b, c) -> c
zombies (_, _, zombie) = zombie

-- evaluate current cell
eval :: Matrix -> Int -> Int -> Int -> [Char]
eval inputMatrix lines columns inter
   | index inter inputMatrix == "m" && alive == 3 = "v"
   | index inter inputMatrix == "v" && zombie >= 2 = "z"
   | index inter inputMatrix == "v" && alive < 2 && zombie < 2  = "m"
   | index inter inputMatrix == "v" && alive > 3 && zombie == 0 = "m"
   | index inter inputMatrix == "z" && alive == 0 = "m"
   | otherwise = index inter inputMatrix
   where alive  = alives  (collection inputMatrix lines columns inter)
         zombie = zombies (collection inputMatrix lines columns inter)
         dead   = deads   (collection inputMatrix lines columns inter)

-- GAME LOGIC
gamelogic :: Matrix -> Matrix -> Int -> Int -> Int -> Matrix
gamelogic inputMatrix outputMatrix linhas colunas inter =
   if inter >= linhas * colunas then outputMatrix
   else gamelogic inputMatrix partialMatrix linhas colunas (inter + 1)
   where partialMatrix = eval inputMatrix linhas colunas inter : outputMatrix
-- END GAME LOGIC

-- COMPARE
compareMatrix :: Matrix -> Matrix -> Bool
compareMatrix m1 m2 = m1 == m2
-- END COMPARE

-- GAME LOOP
gameloop :: Int -> Int -> Matrix -> Matrix -> Int -> Int -> Result
gameloop begin end inputMatrix outputMatrix linhas colunas
    | begin == end = Result outputMatrix begin
    | begin < end = do
        if compareMatrix inputMatrix outputMatrix then
            Result outputMatrix begin
        else
            gameloop (begin + 1) end inputMatrix logic linhas colunas
    | otherwise = Result inputMatrix begin
    where
        logic = reverse(gamelogic inputMatrix [] linhas colunas 0)

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
    (iteracao_t1, linhas_t1, colunas_t1, matriz_t1) <- readInputFile "teste_6.txt" -- Lê os dados do arquivo
    putStrLn ("Iterações: " ++ intToStr iteracao_t1)
    putStrLn ("Linhas: " ++ intToStr linhas_t1)
    putStrLn ("Colunas: " ++ intToStr colunas_t1)
    let matriz_t1_str = matrix2str matriz_t1 1 colunas_t1
    putStrLn ("\nMatriz inicial\n" ++ matriz_t1_str)

    let gameResult_t1 = gameloop 0 iteracao_t1 matriz_t1 [] linhas_t1 colunas_t1
    let iteracoes_gastas_t1 = getNumberOfIterations gameResult_t1
    putStrLn ("Número de iterações gastas: " ++ intToStr iteracoes_gastas_t1)

    putStr "\nResultado do jogo:\n"

    let final_matriz_t1 = matrix2str (getResult gameResult_t1) 1 colunas_t1
    putStr final_matriz_t1

    putStrLn "\nFim Teste 1.\n"
    -- Fim teste 1
