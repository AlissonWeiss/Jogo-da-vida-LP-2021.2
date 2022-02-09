
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

-- TODO v

count matriz linhas colunas [] (a,b,c) = (a,b,c)
count matriz linhas colunas indexes (a,b,c)
    | i < 0 || j < 0 = count matriz linhas colunas body (a, b, c)
    | i >= linhas || j >= colunas = count matriz linhas colunas body (a, b, c)
    | matriz!!index == "m" = count matriz linhas colunas body (a, b + 1, c)
    | matriz!!index == "v" = count matriz linhas colunas body (a + 1, b, c)
    | matriz!!index == "z" = count matriz linhas colunas body (a, b, c + 1)
    where body = tail indexes
          i = head indexes !! 0
          j = head indexes !! 1
          index = i * colunas + j

-- neighbors :: Num a => a -> a -> [[a]]
neighbors i j = [[i-1,j+1], [i,j+1], [i+1,j+1],[i-1,j], [i+1,j],[i-1,j-1],[i, j-1], [i+1,j-1]]

-- adz :: t1 -> t2 -> a -> a -> t3
adz n linhas colunas i = count n linhas colunas (neighbors (div i colunas) (mod i colunas)) (0,0,0)

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

-- END FILE FUNCTIONS
eval :: Matrix -> Int -> Int -> Int -> [Char]
eval inputMatrix lines columns inter
   | index inter inputMatrix == "m" && alive == 3 = "v"
   | index inter inputMatrix == "v" && zombie >= 2 = "z"
   | index inter inputMatrix == "v" && alive < 2 && zombie < 2  = "m"
   | index inter inputMatrix == "v" && alive > 3 && zombie == 0 = "m"
   | index inter inputMatrix == "z" && alive == 0 = "m"
   | otherwise = index inter inputMatrix
   where alive  = alives (adz inputMatrix lines columns inter)
         zombie = zombies (adz inputMatrix lines columns inter)
         dead   = deads (adz inputMatrix lines columns inter)



-- GAME LOGIC
gamelogic :: Matrix -> Matrix -> Int -> Int -> Int -> Matrix
gamelogic inputMatrix outputMatrix linhas colunas inter =
   if inter >= linhas * colunas then outputMatrix
   else gamelogic inputMatrix partialMatrix linhas colunas (inter + 1)
   where partialMatrix = eval inputMatrix linhas colunas inter : outputMatrix
-- gamelogic ("m" : "v" : "v" : "v" : xs) = "v" : "v" : "v" : "v" : xs
-- gamelogic x = x

-- END GAME LOGIC

-- COMPARE
compareMatrix :: Matrix -> Matrix -> Bool
compareMatrix m1 m2 = m1 == m2
-- END COMPARE

-- GAME LOOP
gameloop :: Int -> Int -> Matrix -> Int -> Int -> Result
gameloop begin end inputMatrix linhas colunas
    | begin == end = Result inputMatrix begin
    | begin < end = do
        if inputMatrix == logic then
            Result logic begin
        else
            gameloop (begin + 1) end logic linhas colunas
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

-- TESTES

testExecution:: Int -> String -> IO ()
testExecution nTeste fileName = do

    putStrLn ("\nTeste " ++ intToStr nTeste ++ "\n") 
    (iteracao, linhas, colunas, matriz) <- readInputFile fileName -- Lê os dados do arquivo
    putStrLn ("Iterações: " ++ intToStr iteracao)
    putStrLn ("Linhas   : " ++ intToStr linhas)
    putStrLn ("Colunas  : " ++ intToStr colunas)
    let matriz_str = matrix2str matriz 1 colunas
    putStrLn ("\nMatriz inicial\n" ++ matriz_str)

    let gameResult = gameloop 0 iteracao matriz linhas colunas
    let iteracoes_gastas = getNumberOfIterations gameResult
    putStrLn ("Número de iterações gastas: " ++ intToStr iteracoes_gastas)

    putStr "\nResultado do jogo:\n"

    let final_matriz_t1 = matrix2str (getResult gameResult) 1 colunas
    putStr final_matriz_t1

    putStrLn ("\nFim Teste " ++ intToStr nTeste ++ "\n")
    putStrLn "-------------------------------------------"


-- END TESTES

-- GAME INSTANCE
main :: IO ()
main = do

    -- Inicio teste 1
    testExecution 1 "teste_1.txt"
    -- Fim teste 1
    
    -- Inicio teste 2
    testExecution 2 "teste_2.txt"
    -- Fim teste 2
    
    -- Inicio teste 3
    testExecution 3 "teste_3.txt"
    -- Fim teste 3
    
    -- Inicio teste 4
    testExecution 4 "teste_4.txt"
    -- Fim teste 4
    
    -- Inicio teste 5
    testExecution 5 "teste_5.txt"
    -- Fim teste 5
    
    -- Inicio teste 6
    testExecution 6 "teste_6.txt"
    -- Fim teste 6
    