import Uteis

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

-- utils:
alives :: (a, b, c) -> a
alives (alive, _, _) = alive
deads :: (a, b, c) -> b
deads (_, dead, _) = dead
zombies :: (a, b, c) -> c
zombies (_, _, zombie) = zombie

-- evaluate current cell
eval :: Matrix -> Int -> Int -> Int -> [Char]
eval inputMatrix lines columns inter
   | Uteis.index inter inputMatrix == "m" && alive == 3 = "v"
   | Uteis.index inter inputMatrix == "v" && zombie >= 2 = "z"
   | Uteis.index inter inputMatrix == "v" && alive < 2 && zombie < 2  = "m"
   | Uteis.index inter inputMatrix == "v" && alive > 3 && zombie == 0 = "m"
   | Uteis.index inter inputMatrix == "z" && alive == 0 = "m"
   | otherwise = Uteis.index inter inputMatrix
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

-- GAME LOOP
gameloop :: Int -> Int -> Matrix -> Int -> Int -> Result
gameloop begin end inputMatrix linhas colunas
    | begin == end = Result inputMatrix begin
    | begin < end = do
        if compareMatrix inputMatrix logic then
            Result logic begin
        else
            gameloop (begin + 1) end logic linhas colunas
    | otherwise = Result inputMatrix begin
    where
        logic = reverse(gamelogic inputMatrix [] linhas colunas 0)

-- END GAME LOOP

-- TESTES

testExecution:: Int -> String -> IO ()
testExecution nTeste fileName = do

    putStrLn ("\nTeste " ++ intToStr nTeste ++ "\n") 
    (iteracao, linhas, colunas, matriz) <- Uteis.readInputFile fileName -- Lê os dados do arquivo
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
    