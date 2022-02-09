
module Uteis where

-- CONVERTER
strToInt :: String -> Int
strToInt = read

intToStr :: Int -> String
intToStr = show
-- END CONVERTER

-- FILE FUNCTIONS
readInputFile :: FilePath -> IO(Int, Int, Int, [String])
readInputFile file = fmap(readLine . words) (readFile file)

readLine :: [String] -> (Int, Int, Int, [String])
readLine (iteracao : linhas : colunas : matriz) =
    (i, l, c, m)
    where
        i = strToInt iteracao
        l = strToInt linhas
        c = strToInt colunas
        m = matriz

-- END FILE FUNCTIONS

index :: Int -> [String] -> [Char]
index i matrix = head(drop i matrix)

-- PRINT MATRIX
matrix2str :: [String] -> Int -> Int -> String
matrix2str [] _ _ = ""
matrix2str (x : xs) index col = do
    if index == col then
        x ++ "|\n" ++ matrix2str xs 1 col
    else
        x ++ "|" ++ matrix2str xs (index + 1) col
-- END PRINT MATRIX


-- COMPARE
compareMatrix :: [String] -> [String] -> Bool
compareMatrix m1 m2 = m1 == m2
-- END COMPARE