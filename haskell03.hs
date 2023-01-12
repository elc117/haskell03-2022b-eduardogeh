applyExpr' :: [Int] -> [Int]
applyExpr' = map (\x -> (x * 3) + 2)

add10toall :: [Int] -> [Int]
add10toall lista = [x + 10 | x <- lista]

multN :: Int -> [Int] -> [Int]
multN n lista = [x * n | x <- lista]

multN' :: Int -> [Int] -> [Int]
multN' n x = map (\x -> x * n) x

applyExpr :: [Int] -> [Int]
applyExpr = map (\x -> (x * 3) + 2)

addSuffix :: String -> [String] -> [String]
addSuffix suffix lista = map (\lista -> lista ++ suffix) lista

sumOdds :: [Int] -> Int
sumOdds lista = sum [x | x <- lista, (x `mod` 2) == 1]

selectExpr :: [Int] -> [Int]
selectExpr lista = [x | x <- lista, (x >= 20 && x <= 50)]

calcExpr :: [Float] -> [Float]
calcExpr lista = [z | z <- [(x ^ 2) / 2 | x <- lista], z > 10]

trSpaces :: String -> String
trSpaces lista = [if x == ' ' then '-' else x | x <- lista]

selectSnd :: [(Int, Int)] -> [Int]
selectSnd tupla = [snd x | x <- tupla]

selectgt5 :: [Int] -> [Int]
selectgt5 y = [x | x <- y, x > 5]

countShorts :: [String] -> Int
countShorts lista = length [x | x <- lista, (length x) < 5]

dotProd :: [Int] -> [Int] -> Int
dotProd x y = sum [(fst z) * (snd z) | z <- zip (x) (y)]

main = do
  print ("Ta tudo funcionando")
